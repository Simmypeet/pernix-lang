use core::fmt;
use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use thiserror::Error;

use super::contains_forall_lifetime;
use crate::{
    arena::ID,
    semantic::{
        equality,
        instantiation::Instantiation,
        model::{Default, Model},
        normalizer::Normalizer,
        order,
        predicate::{ConstantType, LifetimeConstraint, Predicate, Tuple},
        session::{Cached, ExceedLimitError, Limit, Session},
        term::{
            constant::Constant, lifetime::Lifetime, r#type::Type,
            GenericArguments,
        },
        unification, Environment, Premise, TraitContext,
    },
    symbol::{
        self, ConstantParameterID, Generic, LifetimeParameterID,
        TraitImplementationKindID, TypeParameterID,
    },
    table::{self, DisplayObject, Index, State, Table},
};

/// Represents a predicate stating that there exists an implementation for the
/// given trait and generic arguments
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Trait<M: Model> {
    /// The trait to be implemented.
    pub id: ID<symbol::Trait>,

    /// Whether the implementation is const.
    pub is_const: bool,

    /// The generic arguments supplied to the trait.
    pub generic_arguments: GenericArguments<M>,
}

impl<M: Model> Trait<M> {
    /// Converts the [`Trait`] predicate from the [`Default`] model to the model
    /// `M`.
    pub fn from_default_model(predicate: Trait<Default>) -> Self {
        Self {
            id: predicate.id,
            is_const: predicate.is_const,
            generic_arguments: GenericArguments::from_default_model(
                predicate.generic_arguments,
            ),
        }
    }
}

impl<T: State, M: Model> table::Display<T> for Trait<M> {
    fn fmt(
        &self,
        table: &Table<T>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        if self.is_const {
            write!(f, "const ")?;
        }

        write!(
            f,
            "trait {}{}",
            table.get_qualified_name(self.id.into()).ok_or(fmt::Error)?,
            DisplayObject { display: &self.generic_arguments, table }
        )
    }
}

impl<M: Model> Trait<M> {
    /// Checks if the trait contains a `forall` lifetime.
    #[must_use]
    pub fn contains_forall_lifetime(&self) -> bool {
        self.generic_arguments.lifetimes.iter().any(Lifetime::is_forall)
            || self.generic_arguments.types.iter().any(contains_forall_lifetime)
            || self
                .generic_arguments
                .constants
                .iter()
                .any(contains_forall_lifetime)
    }
}

/// A query for checking [`Trait`] predicate satisfiability.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Query<'a, M: Model> {
    pub id: ID<symbol::Trait>,
    pub is_const: bool,
    pub generic_arguments: &'a GenericArguments<M>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct HigherRankedLifetimeUnifyingConfig;

impl<M: Model> unification::Config<M> for HigherRankedLifetimeUnifyingConfig {
    fn lifetime_unifiable(
        &mut self,
        from: &Lifetime<M>,
        _: &Lifetime<M>,
    ) -> Result<bool, ExceedLimitError> {
        Ok(from.is_forall())
    }

    fn type_unifiable(
        &mut self,
        _: &Type<M>,
        _: &Type<M>,
    ) -> Result<bool, ExceedLimitError> {
        Ok(false)
    }

    fn constant_unifiable(
        &mut self,
        _: &Constant<M>,
        _: &Constant<M>,
    ) -> Result<bool, ExceedLimitError> {
        Ok(false)
    }
}

/// A result of a trait satisfiability query.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Satisfiability<M: Model> {
    /// List of lifetime constraints that must be satisfied for the trait to be
    /// satisfied.
    pub lifetime_constraints: HashSet<LifetimeConstraint<M>>,
}

impl<M: Model> Trait<M> {
    /// Determines whether there exists an implementation for the given trait
    /// and generic arguments.
    ///
    /// # Errors
    ///
    /// See [`ExceedLimitError`] for more information.
    #[allow(clippy::too_many_lines)]
    pub fn satisfies(
        id: ID<symbol::Trait>,
        is_const: bool,
        generic_arguments: &GenericArguments<M>,
        environment: &Environment<M, impl State, impl Normalizer<M>>,
        limit: &mut Limit<
            impl Session<Lifetime<M>> + Session<Type<M>> + Session<Constant<M>>,
        >,
    ) -> Result<Option<Satisfiability<M>>, ExceedLimitError> {
        match limit.mark_as_in_progress(
            Query { id, is_const, generic_arguments },
            (),
        )? {
            Some(Cached::InProgress(())) => {
                // co-inductive
                limit.clear_query(Query { id, is_const, generic_arguments });
                return Ok(Some(Satisfiability {
                    lifetime_constraints: HashSet::new(),
                }));
            }
            Some(Cached::Done(satisfiability)) => {
                return Ok(Some(satisfiability));
            }
            None => {}
        }

        if is_in_active_trait_implementation(
            id,
            false,
            generic_arguments,
            environment,
            limit,
        )? {
            let pass = match environment.premise.trait_context {
                TraitContext::InTraitImplementation(
                    trait_implementation_id,
                ) => {
                    let implementation =
                        environment.table.get(trait_implementation_id).unwrap();

                    !is_const || implementation.is_const
                }
                TraitContext::InTrait(_) => !is_const,
                TraitContext::Normal => false,
            };

            // pass the check
            if pass {
                let result =
                    Satisfiability { lifetime_constraints: HashSet::new() };

                limit.mark_as_done(
                    Query { id, is_const, generic_arguments },
                    result.clone(),
                );

                return Ok(Some(result));
            }
        }

        // look for the premise that matches
        'outer: for trait_premise in environment
            .premise
            .predicates
            .iter()
            .filter_map(Predicate::as_trait)
        {
            if (is_const && !trait_premise.is_const) || (id != trait_premise.id)
            {
                continue;
            }

            let Some(forall_lifetime_unification) =
                trait_premise.generic_arguments.unify_as_mapping(
                    generic_arguments,
                    &mut HigherRankedLifetimeUnifyingConfig,
                    environment,
                    limit,
                )?
            else {
                continue;
            };

            for lifetimes in forall_lifetime_unification.lifetimes.values() {
                let mut lifetimes_iter = lifetimes.iter();

                // get the first lifetime to compare with the rest
                let Some(first_lifetime) = lifetimes_iter.next() else {
                    continue 'outer;
                };

                for lifetime in lifetimes_iter {
                    if !equality::equals(
                        first_lifetime,
                        lifetime,
                        environment,
                        limit,
                    )? {
                        continue 'outer;
                    }
                }
            }

            let Some(trait_sym) = environment.table.get(trait_premise.id)
            else {
                continue;
            };

            // constraints that involve with forall lifetime
            let predicate_with_forall_lifetimes = {
                let instantiation = Instantiation::from_generic_arguments(
                    trait_premise.generic_arguments.clone(),
                    trait_premise.id.into(),
                    &trait_sym.generic_declaration.parameters,
                )
                .unwrap();

                trait_sym
                    .generic_declaration
                    .predicates
                    .iter()
                    .filter_map(|x| {
                        let mut predicate =
                            Predicate::from_default_model(x.predicate.clone());

                        predicate.instantiate(&instantiation);

                        if predicate.contains_forall_lifetime() {
                            Some(predicate)
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>()
            };

            let forall_lifetime_instantiation = Instantiation {
                lifetimes: forall_lifetime_unification
                    .lifetimes
                    .iter()
                    .filter_map(|(forall, lifetimes)| {
                        lifetimes
                            .iter()
                            .next()
                            .cloned()
                            .map(|lifetime| (forall.clone(), lifetime))
                    })
                    .collect(),
                types: HashMap::default(),
                constants: HashMap::default(),
            };

            let mut lifetime_constraints = HashSet::new();
            for mut predicate in predicate_with_forall_lifetimes {
                predicate.instantiate(&forall_lifetime_instantiation);

                if !match predicate {
                    Predicate::TraitTypeEquality(equality) => equality::equals(
                        &Type::TraitMember(equality.lhs.clone()),
                        &equality.rhs,
                        environment,
                        limit,
                    )?,
                    Predicate::ConstantType(constant_type) => {
                        ConstantType::satisfies(
                            &constant_type.0,
                            environment,
                            limit,
                        )?
                    }
                    Predicate::LifetimeOutlives(outlives) => {
                        lifetime_constraints.insert(
                            LifetimeConstraint::LifetimeOutlives(outlives),
                        );
                        true
                    }
                    Predicate::TypeOutlives(outlives) => {
                        lifetime_constraints
                            .insert(LifetimeConstraint::TypeOutlives(outlives));
                        true
                    }
                    Predicate::TupleType(tuple) => {
                        Tuple::satisfies(&tuple.0, environment, limit)?
                    }
                    Predicate::TupleConstant(tuple) => {
                        Tuple::satisfies(&tuple.0, environment, limit)?
                    }
                    Predicate::Trait(tr) => {
                        if let Some(satisfiability) = Self::satisfies(
                            tr.id,
                            tr.is_const,
                            &tr.generic_arguments,
                            environment,
                            limit,
                        )? {
                            lifetime_constraints
                                .extend(satisfiability.lifetime_constraints);
                            true
                        } else {
                            false
                        }
                    }
                } {
                    continue 'outer;
                }
            }

            // if all the lifetimes are equal, then the trait is satisfied
            limit.mark_as_done(
                Query { id, is_const, generic_arguments },
                Satisfiability {
                    lifetime_constraints: lifetime_constraints.clone(),
                },
            );
            return Ok(Some(Satisfiability { lifetime_constraints }));
        }

        // manually search for the trait implementation
        match resolve_implementation(id, generic_arguments, environment, limit)
        {
            Ok(implementation) => {
                limit.mark_as_done(
                    Query { id, is_const, generic_arguments },
                    Satisfiability {
                        lifetime_constraints: implementation
                            .lifetime_constraints
                            .clone(),
                    },
                );
                Ok(Some(Satisfiability {
                    lifetime_constraints: implementation.lifetime_constraints,
                }))
            }

            Err(ResolveError::ExceedLimitError(exceed_limit_error)) => {
                Err(exceed_limit_error)
            }

            Err(_) => {
                limit.clear_query(Query { id, is_const, generic_arguments });
                Ok(None)
            }
        }
    }
}

impl<M: Model> Trait<M> {
    /// Applies an instantiation to the generic arguments.
    pub fn instantiate(&mut self, instantiation: &Instantiation<M>) {
        self.generic_arguments.instantiate(instantiation);
    }
}

/// A result of a trait implementation resolution query.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Implementation<M: Model> {
    /// The deduced substitution for the generic arguments of the trait
    /// implementation.
    pub deduced_substitution: Instantiation<M>,

    /// The ID of the resolved trait implementation.
    pub id: ID<symbol::TraitImplementation>,

    /// List of lifetime constraints that must be satisfied for the trait
    /// implementaton to be well-formed.
    pub lifetime_constraints: HashSet<LifetimeConstraint<M>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[allow(missing_docs)]
pub enum ResolveError {
    #[error("the trait id was invalid")]
    InvalidID,
    #[error("the passed generic arguments were not definite")]
    NonDefiniteGenericArguments,
    #[error("the trait was ambiguous")]
    AmbiguousTrait,
    #[error(transparent)]
    ExceedLimitError(#[from] ExceedLimitError),
    #[error("the trait implementation was not found")]
    NotFound,
    #[error(
        "the generic arguments contained a term that can be rewritten in \
         multiple ways and caused an ambiguity in trait resolution"
    )]
    AmbiguousTerm,
}

/// Resolves for the trait implementation for the given trait and generic
/// arguments.
///
/// # Errors
///
/// - [`ResolveError::InvalidID`]: If the `trait_id` is invalid.
/// - [`ResolveError::NonDefiniteGenericArguments`]: If the `generic_arguments`
///   are not definite.
/// - [`ResolveError::AmbiguousTrait`]: If the trait defined in the table is
///   ambiguous (multiple trait implementation matches).
/// - [`ResolveError::NotFound`]: If the trait implementation was not found.
/// - [`ResolveError::ExceedLimitError`]: If the session limit was exceeded; see
///   [`ExceedLimitError`] for more information.
#[allow(clippy::too_many_lines)]
pub fn resolve_implementation<M: Model>(
    trait_id: ID<symbol::Trait>,
    generic_arguments: &GenericArguments<M>,
    environment: &Environment<M, impl State, impl Normalizer<M>>,
    limit: &mut Limit<
        impl Session<Lifetime<M>> + Session<Type<M>> + Session<Constant<M>>,
    >,
) -> Result<Implementation<M>, ResolveError> {
    let trait_symbol =
        environment.table.get(trait_id).ok_or(ResolveError::InvalidID)?;

    if is_in_active_trait_implementation(
        trait_id,
        true,
        generic_arguments,
        environment,
        limit,
    )? {
        return Ok(Implementation {
            deduced_substitution: {
                let implementation = environment
                    .table
                    .get(
                        environment
                            .premise
                            .trait_context
                            .into_in_trait_implementation()
                            .unwrap(),
                    )
                    .unwrap();

                Instantiation {
                    lifetimes: implementation
                        .signature
                        .generic_declaration
                        .parameters
                        .lifetime_parameters_as_order()
                        .map(|x| {
                            let lifetime =
                                Lifetime::Parameter(LifetimeParameterID {
                                    parent: environment
                                        .premise
                                        .trait_context
                                        .into_in_trait_implementation()
                                        .unwrap()
                                        .into(),
                                    id: x.0,
                                });

                            (lifetime.clone(), lifetime)
                        })
                        .collect(),

                    types: implementation
                        .signature
                        .generic_declaration
                        .parameters
                        .type_parameters_as_order()
                        .map(|x| {
                            let ty = Type::Parameter(TypeParameterID {
                                parent: environment
                                    .premise
                                    .trait_context
                                    .into_in_trait_implementation()
                                    .unwrap()
                                    .into(),
                                id: x.0,
                            });

                            (ty.clone(), ty)
                        })
                        .collect(),

                    constants: implementation
                        .signature
                        .generic_declaration
                        .parameters
                        .constant_parameters_as_order()
                        .map(|x| {
                            let constant =
                                Constant::Parameter(ConstantParameterID {
                                    parent: environment
                                        .premise
                                        .trait_context
                                        .into_in_trait_implementation()
                                        .unwrap()
                                        .into(),
                                    id: x.0,
                                });

                            (constant.clone(), constant)
                        })
                        .collect(),
                }
            },
            id: environment
                .premise
                .trait_context
                .into_in_trait_implementation()
                .unwrap(),
            lifetime_constraints: HashSet::new(),
        });
    }

    if !generic_arguments.definite(environment, limit)? {
        return Err(ResolveError::NonDefiniteGenericArguments);
    }

    let mut candidate: Option<(
        TraitImplementationKindID,
        Instantiation<M>,
        HashSet<LifetimeConstraint<M>>,
    )> = None;

    for (key, arguments) in trait_symbol
        .implementations
        .iter()
        .map(|k| {
            (
                TraitImplementationKindID::Positive(*k),
                GenericArguments::from_default_model(
                    environment
                        .table
                        .get(*k)
                        .unwrap()
                        .signature
                        .arguments
                        .clone(),
                ),
            )
        })
        .chain(trait_symbol.negative_implementations.iter().map(|k| {
            (
                TraitImplementationKindID::Negative(*k),
                GenericArguments::from_default_model(
                    environment
                        .table
                        .get(*k)
                        .unwrap()
                        .signature
                        .arguments
                        .clone(),
                ),
            )
        }))
    {
        // builds the unification
        let Some(unification) =
            arguments.deduce(generic_arguments, environment, limit)?
        else {
            continue;
        };

        // check if satisfies all the predicate
        let generic_symbol = environment.table.get_generic(key.into()).unwrap();
        let Some(lifetime_constraints) = predicate_satisfies(
            &*generic_symbol,
            &unification,
            environment,
            limit,
        )?
        else {
            continue;
        };

        // assign the candidate
        match &mut candidate {
            Some((
                candidate_id,
                candidate_unification,
                candidate_lifetime_constraints,
            )) => {
                let combined_premise = {
                    let mut combined_premise = Premise::default();

                    let current_geenric_sym =
                        environment.table.get_generic(key.into()).unwrap();
                    let candidate_generic_sym = environment
                        .table
                        .get_generic((*candidate_id).into())
                        .unwrap();

                    combined_premise.append_from_predicates(
                        current_geenric_sym
                            .generic_declaration()
                            .predicates
                            .iter()
                            .map(|x| {
                                Predicate::from_default_model(
                                    x.predicate.clone(),
                                )
                            }),
                    );
                    combined_premise.append_from_predicates(
                        candidate_generic_sym
                            .generic_declaration()
                            .predicates
                            .iter()
                            .map(|x| {
                                Predicate::from_default_model(
                                    x.predicate.clone(),
                                )
                            }),
                    );

                    combined_premise
                };

                // check which one is more specific
                match GenericArguments::from_default_model(
                    environment
                        .table
                        .get_trait_implementation_signature(key)
                        .unwrap()
                        .arguments
                        .clone(),
                )
                .order(
                    &GenericArguments::from_default_model(
                        environment
                            .table
                            .get_trait_implementation_signature(*candidate_id)
                            .unwrap()
                            .arguments
                            .clone(),
                    ),
                    &Environment {
                        premise: &combined_premise,
                        table: environment.table,
                        normalizer: environment.normalizer,
                    },
                    limit,
                )? {
                    order::Order::Incompatible => {
                        return Err(ResolveError::AmbiguousTerm)
                    }
                    order::Order::MoreGeneral => {}
                    order::Order::MoreSpecific => {
                        *candidate_id = key;
                        *candidate_unification = unification;
                        *candidate_lifetime_constraints = lifetime_constraints;
                    }
                    order::Order::Ambiguous => {
                        return Err(ResolveError::AmbiguousTrait)
                    }
                }
            }

            candidate @ None => {
                *candidate = Some((key, unification, lifetime_constraints));
            }
        }
    }

    match candidate {
        Some((
            TraitImplementationKindID::Positive(id),
            deduced_substitution,
            lifetime_constraints,
        )) => Ok(Implementation {
            deduced_substitution,
            id,
            lifetime_constraints,
        }),

        None | Some((TraitImplementationKindID::Negative(_), _, _)) => {
            Err(ResolveError::NotFound)
        }
    }
}

fn predicate_satisfies<
    M: Model,
    R: Session<Lifetime<M>> + Session<Type<M>> + Session<Constant<M>>,
>(
    generic_symbol: &dyn Generic,
    substitution: &Instantiation<M>,
    environment: &Environment<M, impl State, impl Normalizer<M>>,
    session: &mut Limit<R>,
) -> Result<Option<HashSet<LifetimeConstraint<M>>>, ExceedLimitError> {
    let mut lifetime_constraints = HashSet::new();

    // check if satisfies all the predicate
    for mut predicate in generic_symbol
        .generic_declaration()
        .predicates
        .iter()
        .map(|x| Predicate::from_default_model(x.predicate.clone()))
    {
        predicate.instantiate(substitution);

        match predicate {
            Predicate::TraitTypeEquality(equality) => {
                if !equality::equals(
                    &Type::TraitMember(equality.lhs.clone()),
                    &equality.rhs,
                    environment,
                    session,
                )? {
                    return Ok(None);
                }
            }
            Predicate::ConstantType(constant_type) => {
                if !ConstantType::satisfies(
                    &constant_type.0,
                    environment,
                    session,
                )? {
                    return Ok(None);
                }
            }
            Predicate::LifetimeOutlives(outlives) => {
                lifetime_constraints
                    .insert(LifetimeConstraint::LifetimeOutlives(outlives));
            }
            Predicate::TypeOutlives(outlives) => {
                lifetime_constraints
                    .insert(LifetimeConstraint::TypeOutlives(outlives));
            }

            Predicate::TupleType(tuple_type) => {
                if !Tuple::satisfies(&tuple_type.0, environment, session)? {
                    return Ok(None);
                }
            }

            Predicate::TupleConstant(tuple_constant) => {
                if !Tuple::satisfies(&tuple_constant.0, environment, session)? {
                    return Ok(None);
                }
            }

            Predicate::Trait(tr) => {
                if let Some(satisfiability) = Trait::satisfies(
                    tr.id,
                    tr.is_const,
                    &tr.generic_arguments,
                    environment,
                    session,
                )? {
                    lifetime_constraints
                        .extend(satisfiability.lifetime_constraints);
                } else {
                    return Ok(None);
                }
            }
        }
    }

    Ok(Some(lifetime_constraints))
}

fn is_in_active_trait_implementation<M: Model>(
    trait_id: ID<symbol::Trait>,
    need_implementation: bool,
    generic_arguments: &GenericArguments<M>,
    environment: &Environment<M, impl State, impl Normalizer<M>>,
    limit: &mut Limit<
        impl Session<Lifetime<M>> + Session<Type<M>> + Session<Constant<M>>,
    >,
) -> Result<bool, ExceedLimitError> {
    match environment.premise.trait_context {
        TraitContext::InTraitImplementation(trait_implementation) => {
            let Some(implementation) =
                environment.table.get(trait_implementation)
            else {
                return Ok(false);
            };

            // check if the trait id is the same
            if implementation.signature.implemented_id != trait_id {
                return Ok(false);
            }

            // check if the generic arguments are the same
            GenericArguments::from_default_model(
                implementation.signature.arguments.clone(),
            )
            .equals(generic_arguments, environment, limit)
        }
        TraitContext::InTrait(env_trait_id) => {
            if need_implementation || env_trait_id != trait_id {
                return Ok(false);
            }

            let trait_symbol = environment.table.get(trait_id).unwrap();

            trait_symbol
                .generic_declaration()
                .parameters
                .create_identity_generic_arguments(env_trait_id.into())
                .equals(generic_arguments, environment, limit)
        }
        TraitContext::Normal => Ok(false),
    }
}

#[cfg(test)]
mod tests;
