use core::fmt;
use std::{collections::HashSet, sync::Arc};

use thiserror::Error;

use super::contains_forall_lifetime;
use crate::{
    arena::ID,
    symbol::{
        self,
        table::{self, representation::Index, DisplayObject, State, Table},
        ConstantParameterID, Generic, LifetimeParameterID,
        TraitImplementationID, TypeParameterID, Variance,
    },
    type_system::{
        compatible, deduction,
        instantiation::Instantiation,
        model::{Default, Model},
        normalizer::Normalizer,
        order,
        predicate::Predicate,
        query::{self, Context},
        term::{
            constant::Constant, lifetime::Lifetime, r#type::Type,
            GenericArguments,
        },
        unification::{self, Log},
        Compute, Environment, LifetimeConstraint, Output, OverflowError,
        Premise, Satisfied, Succeeded, TraitContext,
    },
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

impl<T: State, M: Model> table::Display<T> for Trait<M>
where
    GenericArguments<M>: table::Display<T>,
{
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct HigherRankedLifetimeUnifyingConfig;

impl<M: Model> unification::Predicate<Lifetime<M>>
    for HigherRankedLifetimeUnifyingConfig
{
    fn unifiable(
        &self,
        _: &Lifetime<M>,
        _: &Lifetime<M>,
        _: &Vec<Log<M>>,
        _: &Vec<Log<M>>,
    ) -> Result<Output<Satisfied, M>, OverflowError> {
        Ok(Some(Succeeded::satisfied()))
    }
}

impl<M: Model> unification::Predicate<Type<M>>
    for HigherRankedLifetimeUnifyingConfig
{
    fn unifiable(
        &self,
        _: &Type<M>,
        _: &Type<M>,
        _: &Vec<Log<M>>,
        _: &Vec<Log<M>>,
    ) -> Result<Output<Satisfied, M>, OverflowError> {
        Ok(None)
    }
}

impl<M: Model> unification::Predicate<Constant<M>>
    for HigherRankedLifetimeUnifyingConfig
{
    fn unifiable(
        &self,
        _: &Constant<M>,
        _: &Constant<M>,
        _: &Vec<Log<M>>,
        _: &Vec<Log<M>>,
    ) -> Result<Output<Satisfied, M>, OverflowError> {
        Ok(None)
    }
}

impl<M: Model> Compute for Trait<M> {
    type Error = OverflowError;
    type Parameter = ();

    #[allow(private_bounds, private_interfaces)]
    fn implementation(
        &self,
        environment: &Environment<
            Self::Model,
            impl State,
            impl Normalizer<Self::Model>,
        >,
        context: &mut Context<Self::Model>,
        (): Self::Parameter,
        (): Self::InProgress,
    ) -> Result<Option<Self::Result>, Self::Error> {
        // if this query was made in some trait implementation or trait, then
        // check if the trait implementation is the same as the query one.
        if let Some(Succeeded { result: Satisfied, constraints }) =
            is_in_active_trait_implementation(
                self.id,
                false,
                &self.generic_arguments,
                environment,
                context,
            )?
        {
            // pass the check
            if match environment.premise.trait_context {
                TraitContext::InTraitImplementation(
                    trait_implementation_id,
                ) => {
                    let implementation =
                        environment.table.get(trait_implementation_id).unwrap();

                    !self.is_const || implementation.is_const
                }
                TraitContext::InTrait(_) => !self.is_const,
                TraitContext::Normal => false,
            } {
                return Ok(Some(Succeeded::satisfied_with(constraints)));
            }
        }

        // look for the premise that matches
        'outer: for trait_premise in environment
            .premise
            .predicates
            .iter()
            .filter_map(Predicate::as_trait)
        {
            if (self.is_const && !trait_premise.is_const)
                || (self.id != trait_premise.id)
            {
                continue;
            }

            let Some(Succeeded {
                result: forall_lifetime_unification,
                mut constraints,
            }) = self.generic_arguments.unify_as_mapping_with_context(
                &trait_premise.generic_arguments,
                Arc::new(HigherRankedLifetimeUnifyingConfig),
                environment,
                context,
            )?
            else {
                continue;
            };

            // check if all the lifetimes equal
            for (key, lifetimes) in forall_lifetime_unification.lifetimes.iter()
            {
                let key_lifetime_is_forall = key.is_forall();
                let mut lifetimes_iter = lifetimes.iter();

                // get the first lifetime to compare with the rest
                let Some(first_lifetime) = lifetimes_iter.next() else {
                    continue 'outer;
                };

                // if the key is forall, then all the lifetimes must be forall
                if key_lifetime_is_forall && !first_lifetime.is_forall() {
                    continue 'outer;
                }

                for lifetime in lifetimes_iter {
                    // the lifetime must match
                    if first_lifetime != lifetime {
                        // otherwise, create a lifetime matching constraint

                        // can't create a lifetime matching constraint if the
                        // lifetimes are forall
                        if lifetime.is_forall() || first_lifetime.is_forall() {
                            continue 'outer;
                        }

                        constraints.insert(
                            LifetimeConstraint::LifetimeMatching(
                                first_lifetime.clone(),
                                lifetime.clone(),
                            ),
                        );
                    }
                }
            }

            // satisfied by the trait premise
            return Ok(Some(Succeeded::satisfied_with(constraints)));
        }

        // manually search for the trait implementation
        match resolve_implementation_with_context(
            self.id,
            &self.generic_arguments,
            environment,
            context,
        ) {
            Ok(implementation) => {
                let implementation_sym =
                    environment.table.get(implementation.result.id).unwrap();

                if self.is_const && !implementation_sym.is_const {
                    return Ok(None);
                }

                Ok(Some(Succeeded::satisfied_with(implementation.constraints)))
            }

            Err(ResolveError::Overflow(exceed_limit_error)) => {
                Err(exceed_limit_error)
            }

            Err(_) => Ok(None),
        }
    }

    #[allow(private_bounds, private_interfaces)]
    fn on_cyclic(
        &self,
        _: Self::Parameter,
        _: Self::InProgress,
        _: Self::InProgress,
        _: &[query::QueryCall<Self::Model>],
    ) -> Result<Option<Self::Result>, Self::Error> {
        Ok(Some(Succeeded::satisfied())) // co-inductive
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
    pub id: ID<symbol::PositiveTraitImplementation>,
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
    Overflow(#[from] OverflowError),
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
/// - [`ResolveError::Overflow`]: If the session limit was exceeded; see
///   [`OverflowError`] for more information.
#[allow(clippy::too_many_lines)]
pub fn resolve_implementation<M: Model>(
    trait_id: ID<symbol::Trait>,
    generic_arguments: &GenericArguments<M>,
    environment: &Environment<M, impl State, impl Normalizer<M>>,
) -> Result<Succeeded<Implementation<M>, M>, ResolveError> {
    resolve_implementation_with_context(
        trait_id,
        generic_arguments,
        environment,
        &mut Context::new(),
    )
}

pub(in crate::type_system) fn resolve_implementation_with_context<M: Model>(
    trait_id: ID<symbol::Trait>,
    generic_arguments: &GenericArguments<M>,
    environment: &Environment<M, impl State, impl Normalizer<M>>,
    context: &mut Context<M>,
) -> Result<Succeeded<Implementation<M>, M>, ResolveError> {
    let trait_symbol =
        environment.table.get(trait_id).ok_or(ResolveError::InvalidID)?;

    if let Some(Succeeded { result: Satisfied, constraints }) =
        is_in_active_trait_implementation(
            trait_id,
            true,
            generic_arguments,
            environment,
            context,
        )?
    {
        return Ok(Succeeded {
            result: Implementation {
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
            },
            constraints,
        });
    }

    let Some(Succeeded {
        result: Satisfied,
        constraints: mut definite_lifetime_constraints,
    }) = generic_arguments.definite_with_context(environment, context)?
    else {
        return Err(ResolveError::NonDefiniteGenericArguments);
    };

    let mut candidate: Option<(
        TraitImplementationID,
        Instantiation<M>,
        HashSet<LifetimeConstraint<M>>,
    )> = None;

    for (key, arguments) in
        trait_symbol.implementations().iter().copied().map(|k| {
            (
                k,
                GenericArguments::from_default_model(
                    environment
                        .table
                        .get_implementation(k.into())
                        .unwrap()
                        .arguments()
                        .clone(),
                ),
            )
        })
    {
        // builds the unification
        let Succeeded {
            result: instantiation,
            constraints: mut lifetime_constraints,
        } = match arguments.deduce_with_context(
            generic_arguments,
            environment,
            context,
        ) {
            Ok(unification) => unification,

            Err(deduction::Error::Overflow(overflow)) => {
                return Err(overflow.into())
            }

            Err(
                deduction::Error::MismatchedGenericArgumentCount(_)
                | deduction::Error::UnificationFailure(_),
            ) => continue,
        };

        // check if satisfies all the predicate
        let generic_symbol = environment.table.get_generic(key.into()).unwrap();
        let Some(new_constraints) = predicate_satisfies(
            &*generic_symbol,
            &instantiation,
            environment,
            context,
        )?
        else {
            continue;
        };
        lifetime_constraints.extend(new_constraints);

        // assign the candidate
        match &mut candidate {
            Some((
                candidate_id,
                candidate_instantiation,
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

                    combined_premise.predicates.extend(
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
                    combined_premise.predicates.extend(
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
                        .get_implementation(key.into())
                        .unwrap()
                        .arguments()
                        .clone(),
                )
                .order_with_context(
                    &GenericArguments::from_default_model(
                        environment
                            .table
                            .get_implementation((*candidate_id).into())
                            .unwrap()
                            .arguments()
                            .clone(),
                    ),
                    &Environment {
                        premise: &combined_premise,
                        table: environment.table,
                        normalizer: environment.normalizer,
                    },
                    context,
                )? {
                    order::Order::Incompatible => {
                        return Err(ResolveError::AmbiguousTerm)
                    }
                    order::Order::MoreGeneral => {}
                    order::Order::MoreSpecific => {
                        *candidate_id = key;
                        *candidate_instantiation = instantiation;
                        *candidate_lifetime_constraints = lifetime_constraints;
                    }
                    order::Order::Ambiguous => {
                        return Err(ResolveError::AmbiguousTrait)
                    }
                }
            }

            candidate @ None => {
                *candidate = Some((key, instantiation, lifetime_constraints));
            }
        }
    }

    match candidate {
        Some((
            TraitImplementationID::Positive(id),
            deduced_substitution,
            lifetime_constraints,
        )) => Ok(Succeeded {
            result: Implementation { deduced_substitution, id },
            constraints: {
                definite_lifetime_constraints.extend(lifetime_constraints);
                definite_lifetime_constraints
            },
        }),

        None | Some((TraitImplementationID::Negative(_), _, _)) => {
            Err(ResolveError::NotFound)
        }
    }
}

fn predicate_satisfies<M: Model>(
    generic_symbol: &dyn Generic,
    substitution: &Instantiation<M>,
    environment: &Environment<M, impl State, impl Normalizer<M>>,
    context: &mut Context<M>,
) -> Result<Option<HashSet<LifetimeConstraint<M>>>, OverflowError> {
    let mut lifetime_constraints = HashSet::new();

    // check if satisfies all the predicate
    for mut predicate in generic_symbol
        .generic_declaration()
        .predicates
        .iter()
        .map(|x| Predicate::from_default_model(x.predicate.clone()))
    {
        predicate.instantiate(substitution);

        let Some(new_lifetime_constraints) = (match predicate {
            Predicate::TraitTypeEquality(equality) => compatible::compatible(
                &Type::TraitMember(equality.lhs),
                &equality.rhs,
                Variance::Covariant,
                environment,
            )?
            .map(|x| x.constraints),
            Predicate::ConstantType(constant_type) => constant_type
                .query_with_context(environment, context)?
                .map(|x| x.constraints),
            Predicate::LifetimeOutlives(outlives) => Some(
                std::iter::once(LifetimeConstraint::LifetimeOutlives(outlives))
                    .collect(),
            ),
            Predicate::TypeOutlives(outlives) => Some(
                std::iter::once(LifetimeConstraint::TypeOutlives(outlives))
                    .collect(),
            ),

            Predicate::TupleType(tuple_type) => tuple_type
                .query_with_context(environment, context)?
                .map(|x| x.constraints),

            Predicate::TupleConstant(tuple_constant) => tuple_constant
                .query_with_context(environment, context)?
                .map(|x| x.constraints),

            Predicate::Trait(tr) => tr
                .query_with_context(environment, context)?
                .map(|x| x.constraints),
        }) else {
            return Ok(None);
        };

        lifetime_constraints.extend(new_lifetime_constraints);
    }

    Ok(Some(lifetime_constraints))
}

fn is_in_active_trait_implementation<M: Model>(
    trait_id: ID<symbol::Trait>,
    need_implementation: bool,
    generic_arguments: &GenericArguments<M>,
    environment: &Environment<M, impl State, impl Normalizer<M>>,
    context: &mut Context<M>,
) -> Result<Output<Satisfied, M>, OverflowError> {
    match environment.premise.trait_context {
        TraitContext::InTraitImplementation(trait_implementation) => {
            let Some(implementation) =
                environment.table.get(trait_implementation)
            else {
                return Ok(None);
            };

            // check if the trait id is the same
            if implementation.implemented_id() != trait_id {
                return Ok(None);
            }

            // check if the generic arguments are the same
            GenericArguments::from_default_model(
                implementation.arguments.clone(),
            )
            .equals_with_context(
                generic_arguments,
                environment,
                context,
            )
        }
        TraitContext::InTrait(env_trait_id) => {
            if need_implementation || env_trait_id != trait_id {
                return Ok(None);
            }

            let trait_symbol = environment.table.get(trait_id).unwrap();

            trait_symbol
                .generic_declaration()
                .parameters
                .create_identity_generic_arguments(env_trait_id.into())
                .equals_with_context(generic_arguments, environment, context)
        }
        TraitContext::Normal => Ok(None),
    }
}

#[cfg(test)]
mod tests;
