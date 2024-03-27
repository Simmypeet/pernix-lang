use core::fmt;
use std::collections::HashMap;

use thiserror::Error;

use super::{contains_forall_lifetime, Outlives};
use crate::{
    arena::ID,
    semantic::{
        equality,
        instantiation::Instantiation,
        order,
        predicate::{ConstantType, NonEquality, Predicate, Tuple},
        session::{Cached, ExceedLimitError, Limit, Session},
        term::{
            constant::Constant, lifetime::Lifetime, r#type::Type,
            GenericArguments,
        },
        unification, Environment, Premise, Semantic,
    },
    symbol::{
        self, ConstantParameterID, Generic, LifetimeParameterID,
        TraitImplementationKindID, TypeParameterID,
    },
    table::{self, DisplayObject, Index, State, Table},
};

/// Enumeration containing either a lifetime or a type outlives predicate.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum LifetimeConstraint {
    LifetimeOutlives(Outlives<Lifetime>),
    TypeOutlives(Outlives<Type>),
}

/// Represents a predicate stating that there exists an implementation for the
/// given trait and generic arguments
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Trait {
    /// The trait to be implemented.
    pub id: ID<symbol::Trait>,

    /// Whether the implementation is const.
    pub is_const: bool,

    /// The generic arguments supplied to the trait.
    pub generic_arguments: GenericArguments,
}

impl<T: State> table::Display<T> for Trait {
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

impl Trait {
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

/// Result of determining whether a trait bound is satisfiable or not.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Satisfiability {
    /// The additional lifetime constraints that must be satisfied in order to
    /// satisfy the trait predicate.
    pub lifetime_constraints: Vec<LifetimeConstraint>,
}

/// A query for checking [`Trait`] predicate satisfiability.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Query<'a> {
    pub id: ID<symbol::Trait>,
    pub is_const: bool,
    pub generic_arguments: &'a GenericArguments,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct HigherRankedLifetimeUnifyingConfig;

impl unification::Config for HigherRankedLifetimeUnifyingConfig {
    fn lifetime_unifiable(&mut self, from: &Lifetime, _: &Lifetime) -> bool {
        from.is_forall()
    }

    fn type_unifiable(&mut self, _: &Type, _: &Type) -> bool { false }

    fn constant_unifiable(&mut self, _: &Constant, _: &Constant) -> bool {
        false
    }
}

impl Trait {
    /// Determines whether there exists an implementation for the given trait
    /// and generic arguments.
    ///
    /// # Errors
    ///
    /// See [`ExceedLimitError`] for more information.
    #[allow(clippy::too_many_lines)]
    pub fn satisfies<
        S: Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
        R: Session<Lifetime> + Session<Type> + Session<Constant>,
    >(
        id: ID<symbol::Trait>,
        is_const: bool,
        generic_arguments: &GenericArguments,
        premise: &Premise,
        table: &Table<impl State>,
        semantic: &mut S,
        session: &mut Limit<R>,
    ) -> Result<Option<Satisfiability>, ExceedLimitError> {
        match session.mark_as_in_progress(
            Query { id, is_const, generic_arguments },
            (),
        )? {
            Some(Cached::InProgress(())) => {
                // co-inductive
                session.clear_query(Query { id, is_const, generic_arguments });
                return Ok(Some(Satisfiability {
                    lifetime_constraints: Vec::new(),
                }));
            }
            Some(Cached::Done(satisfiability)) => {
                return Ok(Some(satisfiability));
            }
            None => {}
        }

        if table.is_in_active_trait_implementation(
            id,
            false,
            generic_arguments,
            premise,
            semantic,
            session,
        )? {
            let pass = match premise.environment {
                Environment::InTraitImplementation(trait_implementation_id) => {
                    let implementation =
                        table.get(trait_implementation_id).unwrap();

                    !is_const || implementation.is_const
                }
                Environment::InTrait(_) => !is_const,
                Environment::Normal => false,
            };

            // pass the check
            if pass {
                let result =
                    Satisfiability { lifetime_constraints: Vec::new() };
                session.mark_as_done(
                    Query { id, is_const, generic_arguments },
                    result.clone(),
                );

                return Ok(Some(result));
            }
        }

        // look for the premise that matches
        'outer: for trait_premise in premise
            .non_equality_predicates
            .iter()
            .filter_map(NonEquality::as_trait)
        {
            if (is_const && !trait_premise.is_const) || (id != trait_premise.id)
            {
                continue;
            }

            let Some(forall_lifetime_unification) =
                trait_premise.generic_arguments.unify_as_mapping(
                    generic_arguments,
                    premise,
                    table,
                    &mut HigherRankedLifetimeUnifyingConfig,
                    semantic,
                    session,
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
                        premise,
                        table,
                        semantic,
                        session,
                    )? {
                        continue 'outer;
                    }
                }
            }

            let Some(trait_sym) = table.get(trait_premise.id) else {
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
                        let mut predicate = x.predicate.clone();

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
                            .copied()
                            .map(|lifetime| (*forall, lifetime))
                    })
                    .collect(),
                types: HashMap::default(),
                constants: HashMap::default(),
            };

            let mut lifetime_constraints = Vec::new();
            for mut predicate in predicate_with_forall_lifetimes {
                predicate.instantiate(&forall_lifetime_instantiation);

                if !match predicate {
                    Predicate::TypeEquality(equality) => equality::equals(
                        &equality.lhs,
                        &equality.rhs,
                        premise,
                        table,
                        semantic,
                        session,
                    )?,
                    Predicate::ConstantEquality(equality) => equality::equals(
                        &equality.lhs,
                        &equality.rhs,
                        premise,
                        table,
                        semantic,
                        session,
                    )?,
                    Predicate::ConstantType(constant_type) => {
                        ConstantType::satisfies(
                            &constant_type.0,
                            premise,
                            table,
                            semantic,
                            session,
                        )?
                    }
                    Predicate::LifetimeOutlives(outlives) => {
                        lifetime_constraints.push(
                            LifetimeConstraint::LifetimeOutlives(outlives),
                        );
                        true
                    }
                    Predicate::TypeOutlives(outlives) => {
                        lifetime_constraints
                            .push(LifetimeConstraint::TypeOutlives(outlives));
                        true
                    }
                    Predicate::TupleType(tuple) => Tuple::satisfies(
                        &tuple.0, premise, table, semantic, session,
                    )?,
                    Predicate::TupleConstant(tuple) => Tuple::satisfies(
                        &tuple.0, premise, table, semantic, session,
                    )?,
                    Predicate::Trait(tr) => {
                        if let Some(satisfiability) = Self::satisfies(
                            tr.id,
                            tr.is_const,
                            &tr.generic_arguments,
                            premise,
                            table,
                            semantic,
                            session,
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
            session.mark_as_done(
                Query { id, is_const, generic_arguments },
                Satisfiability {
                    lifetime_constraints: lifetime_constraints.clone(),
                },
            );
            return Ok(Some(Satisfiability { lifetime_constraints }));
        }

        // manually search for the trait implementation
        match table.resolve_implementation(
            id,
            generic_arguments,
            premise,
            semantic,
            session,
        ) {
            Ok(implementation) => {
                session.mark_as_done(
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
                session.clear_query(Query { id, is_const, generic_arguments });
                Ok(None)
            }
        }
    }
}

impl Trait {
    /// Applies an instantiation to the generic arguments.
    pub fn instantiate(&mut self, instantiation: &Instantiation) {
        self.generic_arguments.instantiate(instantiation);
    }
}

/// A result of a trait implementation resolution query.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Implementation {
    /// The deduced substitution for the generic arguments of the trait
    /// implementation.
    pub deduced_substitution: Instantiation,

    /// The ID of the resolved trait implementation.
    pub id: ID<symbol::TraitImplementation>,

    /// List of lifetime constraints that are introduced by the implements.
    pub lifetime_constraints: Vec<LifetimeConstraint>,
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

impl<T: State> Table<T> {
    fn predicate_satisfies<
        S: Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
        R: Session<Lifetime> + Session<Type> + Session<Constant>,
    >(
        &self,
        generic_symbol: &dyn Generic,
        substitution: &Instantiation,
        premise: &Premise,
        semantic: &mut S,
        session: &mut Limit<R>,
    ) -> Result<Option<Vec<LifetimeConstraint>>, ExceedLimitError> {
        let mut lifetime_constraints = Vec::new();

        // check if satisfies all the predicate
        for mut predicate in generic_symbol
            .generic_declaration()
            .predicates
            .iter()
            .map(|x| &x.predicate)
            .cloned()
        {
            predicate.instantiate(substitution);

            match predicate {
                Predicate::TypeEquality(equality) => {
                    if !equality::equals(
                        &equality.lhs,
                        &equality.rhs,
                        premise,
                        self,
                        semantic,
                        session,
                    )? {
                        return Ok(None);
                    }
                }
                Predicate::ConstantEquality(equality) => {
                    if !equality::equals(
                        &equality.lhs,
                        &equality.rhs,
                        premise,
                        self,
                        semantic,
                        session,
                    )? {
                        return Ok(None);
                    }
                }
                Predicate::ConstantType(constant_type) => {
                    if !ConstantType::satisfies(
                        &constant_type.0,
                        premise,
                        self,
                        semantic,
                        session,
                    )? {
                        return Ok(None);
                    }
                }
                Predicate::LifetimeOutlives(outlives) => lifetime_constraints
                    .push(LifetimeConstraint::LifetimeOutlives(outlives)),
                Predicate::TypeOutlives(outlives) => lifetime_constraints
                    .push(LifetimeConstraint::TypeOutlives(outlives)),

                Predicate::TupleType(tuple_type) => {
                    if !Tuple::satisfies(
                        &tuple_type.0,
                        premise,
                        self,
                        semantic,
                        session,
                    )? {
                        return Ok(None);
                    }
                }

                Predicate::TupleConstant(tuple_constant) => {
                    if !Tuple::satisfies(
                        &tuple_constant.0,
                        premise,
                        self,
                        semantic,
                        session,
                    )? {
                        return Ok(None);
                    }
                }

                Predicate::Trait(tr) => {
                    if let Some(satisfiability) = Trait::satisfies(
                        tr.id,
                        tr.is_const,
                        &tr.generic_arguments,
                        premise,
                        self,
                        semantic,
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

    /// Resolves for the trait implementation for the given trait and generic
    /// arguments.
    ///
    /// # Errors
    ///
    /// - [`ResolveError::InvalidID`]: If the `trait_id` is invalid.
    /// - [`ResolveError::NonDefiniteGenericArguments`]: If the
    ///   `generic_arguments` are not definite.
    /// - [`ResolveError::AmbiguousTrait`]: If the trait defined in the table is
    ///   ambiguous (multiple trait implementation matches).
    /// - [`ResolveError::NotFound`]: If the trait implementation was not found.
    /// - [`ResolveError::ExceedLimitError`]: If the session limit was exceeded;
    ///   see [`ExceedLimitError`] for more information.
    #[allow(clippy::too_many_lines)]
    pub fn resolve_implementation<
        S: Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
        R: Session<Lifetime> + Session<Type> + Session<Constant>,
    >(
        &self,
        trait_id: ID<symbol::Trait>,
        generic_arguments: &GenericArguments,
        premise: &Premise,
        semantic: &mut S,
        session: &mut Limit<R>,
    ) -> Result<Implementation, ResolveError> {
        let trait_symbol = self.get(trait_id).ok_or(ResolveError::InvalidID)?;

        if self.is_in_active_trait_implementation(
            trait_id,
            true,
            generic_arguments,
            premise,
            semantic,
            session,
        )? {
            return Ok(Implementation {
                deduced_substitution: {
                    let implementation = self
                        .get(
                            premise
                                .environment
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
                                        parent: premise
                                            .environment
                                            .into_in_trait_implementation()
                                            .unwrap()
                                            .into(),
                                        id: x.0,
                                    });

                                (lifetime, lifetime)
                            })
                            .collect(),

                        types: implementation
                            .signature
                            .generic_declaration
                            .parameters
                            .type_parameters_as_order()
                            .map(|x| {
                                let ty = Type::Parameter(TypeParameterID {
                                    parent: premise
                                        .environment
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
                                        parent: premise
                                            .environment
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
                id: premise.environment.into_in_trait_implementation().unwrap(),
                lifetime_constraints: Vec::new(),
            });
        }

        if !generic_arguments.definite(premise, self, semantic, session)? {
            return Err(ResolveError::NonDefiniteGenericArguments);
        }

        let mut candidate: Option<(
            TraitImplementationKindID,
            Instantiation,
            Vec<LifetimeConstraint>,
        )> = None;

        for (key, arguments) in trait_symbol
            .implementations
            .iter()
            .map(|k| {
                (
                    TraitImplementationKindID::Positive(*k),
                    self.get(*k).unwrap().signature.arguments.clone(),
                )
            })
            .chain(trait_symbol.negative_implementations.iter().map(|k| {
                (
                    TraitImplementationKindID::Negative(*k),
                    self.get(*k).unwrap().signature.arguments.clone(),
                )
            }))
        {
            // builds the unification
            let Some(unification) = arguments.deduce(
                generic_arguments,
                premise,
                self,
                semantic,
                session,
            )?
            else {
                continue;
            };

            // check if satisfies all the predicate
            let generic_symbol = self.get_generic(key.into()).unwrap();
            let Some(lifetime_constraints) = self.predicate_satisfies(
                &*generic_symbol,
                &unification,
                premise,
                semantic,
                session,
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
                    // check which one is more specific
                    match self
                        .get_trait_implementation_signature(key)
                        .unwrap()
                        .arguments
                        .order(
                            &self
                                .get_trait_implementation_signature(
                                    *candidate_id,
                                )
                                .unwrap()
                                .arguments,
                            premise,
                            self,
                            semantic,
                            session,
                        )? {
                        order::Order::Incompatible => {
                            return Err(ResolveError::AmbiguousTerm)
                        }
                        order::Order::MoreGeneral => {}
                        order::Order::MoreSpecific => {
                            *candidate_id = key;
                            *candidate_unification = unification;
                            *candidate_lifetime_constraints =
                                lifetime_constraints;
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

    fn is_in_active_trait_implementation<
        S: Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
        R: Session<Lifetime> + Session<Type> + Session<Constant>,
    >(
        &self,
        trait_id: ID<symbol::Trait>,
        need_implementation: bool,
        generic_arguments: &GenericArguments,
        premise: &Premise,
        semantic: &mut S,
        session: &mut Limit<R>,
    ) -> Result<bool, ExceedLimitError> {
        match premise.environment {
            Environment::InTraitImplementation(trait_implementation) => {
                let Some(implementation) = self.get(trait_implementation)
                else {
                    return Ok(false);
                };

                // check if the trait id is the same
                if implementation.signature.implemented_id != trait_id {
                    return Ok(false);
                }

                // check if the generic arguments are the same
                implementation.signature.arguments.equals(
                    generic_arguments,
                    premise,
                    self,
                    semantic,
                    session,
                )
            }
            Environment::InTrait(env_trait_id) => {
                if need_implementation || env_trait_id != trait_id {
                    return Ok(false);
                }

                let trait_symbol = self.get(trait_id).unwrap();

                trait_symbol
                    .generic_declaration()
                    .parameters
                    .create_identity_generic_arguments(env_trait_id.into())
                    .equals(generic_arguments, premise, self, semantic, session)
            }
            Environment::Normal => Ok(false),
        }
    }
}
