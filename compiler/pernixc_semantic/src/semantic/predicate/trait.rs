use thiserror::Error;

use super::Outlives;
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
        unification, Premise, Semantic,
    },
    symbol::{self, Generic, TraitImplementationKindID},
    table::{Index, State, Table},
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
            // co-inductive reasoning
            Some(Cached::InProgress(())) => {
                return Ok(Some(Satisfiability {
                    lifetime_constraints: Vec::new(),
                }));
            }
            Some(Cached::Done(satisfiability)) => {
                return Ok(Some(satisfiability));
            }
            None => {}
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

            let Some(unification_mapping) =
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

            for lifetimes in unification_mapping.lifetimes.values() {
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

            // if all the lifetimes are equal, then the trait is satisfied
            session.mark_as_done(
                Query { id, is_const, generic_arguments },
                Satisfiability { lifetime_constraints: Vec::new() },
            );
            return Ok(Some(Satisfiability {
                lifetime_constraints: Vec::new(),
            }));
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
                    let mut merged_premise = Premise::from_predicates(
                        generic_symbol
                            .generic_declaration()
                            .predicates
                            .iter()
                            .map(|x| x.predicate.clone()),
                    );
                    let candidate_sym =
                        self.get_generic((*candidate_id).into()).unwrap();

                    merged_premise.append_from_predicates(
                        candidate_sym
                            .generic_declaration()
                            .predicates
                            .iter()
                            .map(|x| x.predicate.clone()),
                    );

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
}
