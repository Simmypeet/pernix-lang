//! Contains the definition of [`Compatible`] logic.

use std::{collections::HashSet, sync::Arc};

use super::{
    equality::Equality,
    model::Model,
    normalizer::Normalizer,
    predicate::Outlives,
    query::Context,
    sub_term::{SubTypeLocation, TermLocation},
    term::{
        constant::Constant,
        lifetime::Lifetime,
        r#type::{self, Type},
        MemberSymbol, ModelOf, Symbol,
    },
    unification::{self, Matching, Unification},
    variance::Variance,
    Compute, Environment, LifetimeConstraint, Output, OverflowError, Satisfied,
    Succeeded,
};
use crate::{
    symbol::table::{State, Table},
    type_system::sub_term::{Location, SubLifetimeLocation},
    unordered_pair::UnorderedPair,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct LifetimeMatching<M: Model> {
    from: Type<M>,
}

impl<M: Model> unification::Predicate<Lifetime<M>> for LifetimeMatching<M> {
    fn unifiable(
        &self,
        _: &Lifetime<M>,
        _: &Lifetime<M>,
        from_logs: &Vec<unification::Log<M>>,
        _: &Vec<unification::Log<M>>,
    ) -> Result<Output<Satisfied, M>, OverflowError> {
        let mut current_from = self.from.clone();

        for (idx, log) in from_logs.iter().enumerate() {
            match log {
                unification::Log::Substructural(location) => match location {
                    TermLocation::Lifetime(lifetime) => {
                        let SubLifetimeLocation::FromType(location) = lifetime;

                        // shouldn't be from the type alias
                        if matches!(
                            (&current_from, location),
                            (
                                Type::Symbol(Symbol {
                                    id: r#type::SymbolID::Type(_),
                                    ..
                                }),
                                r#type::SubLifetimeLocation::Symbol(_)
                            ) | (
                                Type::MemberSymbol(MemberSymbol { .. }),
                                r#type::SubLifetimeLocation::MemberSymbol(_)
                            )
                        ) {
                            return Ok(None);
                        }

                        // should be the last location
                        assert_eq!(idx, from_logs.len() - 1);
                    }
                    TermLocation::Type(location) => {
                        let SubTypeLocation::FromType(location) = location;

                        // shouldn't be from the type alias
                        if matches!(
                            (&current_from, location),
                            (
                                Type::Symbol(Symbol {
                                    id: r#type::SymbolID::Type(_),
                                    ..
                                }),
                                r#type::SubTypeLocation::Symbol(_)
                            ) | (
                                Type::MemberSymbol(MemberSymbol { .. }),
                                r#type::SubTypeLocation::MemberSymbol(_)
                            )
                        ) {
                            return Ok(None);
                        }

                        current_from =
                            location.get_sub_term(&current_from).unwrap();
                    }
                    TermLocation::Constant(_) => {
                        unreachable!("lifetime shouldn't come from constant")
                    }
                },

                unification::Log::RewrittenLifetime(_) => {
                    unreachable!("lifetime shouldn't be rewritten")
                }

                unification::Log::RewrittenType(rewritten) => {
                    current_from = rewritten.clone();
                }

                unification::Log::RewrittenConstant(_) => {
                    unreachable!("Lifetime shouldn't be in the constant")
                }
            }
        }

        Ok(Some(Succeeded::satisfied()))
    }
}

impl<M: Model> unification::Predicate<Type<M>> for LifetimeMatching<M> {
    fn unifiable(
        &self,
        _: &Type<M>,
        _: &Type<M>,
        _: &Vec<unification::Log<M>>,
        _: &Vec<unification::Log<M>>,
    ) -> Result<Output<Satisfied, M>, OverflowError> {
        Ok(None)
    }
}

impl<M: Model> unification::Predicate<Constant<M>> for LifetimeMatching<M> {
    fn unifiable(
        &self,
        _: &Constant<M>,
        _: &Constant<M>,
        _: &Vec<unification::Log<M>>,
        _: &Vec<unification::Log<M>>,
    ) -> Result<Output<Satisfied, M>, OverflowError> {
        Ok(None)
    }
}

#[must_use]
fn append_outlives_constraints_from_unification<M: Model>(
    mut current_from: Type<M>,
    unifier: unification::Unifier<Type<M>>,
    parent_variance: Variance,
    table: &Table<impl State>,
    outlives: &mut HashSet<LifetimeConstraint<M>>,
) -> bool {
    if let Some(rewritten_from) = unifier.rewritten_from {
        current_from = rewritten_from;
    }

    match unifier.matching {
        unification::Matching::Unifiable(_, _) => {
            unreachable!("the can never be unified")
        }
        unification::Matching::Substructural(substructural) => {
            // look for matched lifetimes
            for (location, unification) in substructural.lifetimes {
                match current_from.get_variance_of(
                    table,
                    parent_variance,
                    std::iter::once(TermLocation::Lifetime(
                        SubLifetimeLocation::FromType(location),
                    )),
                ) {
                    Ok(variance) => match variance {
                        Variance::Covariant => {
                            // 'from: 'to
                            if let Matching::Unifiable(from, to) =
                                unification.matching
                            {
                                outlives.insert(
                                    LifetimeConstraint::LifetimeOutlives(
                                        Outlives { operand: from, bound: to },
                                    ),
                                );
                            }
                        }
                        Variance::Contravariant => {
                            // 'to: 'from
                            if let Matching::Unifiable(from, to) =
                                unification.matching
                            {
                                outlives.insert(
                                    LifetimeConstraint::LifetimeOutlives(
                                        Outlives { operand: to, bound: from },
                                    ),
                                );
                            }
                        }
                        Variance::Invariant => {
                            // 'from: 'to
                            // 'to: 'from
                            if let Matching::Unifiable(from, to) =
                                unification.matching
                            {
                                outlives.insert(
                                    LifetimeConstraint::LifetimeMatching(
                                        UnorderedPair::new(
                                            from.clone(),
                                            to.clone(),
                                        ),
                                    ),
                                );
                            }
                        }
                    },

                    Err(_) => {
                        // the variance cannot be determined, flawed term input
                        return false;
                    }
                }
            }

            // look for matched types
            for (location, unification) in substructural.types {
                let current_variance = match current_from.get_variance_of(
                    table,
                    parent_variance,
                    std::iter::once(TermLocation::Type(
                        SubTypeLocation::FromType(location),
                    )),
                ) {
                    Ok(current_variance) => current_variance,
                    Err(_) => return false,
                };

                let new_from = location.get_sub_term(&current_from).unwrap();

                if !append_outlives_constraints_from_unification(
                    new_from,
                    unification,
                    current_variance,
                    table,
                    outlives,
                ) {
                    return false;
                }
            }
        }
        unification::Matching::Equality => {}
    }

    true
}

/// A trait for determining the equality of two terms while considering the
/// variance of the lifetime.
pub trait Compatible: ModelOf {
    /// The implementation of [`Compatible`] algortihm with the context.
    ///
    /// # Parameters
    ///
    /// - `self`: The term to be checked.
    /// - `target`: The target term to be checked against.
    /// - `variance`: The variance to used for determining the constraint of the
    ///   lifetimes. For the most cases, the default should be
    ///   [`Variance::Covariant`]
    fn compatible_with_context(
        &self,
        target: &Self,
        variance: Variance,
        environment: &Environment<
            Self::Model,
            impl State,
            impl Normalizer<Self::Model>,
        >,
        context: &mut Context<Self::Model>,
    ) -> Result<Output<Satisfied, Self::Model>, OverflowError>;

    /// A delegate method for [`compatible_with_context`] with the default
    /// context.
    fn compatible(
        &self,
        target: &Self,
        variance: Variance,
        environment: &Environment<
            Self::Model,
            impl State,
            impl Normalizer<Self::Model>,
        >,
    ) -> Result<Output<Satisfied, Self::Model>, OverflowError> {
        self.compatible_with_context(
            target,
            variance,
            environment,
            &mut Context::new(),
        )
    }
}

impl<M: Model> Compatible for Lifetime<M> {
    fn compatible_with_context(
        &self,
        target: &Self,
        variance: Variance,
        _: &Environment<Self::Model, impl State, impl Normalizer<Self::Model>>,
        _: &mut Context<Self::Model>,
    ) -> Result<Output<Satisfied, Self::Model>, OverflowError> {
        if self == target {
            return Ok(Some(Succeeded::satisfied()));
        }

        let constraint = match variance {
            Variance::Covariant => {
                LifetimeConstraint::LifetimeOutlives(Outlives {
                    operand: self.clone(),
                    bound: target.clone(),
                })
            }
            Variance::Contravariant => {
                LifetimeConstraint::LifetimeOutlives(Outlives {
                    operand: target.clone(),
                    bound: self.clone(),
                })
            }
            Variance::Invariant => LifetimeConstraint::LifetimeMatching(
                UnorderedPair::new(self.clone(), target.clone()),
            ),
        };

        Ok(Some(Succeeded::with_constraints(
            Satisfied,
            std::iter::once(constraint).collect(),
        )))
    }
}

impl<M: Model> Compatible for Type<M> {
    fn compatible_with_context(
        &self,
        target: &Self,
        variance: Variance,
        environment: &Environment<
            Self::Model,
            impl State,
            impl Normalizer<Self::Model>,
        >,
        context: &mut Context<Self::Model>,
    ) -> Result<Output<Satisfied, Self::Model>, OverflowError> {
        let Some(Succeeded { result: unification, mut constraints }) =
            Unification::new(
                self.clone(),
                target.clone(),
                Arc::new(LifetimeMatching { from: self.clone() }),
            )
            .query_with_context(environment, context)?
        else {
            return Ok(None);
        };

        if !append_outlives_constraints_from_unification(
            self.clone(),
            unification,
            variance,
            environment.table,
            &mut constraints,
        ) {
            return Ok(None);
        }

        Ok(Some(Succeeded::with_constraints(Satisfied, constraints)))
    }
}

impl<M: Model> Compatible for Constant<M> {
    fn compatible_with_context(
        &self,
        target: &Self,
        _: Variance,
        environment: &Environment<
            Self::Model,
            impl State,
            impl Normalizer<Self::Model>,
        >,
        context: &mut Context<Self::Model>,
    ) -> Result<Output<Satisfied, Self::Model>, OverflowError> {
        // use default strict equality for constant
        Equality::new(self.clone(), target.clone())
            .query_with_context(environment, context)
    }
}

#[cfg(test)]
mod tests;
