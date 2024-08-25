//! Contains the generic arguments deduction logic.

use std::{
    collections::{BTreeMap, BTreeSet},
    hash::Hash,
    sync::Arc,
};

use super::{
    compatible::{Compatibility, Compatible},
    equality::Equality,
    instantiation::{self, Instantiation},
    mapping::Mapping,
    model::Model,
    normalizer::Normalizer,
    observer::Observer,
    query::Context,
    term::{
        constant::Constant, lifetime::Lifetime, r#type::Type, GenericArguments,
        Term,
    },
    unification::{self, Log, Unification},
    variance::Variance,
    Compute, Environment, Output, Satisfied, Succeeded,
};
use crate::{
    symbol::{table::State, GenericKind},
    type_system::{
        predicate::Outlives, LifetimeConstraint, LifetimeUnifyingPredicate,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct DuductionPredicate;

impl<M: Model> unification::Predicate<Lifetime<M>> for DuductionPredicate {
    fn unifiable(
        &self,
        _: &Lifetime<M>,
        _: &Lifetime<M>,
        _: &Vec<Log<M>>,
        _: &Vec<Log<M>>,
    ) -> Result<Output<Satisfied, M>, super::OverflowError> {
        Ok(Some(Succeeded::satisfied()))
    }
}

impl<M: Model> unification::Predicate<Type<M>> for DuductionPredicate {
    fn unifiable(
        &self,
        from: &Type<M>,
        _: &Type<M>,
        _: &Vec<Log<M>>,
        _: &Vec<Log<M>>,
    ) -> Result<Output<Satisfied, M>, super::OverflowError> {
        Ok(matches!(from, Type::Parameter(_) | Type::TraitMember(_))
            .then_some(Succeeded::satisfied()))
    }
}

impl<M: Model> unification::Predicate<Constant<M>> for DuductionPredicate {
    fn unifiable(
        &self,
        from: &Constant<M>,
        _: &Constant<M>,
        _: &Vec<Log<M>>,
        _: &Vec<Log<M>>,
    ) -> Result<Output<Satisfied, M>, super::OverflowError> {
        Ok(matches!(from, Constant::Parameter(_))
            .then_some(Succeeded::satisfied()))
    }
}

fn unify<T: Term, S: State>(
    lhs: &[T],
    rhs: &[T],
    environment: &Environment<
        T::Model,
        S,
        impl Normalizer<T::Model, S>,
        impl Observer<T::Model, S>,
    >,
    context: &mut Context<T::Model>,
    mut existing: Succeeded<Mapping<T::Model>, T::Model>,
) -> Result<Output<Mapping<T::Model>, T::Model>, Error> {
    for (lhs, rhs) in lhs.iter().zip(rhs.iter()) {
        let Some(new) = Unification::new(
            lhs.clone(),
            rhs.clone(),
            Arc::new(DuductionPredicate),
        )
        .query_with_context(environment, context)?
        else {
            return Ok(None);
        };

        existing.constraints.extend(new.constraints);
        existing.result.append_from_unifier(new.result);
    }

    Ok(Some(existing))
}

fn extract<K: Ord, V>(
    map: BTreeMap<K, V>,
    mut predicate: impl FnMut(&K) -> bool,
) -> (BTreeMap<K, V>, BTreeMap<K, V>) {
    let mut positive = BTreeMap::new();
    let mut negative = BTreeMap::new();

    for (key, value) in map {
        if predicate(&key) {
            positive.insert(key, value);
        } else {
            negative.insert(key, value);
        }
    }

    (positive, negative)
}

fn mapping_equals<
    T: Term,
    S: State,
    N: Normalizer<T::Model, S>,
    O: Observer<T::Model, S>,
>(
    unification: BTreeMap<T, BTreeSet<T>>,
    substitution: &Instantiation<T::Model>,
    environment: &Environment<T::Model, S, N, O>,
    context: &mut Context<T::Model>,
    mut compatible: impl FnMut(
        &T,
        &T,
        &Environment<T::Model, S, N, O>,
        &mut Context<T::Model>,
    ) -> Result<Output<Satisfied, T::Model>, Error>,
) -> Result<Output<Satisfied, T::Model>, Error> {
    let mut constraints = BTreeSet::new();

    for (mut key, values) in unification {
        instantiation::instantiate(&mut key, substitution);

        for value in values {
            let Some(Succeeded {
                result: Satisfied,
                constraints: new_constraint,
            }) = compatible(&key, &value, environment, context)?
            else {
                continue;
            };

            constraints.extend(new_constraint);
        }
    }

    Ok(Some(Succeeded::satisfied_with(constraints)))
}

#[allow(clippy::type_complexity)]
fn from_unification_to_substitution<
    T: Term,
    S: State,
    N: Normalizer<T::Model, S>,
    O: Observer<T::Model, S>,
>(
    unification: BTreeMap<T, BTreeSet<T>>,
    environment: &Environment<T::Model, S, N, O>,
    context: &mut Context<T::Model>,
    mut compatible: impl FnMut(
        &T,
        &T,
        &Environment<T::Model, S, N, O>,
        &mut Context<T::Model>,
    ) -> Result<
        Output<Satisfied, T::Model>,
        super::OverflowError,
    >,
) -> Result<Output<BTreeMap<T, T>, T::Model>, super::OverflowError> {
    let mut result = BTreeMap::new();

    let mut constraints = BTreeSet::new();

    for (key, values) in unification {
        let mut values = values.into_iter();

        let sampled = values.next().expect("should at least have one element");

        for value in values {
            let Some(compat) =
                compatible(&sampled, &value, environment, context)?
            else {
                return Ok(None);
            };

            constraints.extend(compat.constraints);
        }

        assert!(result.insert(key, sampled).is_none());
    }

    Ok(Some(Succeeded::with_constraints(result, constraints)))
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error(
    "the number of generic arguments between the two generic arguments does \
     not match"
)]
#[allow(missing_docs)]
pub struct MismatchedGenericArgumentCountError {
    pub lhs_count: usize,
    pub rhs_count: usize,
    pub kind: GenericKind,
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error("can't unify the two generic arguments")]
#[allow(missing_docs)]
pub struct UnificationFailureError;

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum Error {
    #[error(transparent)]
    TypeSystem(#[from] super::OverflowError),

    #[error(transparent)]
    MismatchedGenericArgumentCount(#[from] MismatchedGenericArgumentCountError),

    #[error(transparent)]
    UnificationFailure(#[from] UnificationFailureError),
}

/// Results of deduction from generic arguments.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Deduction<M: Model> {
    /// The instantiation of all generic parameters.
    pub instantiation: Instantiation<M>,

    /// If `true`, the lifetime parameter in the generic arguments of the
    /// implementation is not general to accomodate forall lifetimes.
    pub is_not_general_enough: bool,
}

impl<M: Model> GenericArguments<M> {
    /// Performs generic parameter deduction.
    ///
    /// # Errors
    ///
    /// See [`OverflowError`] for more information.
    pub fn deduce<S: State>(
        &self,
        target: &Self,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
    ) -> Result<Succeeded<Deduction<M>, M>, Error> {
        self.deduce_with_context(target, environment, &mut Context::new())
    }

    #[allow(clippy::too_many_lines)]
    pub(super) fn deduce_with_context<S: State>(
        &self,
        target: &Self,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
        context: &mut Context<M>,
    ) -> Result<Succeeded<Deduction<M>, M>, Error> {
        // arity check
        if self.lifetimes.len() != target.lifetimes.len() {
            return Err(MismatchedGenericArgumentCountError {
                lhs_count: self.lifetimes.len(),
                rhs_count: target.lifetimes.len(),
                kind: GenericKind::Lifetime,
            }
            .into());
        }
        if self.types.len() != target.types.len() {
            return Err(MismatchedGenericArgumentCountError {
                lhs_count: self.types.len(),
                rhs_count: target.types.len(),
                kind: GenericKind::Type,
            }
            .into());
        }
        if self.constants.len() != target.constants.len() {
            return Err(MismatchedGenericArgumentCountError {
                lhs_count: self.constants.len(),
                rhs_count: target.constants.len(),
                kind: GenericKind::Constant,
            }
            .into());
        }

        // unify all kinds of generic arguments
        let Some(unification) = unify(
            &self.lifetimes,
            &target.lifetimes,
            environment,
            context,
            Succeeded::new(Mapping::default()),
        )?
        else {
            return Err(UnificationFailureError.into());
        };
        let Some(unification) = unify(
            &self.types,
            &target.types,
            environment,
            context,
            unification,
        )?
        else {
            return Err(UnificationFailureError.into());
        };
        let Some(Succeeded { result: unification, mut constraints }) = unify(
            &self.constants,
            &target.constants,
            environment,
            context,
            unification,
        )?
        else {
            return Err(UnificationFailureError.into());
        };

        // flag determining whether the unification is general enough or not.
        let mut is_not_general_enough = false;

        // separate out the unification between generic parameters and trait
        // members
        let (base_unification, trait_type_map) = {
            let (lifetime_param_map, other_lifetime_map) =
                extract(unification.lifetimes, Lifetime::is_parameter);
            let (type_param_map, trait_type_map) =
                extract(unification.types, Type::is_parameter);
            let (constant_param_map, other_constant_map) =
                extract(unification.constants, Constant::is_parameter);

            assert!(other_constant_map.is_empty());

            // add lifetime constraints
            for (key, values) in other_lifetime_map {
                for value in values {
                    if key == value {
                        continue;
                    }

                    if value.is_forall() || key.is_forall() {
                        is_not_general_enough = true;
                    } else {
                        constraints.insert(
                            LifetimeConstraint::LifetimeOutlives(
                                Outlives::new(key.clone(), value.clone()),
                            ),
                        );
                        constraints.insert(
                            LifetimeConstraint::LifetimeOutlives(
                                Outlives::new(value, key.clone()),
                            ),
                        );
                    }
                }
            }

            let Some(Succeeded {
                result: lifetimes,
                constraints: new_constraints,
            }) = from_unification_to_substitution(
                lifetime_param_map,
                environment,
                context,
                |lhs, rhs, _, _| {
                    if lhs == rhs {
                        Ok(Some(Succeeded::satisfied()))
                    } else if lhs.is_forall() || rhs.is_forall() {
                        is_not_general_enough = true;

                        Ok(Some(Succeeded::satisfied()))
                    } else {
                        Ok(Some(Succeeded::satisfied_with(
                            [
                                LifetimeConstraint::LifetimeOutlives(
                                    Outlives::new(lhs.clone(), rhs.clone()),
                                ),
                                LifetimeConstraint::LifetimeOutlives(
                                    Outlives::new(rhs.clone(), lhs.clone()),
                                ),
                            ]
                            .into_iter()
                            .collect(),
                        )))
                    }
                },
            )?
            else {
                return Err(UnificationFailureError.into());
            };

            constraints.extend(new_constraints);

            let Some(Succeeded { result: types, constraints: new_constraints }) =
                from_unification_to_substitution(
                    type_param_map,
                    environment,
                    context,
                    |lhs, rhs, environment, context| {
                        let Some(Succeeded {
                            result: unification,
                            mut constraints,
                        }) = Unification::new(
                            lhs.clone(),
                            rhs.clone(),
                            Arc::new(LifetimeUnifyingPredicate),
                        )
                        .query_with_context(environment, context)?
                        else {
                            return Ok(None);
                        };

                        let mut mapping = Mapping::default();
                        mapping.append_from_unifier(unification);

                        assert!(mapping.types.is_empty());
                        assert!(mapping.constants.is_empty());

                        // all lifetimes must strictly matched
                        for (lhs, values) in mapping.lifetimes {
                            for rhs in values {
                                if lhs == rhs {
                                    continue;
                                }

                                if lhs.is_forall() || rhs.is_forall() {
                                    is_not_general_enough = true;
                                } else {
                                    constraints.insert(
                                        LifetimeConstraint::LifetimeOutlives(
                                            Outlives::new(
                                                lhs.clone(),
                                                rhs.clone(),
                                            ),
                                        ),
                                    );
                                    constraints.insert(
                                        LifetimeConstraint::LifetimeOutlives(
                                            Outlives::new(rhs, lhs.clone()),
                                        ),
                                    );
                                }
                            }
                        }

                        Ok(Some(Succeeded::satisfied_with(constraints)))
                    },
                )?
            else {
                return Err(UnificationFailureError.into());
            };
            constraints.extend(new_constraints);

            let Some(Succeeded {
                result: constants,
                constraints: new_constraints,
            }) = from_unification_to_substitution(
                constant_param_map,
                environment,
                context,
                |lhs, rhs, environment, context| {
                    Equality::new(lhs.clone(), rhs.clone())
                        .query_with_context(environment, context)
                },
            )?
            else {
                return Err(UnificationFailureError.into());
            };

            constraints.extend(new_constraints);

            (Instantiation { lifetimes, types, constants }, trait_type_map)
        };

        let Some(Succeeded { result: Satisfied, constraints: new_constraints }) =
            mapping_equals(
                trait_type_map,
                &base_unification,
                environment,
                context,
                |term, target, environment, context| match term
                    .compatible_with_context(
                        target,
                        Variance::Covariant,
                        environment,
                        context,
                    )? {
                    Some(Succeeded {
                        result: Compatibility { forall_lifetime_errors, .. },
                        constraints,
                    }) => {
                        if !forall_lifetime_errors.is_empty() {
                            is_not_general_enough = true;
                        }

                        Ok(Some(Succeeded::satisfied_with(constraints)))
                    }
                    None => Ok(None),
                },
            )?
        else {
            return Err(UnificationFailureError.into());
        };

        constraints.extend(new_constraints);

        Ok(Succeeded {
            result: Deduction {
                instantiation: base_unification,
                is_not_general_enough,
            },
            constraints,
        })
    }
}
