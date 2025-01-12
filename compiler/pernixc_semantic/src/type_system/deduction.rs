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
    term::{
        constant::Constant, lifetime::Lifetime, r#type::Type, GenericArguments,
        Term,
    },
    unification::{self, Log, Unification},
    variance::Variance,
    AbruptError, Environment, Satisfied, Succeeded,
};
use crate::{
    component::generic_parameters::GenericKind,
    type_system::{
        self, predicate::Outlives, LifetimeConstraint,
        LifetimeUnifyingPredicate,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct DuductionPredicate;

impl<M: Model> unification::Predicate<Lifetime<M>> for DuductionPredicate {
    fn unifiable(
        &self,
        _: &Lifetime<M>,
        _: &Lifetime<M>,
        _: &[Log<M>],
        _: &[Log<M>],
    ) -> type_system::Result<Satisfied, M> {
        Ok(Some(Succeeded::satisfied()))
    }
}

impl<M: Model> unification::Predicate<Type<M>> for DuductionPredicate {
    fn unifiable(
        &self,
        from: &Type<M>,
        _: &Type<M>,
        _: &[Log<M>],
        _: &[Log<M>],
    ) -> type_system::Result<Satisfied, M> {
        Ok(matches!(from, Type::Parameter(_) | Type::TraitMember(_))
            .then_some(Succeeded::satisfied()))
    }
}

impl<M: Model> unification::Predicate<Constant<M>> for DuductionPredicate {
    fn unifiable(
        &self,
        from: &Constant<M>,
        _: &Constant<M>,
        _: &[Log<M>],
        _: &[Log<M>],
    ) -> type_system::Result<Satisfied, M> {
        Ok(matches!(from, Constant::Parameter(_))
            .then_some(Succeeded::satisfied()))
    }
}

fn unify<T: Term>(
    lhs: &[T],
    rhs: &[T],
    environment: &Environment<T::Model, impl Normalizer<T::Model>>,
    mut existing: Succeeded<Mapping<T::Model>, T::Model>,
) -> type_system::Result<Mapping<T::Model>, T::Model, Error> {
    for (lhs, rhs) in lhs.iter().zip(rhs.iter()) {
        let Some(new) = environment.query(&Unification::new(
            lhs.clone(),
            rhs.clone(),
            Arc::new(DuductionPredicate),
        ))?
        else {
            return Ok(None);
        };

        existing.constraints.extend(new.constraints.iter().cloned());
        existing.result.append_from_unifier(new.result.clone());
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

fn mapping_equals<T: Term, N: Normalizer<T::Model>>(
    unification: BTreeMap<T, BTreeSet<T>>,
    substitution: &Instantiation<T::Model>,
    environment: &Environment<T::Model, N>,
    mut compatible: impl FnMut(
        &T,
        &T,
        &Environment<T::Model, N>,
    ) -> Result<
        Option<Succeeded<Satisfied, T::Model>>,
        Error,
    >,
) -> type_system::Result<Satisfied, T::Model, Error> {
    let mut constraints = BTreeSet::new();

    for (mut key, values) in unification {
        instantiation::instantiate(&mut key, substitution);

        for value in values {
            let Some(Succeeded {
                result: Satisfied,
                constraints: new_constraint,
            }) = compatible(&key, &value, environment)?
            else {
                continue;
            };

            constraints.extend(new_constraint);
        }
    }

    Ok(Some(Succeeded::satisfied_with(constraints)))
}

#[allow(clippy::type_complexity)]
fn from_unification_to_substitution<T: Term, N: Normalizer<T::Model>>(
    unification: BTreeMap<T, BTreeSet<T>>,
    environment: &Environment<T::Model, N>,
    mut compatible: impl FnMut(
        &T,
        &T,
        &Environment<T::Model, N>,
    ) -> Result<
        Option<Succeeded<Satisfied, T::Model>>,
        super::AbruptError,
    >,
) -> type_system::Result<BTreeMap<T, T>, T::Model, AbruptError> {
    let mut result = BTreeMap::new();

    let mut constraints = BTreeSet::new();

    for (key, values) in unification {
        let mut values = values.into_iter();

        let sampled = values.next().expect("should at least have one element");

        for value in values {
            let Some(compat) = compatible(&sampled, &value, environment)?
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
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum Error {
    #[error(transparent)]
    Abrupt(#[from] super::AbruptError),

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
    /// See [`Error`] for more information.
    #[allow(clippy::too_many_lines)]
    pub fn deduce(
        &self,
        target: &Self,
        environment: &Environment<M, impl Normalizer<M>>,
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
            Succeeded::new(Mapping::default()),
        )?
        else {
            return Err(UnificationFailureError.into());
        };
        let Some(unification) =
            unify(&self.types, &target.types, environment, unification)?
        else {
            return Err(UnificationFailureError.into());
        };
        let Some(Succeeded { result: unification, mut constraints }) = unify(
            &self.constants,
            &target.constants,
            environment,
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
                |lhs, rhs, _| {
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
                    |lhs, rhs, environment| {
                        let Some(unifier) =
                            environment.query(&Unification::new(
                                lhs.clone(),
                                rhs.clone(),
                                Arc::new(LifetimeUnifyingPredicate),
                            ))?
                        else {
                            return Ok(None);
                        };
                        let mut constraints = unifier.constraints.clone();

                        let mut mapping = Mapping::default();
                        mapping.append_from_unifier(unifier.result.clone());

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
                |lhs, rhs, environment| {
                    environment
                        .query(&Equality::new(lhs.clone(), rhs.clone()))
                        .map(|x| x.map(|x| (*x).clone()))
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
                |term, target, environment| match term.compatible(
                    target,
                    Variance::Covariant,
                    environment,
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
