//! Contains the generic arguments deduction logic.

use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    sync::Arc,
};

use super::{
    compatible,
    equality::Equality,
    instantiation::{self, Instantiation},
    mapping::Mapping,
    model::Model,
    normalizer::Normalizer,
    query::Context,
    term::{
        constant::Constant, lifetime::Lifetime, r#type::Type, GenericArguments,
        Term,
    },
    unification::{self, Log, Unification},
    Compute, Environment, LifetimeConstraint, Output, OverflowError, Satisfied,
    Succeeded,
};
use crate::symbol::{table::State, GenericKind, Variance};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct CompatiblePredicate;

impl<M: Model> unification::Predicate<Lifetime<M>> for CompatiblePredicate {
    fn unifiable(
        &self,
        _: &Lifetime<M>,
        _: &Lifetime<M>,
        _: &Vec<Log<M>>,
        _: &Vec<Log<M>>,
    ) -> Result<Output<Satisfied, M>, OverflowError> {
        Ok(None)
    }
}

impl<M: Model> unification::Predicate<Type<M>> for CompatiblePredicate {
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

impl<M: Model> unification::Predicate<Constant<M>> for CompatiblePredicate {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct DuductionPredicate;

impl<M: Model> unification::Predicate<Lifetime<M>> for DuductionPredicate {
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

impl<M: Model> unification::Predicate<Type<M>> for DuductionPredicate {
    fn unifiable(
        &self,
        from: &Type<M>,
        _: &Type<M>,
        _: &Vec<Log<M>>,
        _: &Vec<Log<M>>,
    ) -> Result<Output<Satisfied, M>, OverflowError> {
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
    ) -> Result<Output<Satisfied, M>, OverflowError> {
        Ok(matches!(from, Constant::Parameter(_))
            .then_some(Succeeded::satisfied()))
    }
}

fn unify<T: Term>(
    lhs: &[T],
    rhs: &[T],
    environment: &Environment<T::Model, impl State, impl Normalizer<T::Model>>,
    context: &mut Context<T::Model>,
    mut existing: Succeeded<Mapping<T::Model>, T::Model>,
) -> Result<Output<Mapping<T::Model>, T::Model>, OverflowError> {
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
        existing.result.append_from_unification(new.result);
    }

    Ok(Some(existing))
}

fn extract<K: Eq + Hash, V>(
    map: HashMap<K, V>,
    mut predicate: impl FnMut(&K) -> bool,
) -> (HashMap<K, V>, HashMap<K, V>) {
    let mut positive = HashMap::new();
    let mut negative = HashMap::new();

    for (key, value) in map {
        if predicate(&key) {
            positive.insert(key, value);
        } else {
            negative.insert(key, value);
        }
    }

    (positive, negative)
}

fn mapping_equals<T: Term, S: State, N: Normalizer<T::Model>>(
    unification: HashMap<T, HashSet<T>>,
    substitution: &Instantiation<T::Model>,
    environment: &Environment<T::Model, S, N>,
    context: &mut Context<T::Model>,
    mut compatible: impl FnMut(
        &T,
        &T,
        &Environment<T::Model, S, N>,
        &mut Context<T::Model>,
    )
        -> Result<Output<Satisfied, T::Model>, OverflowError>,
) -> Result<Output<Satisfied, T::Model>, OverflowError> {
    let mut constraints = HashSet::new();

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
    N: Normalizer<T::Model>,
>(
    unification: HashMap<T, HashSet<T>>,
    environment: &Environment<T::Model, S, N>,
    context: &mut Context<T::Model>,
    mut compatible: impl FnMut(
        &T,
        &T,
        &Environment<T::Model, S, N>,
        &mut Context<T::Model>,
    )
        -> Result<Output<Satisfied, T::Model>, OverflowError>,
) -> Result<Output<HashMap<T, T>, T::Model>, OverflowError> {
    let mut result = HashMap::new();
    let mut constraints = HashSet::new();

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
    Overflow(#[from] OverflowError),

    #[error(transparent)]
    MismatchedGenericArgumentCount(#[from] MismatchedGenericArgumentCountError),

    #[error(transparent)]
    UnificationFailure(#[from] UnificationFailureError),
}

impl<M: Model> GenericArguments<M> {
    /// Performs generic parameter deduction.
    ///
    /// # Errors
    ///
    /// See [`OverflowError`] for more information.
    pub fn deduce(
        &self,
        another: &Self,
        environment: &Environment<M, impl State, impl Normalizer<M>>,
    ) -> Result<Succeeded<Instantiation<M>, M>, Error> {
        self.deduce_with_context(another, environment, &mut Context::new())
    }

    #[allow(clippy::too_many_lines)]
    pub(super) fn deduce_with_context(
        &self,
        another: &Self,
        environment: &Environment<M, impl State, impl Normalizer<M>>,
        context: &mut Context<M>,
    ) -> Result<Succeeded<Instantiation<M>, M>, Error> {
        // arity check
        if self.lifetimes.len() != another.lifetimes.len() {
            return Err(MismatchedGenericArgumentCountError {
                lhs_count: self.lifetimes.len(),
                rhs_count: another.lifetimes.len(),
                kind: GenericKind::Lifetime,
            }
            .into());
        }
        if self.types.len() != another.types.len() {
            return Err(MismatchedGenericArgumentCountError {
                lhs_count: self.types.len(),
                rhs_count: another.types.len(),
                kind: GenericKind::Type,
            }
            .into());
        }
        if self.constants.len() != another.constants.len() {
            return Err(MismatchedGenericArgumentCountError {
                lhs_count: self.constants.len(),
                rhs_count: another.constants.len(),
                kind: GenericKind::Constant,
            }
            .into());
        }

        // unify all kinds of generic arguments
        let Some(unification) = unify(
            &self.lifetimes,
            &another.lifetimes,
            environment,
            context,
            Succeeded::new(Mapping::default()),
        )?
        else {
            return Err(UnificationFailureError.into());
        };
        let Some(unification) = unify(
            &self.types,
            &another.types,
            environment,
            context,
            unification,
        )?
        else {
            return Err(UnificationFailureError.into());
        };
        let Some(Succeeded { result: unification, mut constraints }) = unify(
            &self.constants,
            &another.constants,
            environment,
            context,
            unification,
        )?
        else {
            return Err(UnificationFailureError.into());
        };

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

                    constraints.insert(LifetimeConstraint::LifetimeMatching(
                        key.clone(),
                        value.clone(),
                    ));
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
                    } else {
                        Ok(Some(Succeeded::satisfied_with(
                            std::iter::once(
                                LifetimeConstraint::LifetimeMatching(
                                    lhs.clone(),
                                    rhs.clone(),
                                ),
                            )
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
                            Arc::new(CompatiblePredicate),
                        )
                        .query_with_context(environment, context)?
                        else {
                            return Ok(None);
                        };

                        let mut mapping = Mapping::default();
                        mapping.append_from_unification(unification);

                        assert!(mapping.types.is_empty());
                        assert!(mapping.constants.is_empty());

                        // all lifetimes must strictly matched
                        for (lhs, values) in mapping.lifetimes {
                            for rhs in values {
                                if lhs == rhs {
                                    continue;
                                }

                                constraints.insert(
                                    LifetimeConstraint::LifetimeMatching(
                                        lhs.clone(),
                                        rhs.clone(),
                                    ),
                                );
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
                |term, target, environment, context| {
                    compatible::compatible_with_context(
                        term,
                        target,
                        Variance::Bivariant,
                        environment,
                        context,
                    )
                },
            )?
        else {
            return Err(UnificationFailureError.into());
        };
        constraints.extend(new_constraints);

        Ok(Succeeded::with_constraints(base_unification, constraints))
    }
}
