//! Contains the generic arguments deduction logic.

use std::{
    collections::{BTreeMap, BTreeSet},
    hash::Hash,
};

use pernixc_query::runtime::executor;
use pernixc_semantic_element::variance::Variance;
use pernixc_term::{
    constant::Constant, generic_arguments::GenericArguments,
    generic_parameters::GenericKind, instantiation::Instantiation,
    lifetime::Lifetime, predicate::Outlives, r#type::Type,
};

use crate::{
    environment::Environment,
    equality::Equality,
    lifetime_constraint::LifetimeConstraint,
    mapping::Mapping,
    normalizer::Normalizer,
    term::Term,
    unification::{self, Log, Unification},
    OverflowError, Satisfied, Succeeded,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct LifetimeUnifyingPredicate;

impl unification::Predicate<Lifetime> for LifetimeUnifyingPredicate {
    fn unifiable(
        &self,
        _: &Lifetime,
        _: &Lifetime,
        _: &[Log],
        _: &[Log],
    ) -> crate::Result<Satisfied> {
        Ok(Some(Succeeded::satisfied()))
    }
}

impl unification::Predicate<Type> for LifetimeUnifyingPredicate {
    fn unifiable(
        &self,
        _: &Type,
        _: &Type,
        _: &[Log],
        _: &[Log],
    ) -> crate::Result<Satisfied> {
        Ok(None)
    }
}

impl unification::Predicate<Constant> for LifetimeUnifyingPredicate {
    fn unifiable(
        &self,
        _: &Constant,
        _: &Constant,
        _: &[Log],
        _: &[Log],
    ) -> crate::Result<Satisfied> {
        Ok(None)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct DuductionPredicate;

impl unification::Predicate<Lifetime> for DuductionPredicate {
    fn unifiable(
        &self,
        _: &Lifetime,
        _: &Lifetime,
        _: &[Log],
        _: &[Log],
    ) -> crate::Result<Satisfied> {
        Ok(Some(Succeeded::satisfied()))
    }
}

impl unification::Predicate<Type> for DuductionPredicate {
    fn unifiable(
        &self,
        from: &Type,
        _: &Type,
        _: &[Log],
        _: &[Log],
    ) -> crate::Result<Satisfied> {
        Ok(matches!(from, Type::Parameter(_) | Type::TraitMember(_))
            .then_some(Succeeded::satisfied()))
    }
}

impl unification::Predicate<Constant> for DuductionPredicate {
    fn unifiable(
        &self,
        from: &Constant,
        _: &Constant,
        _: &[Log],
        _: &[Log],
    ) -> crate::Result<Satisfied> {
        Ok(matches!(from, Constant::Parameter(_))
            .then_some(Succeeded::satisfied()))
    }
}

async fn unify<T: Term>(
    lhs: &[T],
    rhs: &[T],
    environment: &Environment<'_, impl Normalizer>,
    mut existing: Succeeded<Mapping>,
) -> crate::Result<Mapping, Error> {
    for (lhs, rhs) in lhs.iter().zip(rhs.iter()) {
        let Some(new) = environment
            .query(&Unification::new(
                lhs.clone(),
                rhs.clone(),
                DuductionPredicate,
            ))
            .await?
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

trait CompatiblePredicate<T> {
    fn predicate(
        &mut self,
        left: &T,
        right: &T,
        environment: &Environment<'_, impl Normalizer>,
    ) -> impl std::future::Future<
        Output = Result<Option<Succeeded<Satisfied>>, Error>,
    > + Send;
}

async fn mapping_equals<T: Term, N: Normalizer, P: CompatiblePredicate<T>>(
    unification: BTreeMap<T, BTreeSet<T>>,
    substitution: &Instantiation,
    environment: &Environment<'_, N>,
    mut compatible: P,
) -> crate::Result<Satisfied, Error> {
    let mut constraints = BTreeSet::new();

    for (mut key, values) in unification {
        substitution.instantiate(&mut key);

        for value in values {
            let Some(Succeeded {
                result: Satisfied,
                constraints: new_constraint,
            }) = compatible.predicate(&key, &value, environment).await?
            else {
                continue;
            };

            constraints.extend(new_constraint);
        }
    }

    Ok(Some(Succeeded::satisfied_with(constraints)))
}

#[allow(clippy::type_complexity)]
async fn from_unification_to_substitution<T: Term, N: Normalizer>(
    unification: BTreeMap<T, BTreeSet<T>>,
    environment: &Environment<'_, N>,
    mut compatible: impl CompatiblePredicate<T>,
) -> crate::Result<BTreeMap<T, T>, Error> {
    let mut result = BTreeMap::new();

    let mut constraints = BTreeSet::new();

    for (key, values) in unification {
        let mut values = values.into_iter();

        let sampled = values.next().expect("should at least have one element");

        for value in values {
            let Some(compat) =
                compatible.predicate(&sampled, &value, environment).await?
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

    #[error(transparent)]
    CyclicDependency(#[from] executor::CyclicError),
}

impl From<crate::Error> for Error {
    fn from(value: crate::Error) -> Self {
        match value {
            crate::Error::Overflow(overflow_error) => {
                Self::Overflow(overflow_error)
            }
            crate::Error::CyclicDependency(abort) => {
                Self::CyclicDependency(abort)
            }
        }
    }
}

/// Results of deduction from generic arguments.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Deduction {
    /// The instantiation of all generic parameters.
    pub instantiation: Instantiation,

    /// If `true`, the lifetime parameter in the generic arguments of the
    /// implementation is not general to accomodate forall lifetimes.
    pub is_not_general_enough: bool,
}

struct UniToSubs<'a>(&'a mut bool);

impl CompatiblePredicate<Lifetime> for UniToSubs<'_> {
    async fn predicate(
        &mut self,
        lhs: &Lifetime,
        rhs: &Lifetime,
        _: &Environment<'_, impl Normalizer>,
    ) -> Result<Option<Succeeded<Satisfied>>, Error> {
        if lhs == rhs {
            Ok(Some(Succeeded::satisfied()))
        } else if lhs.is_forall() || rhs.is_forall() {
            *self.0 = true;

            Ok(Some(Succeeded::satisfied()))
        } else {
            Ok(Some(Succeeded::satisfied_with(
                [
                    LifetimeConstraint::LifetimeOutlives(Outlives::new(
                        lhs.clone(),
                        rhs.clone(),
                    )),
                    LifetimeConstraint::LifetimeOutlives(Outlives::new(
                        rhs.clone(),
                        lhs.clone(),
                    )),
                ]
                .into_iter()
                .collect(),
            )))
        }
    }
}

impl CompatiblePredicate<Type> for UniToSubs<'_> {
    async fn predicate(
        &mut self,
        lhs: &Type,
        rhs: &Type,
        environment: &Environment<'_, impl Normalizer>,
    ) -> Result<Option<Succeeded<Satisfied>>, Error> {
        let Some(unifier) = environment
            .query(&Unification::new(
                lhs.clone(),
                rhs.clone(),
                LifetimeUnifyingPredicate,
            ))
            .await?
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
                    *self.0 = true;
                } else {
                    constraints.insert(LifetimeConstraint::LifetimeOutlives(
                        Outlives::new(lhs.clone(), rhs.clone()),
                    ));
                    constraints.insert(LifetimeConstraint::LifetimeOutlives(
                        Outlives::new(rhs.clone(), lhs.clone()),
                    ));
                }
            }
        }

        Ok(Some(Succeeded::satisfied_with(constraints)))
    }
}

impl CompatiblePredicate<Constant> for UniToSubs<'_> {
    async fn predicate(
        &mut self,
        lhs: &Constant,
        rhs: &Constant,
        environment: &Environment<'_, impl Normalizer>,
    ) -> Result<Option<Succeeded<Satisfied>>, Error> {
        Ok(environment
            .query(&Equality::new(lhs.clone(), rhs.clone()))
            .await
            .map(|x| x.map(|x| (*x).clone()))?)
    }
}

struct Equals<'a>(&'a mut bool);

impl CompatiblePredicate<Type> for Equals<'_> {
    async fn predicate(
        &mut self,
        left: &Type,
        right: &Type,
        environment: &Environment<'_, impl Normalizer>,
    ) -> Result<Option<Succeeded<Satisfied>>, Error> {
        environment
            .subtypes(left.clone(), right.clone(), Variance::Covariant)
            .await?
            .map_or(Ok(None), |result| {
                if !result.result.forall_lifetime_errors.is_empty() {
                    *self.0 = true;
                }

                Ok(Some(Succeeded::satisfied_with(result.constraints.clone())))
            })
    }
}

impl<N: Normalizer> Environment<'_, N> {
    /// Performs generic parameter deduction.
    ///
    /// # Errors
    ///
    /// See [`Error`] for more information.
    #[allow(clippy::too_many_lines)]
    pub async fn deduce(
        &self,
        this: &GenericArguments,
        target: &GenericArguments,
    ) -> Result<Succeeded<Deduction>, Error> {
        // arity check
        if this.lifetimes.len() != target.lifetimes.len() {
            return Err(MismatchedGenericArgumentCountError {
                lhs_count: this.lifetimes.len(),
                rhs_count: target.lifetimes.len(),
                kind: GenericKind::Lifetime,
            }
            .into());
        }
        if this.types.len() != target.types.len() {
            return Err(MismatchedGenericArgumentCountError {
                lhs_count: this.types.len(),
                rhs_count: target.types.len(),
                kind: GenericKind::Type,
            }
            .into());
        }
        if this.constants.len() != target.constants.len() {
            return Err(MismatchedGenericArgumentCountError {
                lhs_count: this.constants.len(),
                rhs_count: target.constants.len(),
                kind: GenericKind::Constant,
            }
            .into());
        }

        // unify all kinds of generic arguments
        let Some(unification) = unify(
            &this.lifetimes,
            &target.lifetimes,
            self,
            Succeeded::new(Mapping::default()),
        )
        .await?
        else {
            return Err(UnificationFailureError.into());
        };
        let Some(unification) =
            unify(&this.types, &target.types, self, unification).await?
        else {
            return Err(UnificationFailureError.into());
        };
        let Some(Succeeded { result: unification, mut constraints }) =
            unify(&this.constants, &target.constants, self, unification)
                .await?
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
                                Outlives::new(value.clone(), key.clone()),
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
                self,
                UniToSubs(&mut is_not_general_enough),
            )
            .await?
            else {
                return Err(UnificationFailureError.into());
            };

            constraints.extend(new_constraints);

            let Some(Succeeded { result: types, constraints: new_constraints }) =
                from_unification_to_substitution(
                    type_param_map,
                    self,
                    UniToSubs(&mut is_not_general_enough),
                )
                .await?
            else {
                return Err(UnificationFailureError.into());
            };
            constraints.extend(new_constraints);

            let Some(Succeeded {
                result: constants,
                constraints: new_constraints,
            }) = from_unification_to_substitution(
                constant_param_map,
                self,
                UniToSubs(&mut is_not_general_enough),
            )
            .await?
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
                self,
                Equals(&mut is_not_general_enough),
            )
            .await?
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
