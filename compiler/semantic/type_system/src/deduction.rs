//! Contains the generic arguments deduction logic.

use std::{
    collections::{BTreeMap, BTreeSet},
    hash::Hash,
};

use pernixc_semantic_element::variance::Variance;
use pernixc_term::{
    constant::Constant, generic_arguments::GenericArguments,
    instance::Instance, instantiation::Instantiation, lifetime::Lifetime,
    predicate::Outlives, r#type::Type,
};

use crate::{
    OverflowError, Satisfied, Succeeded,
    environment::Environment,
    equality::Equality,
    lifetime_constraint::LifetimeConstraint,
    mapping::Mapping,
    normalizer::Normalizer,
    term::Term,
    unification::{self, Unification},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct LifetimeUnifyingPredicate;

impl unification::Predicate<Lifetime> for LifetimeUnifyingPredicate {
    fn unifiable(
        &self,
        _: &Lifetime,
        _: &Lifetime,
    ) -> Result<Option<Succeeded<Satisfied>>, OverflowError> {
        Ok(Some(Succeeded::satisfied()))
    }
}

impl unification::Predicate<Type> for LifetimeUnifyingPredicate {
    fn unifiable(
        &self,
        _: &Type,
        _: &Type,
    ) -> Result<Option<Succeeded<Satisfied>>, OverflowError> {
        Ok(None)
    }
}

impl unification::Predicate<Constant> for LifetimeUnifyingPredicate {
    fn unifiable(
        &self,
        _: &Constant,
        _: &Constant,
    ) -> Result<Option<Succeeded<Satisfied>>, OverflowError> {
        Ok(None)
    }
}

impl unification::Predicate<Instance> for LifetimeUnifyingPredicate {
    fn unifiable(
        &self,
        _: &Instance,
        _: &Instance,
    ) -> Result<Option<Succeeded<Satisfied>>, OverflowError> {
        Ok(None)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct DeductionPredicate;

impl unification::Predicate<Lifetime> for DeductionPredicate {
    fn unifiable(
        &self,
        _: &Lifetime,
        _: &Lifetime,
    ) -> Result<Option<Succeeded<Satisfied>>, OverflowError> {
        Ok(Some(Succeeded::satisfied()))
    }
}

impl unification::Predicate<Type> for DeductionPredicate {
    fn unifiable(
        &self,
        from: &Type,
        _: &Type,
    ) -> Result<Option<Succeeded<Satisfied>>, OverflowError> {
        Ok(matches!(from, Type::Parameter(_) | Type::InstanceAssociated(_))
            .then_some(Succeeded::satisfied()))
    }
}

impl unification::Predicate<Constant> for DeductionPredicate {
    fn unifiable(
        &self,
        from: &Constant,
        _: &Constant,
    ) -> Result<Option<Succeeded<Satisfied>>, OverflowError> {
        Ok(matches!(from, Constant::Parameter(_))
            .then_some(Succeeded::satisfied()))
    }
}

impl unification::Predicate<Instance> for DeductionPredicate {
    fn unifiable(
        &self,
        from: &Instance,
        _: &Instance,
    ) -> Result<Option<Succeeded<Satisfied>>, OverflowError> {
        Ok(matches!(
            from,
            Instance::Parameter(_) | Instance::InstanceAssociated(_)
        )
        .then_some(Succeeded::satisfied()))
    }
}

async fn unify<T: Term>(
    lhs: &[T],
    rhs: &[T],
    environment: &Environment<'_, impl Normalizer>,
    mut existing: Succeeded<Mapping>,
) -> Result<Option<Succeeded<Mapping>>, OverflowError> {
    for (lhs, rhs) in lhs.iter().zip(rhs.iter()) {
        let Some(new) = environment
            .query(&Unification::new(
                lhs.clone(),
                rhs.clone(),
                DeductionPredicate,
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
    async fn predicate(
        &mut self,
        left: &T,
        right: &T,
        environment: &Environment<'_, impl Normalizer>,
    ) -> Result<Option<Succeeded<Satisfied>>, OverflowError>;
}

async fn mapping_equals<T: Term, N: Normalizer, P: CompatiblePredicate<T>>(
    unification: BTreeMap<T, BTreeSet<T>>,
    substitution: &Instantiation,
    environment: &Environment<'_, N>,
    mut compatible: P,
) -> Result<Option<Succeeded<Satisfied>>, OverflowError> {
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
) -> Result<Option<Succeeded<BTreeMap<T, T>>>, OverflowError> {
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
    ) -> Result<Option<Succeeded<Satisfied>>, OverflowError> {
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
    ) -> Result<Option<Succeeded<Satisfied>>, OverflowError> {
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
    ) -> Result<Option<Succeeded<Satisfied>>, OverflowError> {
        environment
            .query(&Equality::new(lhs.clone(), rhs.clone()))
            .await
            .map(|x| x.map(|x| (*x).clone()))
    }
}

impl CompatiblePredicate<Instance> for UniToSubs<'_> {
    async fn predicate(
        &mut self,
        lhs: &Instance,
        rhs: &Instance,
        environment: &Environment<'_, impl Normalizer>,
    ) -> Result<Option<Succeeded<Satisfied>>, OverflowError> {
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

struct Equals<'a>(&'a mut bool);

impl CompatiblePredicate<Type> for Equals<'_> {
    async fn predicate(
        &mut self,
        left: &Type,
        right: &Type,
        environment: &Environment<'_, impl Normalizer>,
    ) -> Result<Option<Succeeded<Satisfied>>, OverflowError> {
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

impl CompatiblePredicate<Instance> for Equals<'_> {
    async fn predicate(
        &mut self,
        left: &Instance,
        right: &Instance,
        environment: &Environment<'_, impl Normalizer>,
    ) -> Result<Option<Succeeded<Satisfied>>, OverflowError> {
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
    ) -> Result<Option<Succeeded<Deduction>>, OverflowError> {
        assert!(
            this.arity_matches(target),
            "the arity of the generic arguments should match"
        );

        // unify all kinds of generic arguments
        let Some(unification) = unify(
            this.lifetimes(),
            target.lifetimes(),
            self,
            Succeeded::new(Mapping::default()),
        )
        .await?
        else {
            return Ok(None);
        };
        let Some(unification) =
            unify(this.types(), target.types(), self, unification).await?
        else {
            return Ok(None);
        };
        let Some(unification) =
            unify(this.constants(), target.constants(), self, unification)
                .await?
        else {
            return Ok(None);
        };
        let Some(Succeeded { result: unification, mut constraints }) =
            unify(this.instances(), target.instances(), self, unification)
                .await?
        else {
            return Ok(None);
        };

        // flag determining whether the unification is general enough or not.
        let mut is_not_general_enough = false;

        // separate out the unification between generic parameters and trait
        // members
        let (
            base_unification,
            instance_associated_type_map,
            instance_associated_instance_map,
        ) = {
            let (lifetime_param_map, other_lifetime_map) =
                extract(unification.lifetimes, Lifetime::is_parameter);
            let (type_param_map, instance_associated_type_map) =
                extract(unification.types, Type::is_parameter);
            let (constant_param_map, other_constant_map) =
                extract(unification.constants, Constant::is_parameter);
            let (instance_param_map, instance_associated_instance_map) =
                extract(unification.instances, Instance::is_parameter);

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
                return Ok(None);
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
                return Ok(None);
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
                return Ok(None);
            };

            constraints.extend(new_constraints);

            let Some(Succeeded {
                result: instances,
                constraints: new_constraints,
            }) = from_unification_to_substitution(
                instance_param_map,
                self,
                UniToSubs(&mut is_not_general_enough),
            )
            .await?
            else {
                return Ok(None);
            };

            constraints.extend(new_constraints);

            (
                Instantiation::new(lifetimes, types, constants, instances),
                instance_associated_type_map,
                instance_associated_instance_map,
            )
        };

        let Some(Succeeded {
            result: Satisfied,
            constraints: new_ty_constraints,
        }) = mapping_equals(
            instance_associated_type_map,
            &base_unification,
            self,
            Equals(&mut is_not_general_enough),
        )
        .await?
        else {
            return Ok(None);
        };

        let Some(Succeeded {
            result: Satisfied,
            constraints: new_inst_constraints,
        }) = mapping_equals(
            instance_associated_instance_map,
            &base_unification,
            self,
            Equals(&mut is_not_general_enough),
        )
        .await?
        else {
            return Ok(None);
        };

        constraints.extend(new_ty_constraints);
        constraints.extend(new_inst_constraints);

        Ok(Some(Succeeded {
            result: Deduction {
                instantiation: base_unification,
                is_not_general_enough,
            },
            constraints,
        }))
    }
}
