//! Contains the definition of [`Compatible`] logic.

use std::{
    collections::{btree_map::Entry, BTreeMap, BTreeSet},
    sync::Arc,
};

use super::{
    equality::Equality,
    model::Model,
    normalizer::Normalizer,
    predicate::Outlives,
    sub_term::{SubTypeLocation, TermLocation},
    term::{
        constant::Constant,
        lifetime::{Forall, Lifetime},
        r#type::Type,
        GenericArguments, ModelOf, Term,
    },
    unification::{self, Matching, Unification},
    variance::Variance,
    visitor, AbruptError, Environment, LifetimeConstraint, Satisfied,
    Succeeded,
};
use crate::{
    type_system,
    type_system::sub_term::{Location, SubLifetimeLocation},
};

/// The result of matching the lifetime with the forall lifetimes.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ForallLifetimeInstantiation<M: Model> {
    /// The instantiation of the forall lifetimes.
    pub lifetimes_by_forall: BTreeMap<Forall, Lifetime<M>>,
}

struct ForallLifetimeInstantiationVisitor<'a, M: Model> {
    instantiations: &'a ForallLifetimeInstantiation<M>,
}

impl<'a, M: Model> visitor::MutableRecursive<Lifetime<M>>
    for ForallLifetimeInstantiationVisitor<'a, M>
{
    fn visit(
        &mut self,
        term: &mut Lifetime<M>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        let Lifetime::Forall(term_forall) = &*term else {
            return true;
        };

        if let Some(instantiated) =
            self.instantiations.lifetimes_by_forall.get(term_forall)
        {
            *term = instantiated.clone();
        }

        true
    }
}

impl<'a, M: Model> visitor::MutableRecursive<Type<M>>
    for ForallLifetimeInstantiationVisitor<'a, M>
{
    fn visit(
        &mut self,
        _: &mut Type<M>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

impl<'a, M: Model> visitor::MutableRecursive<Constant<M>>
    for ForallLifetimeInstantiationVisitor<'a, M>
{
    fn visit(
        &mut self,
        _: &mut Constant<M>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

impl<M: Model> ForallLifetimeInstantiation<M> {
    /// Instantiates the forall lifetimes in the term.
    pub fn instantiate<T: Term<Model = M>>(&self, term: &mut T) {
        let mut visitor =
            ForallLifetimeInstantiationVisitor { instantiations: self };

        visitor::accept_recursive_mut(term, &mut visitor);
    }
}

/// The forall lifetime is found on the `self` side and matched with the
/// non-forall lifetime on the `target` side.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NotGeneralEnoughLifetimeError<M: Model> {
    /// The forall lifetime found on the `self` side.
    pub forall_lifetime: Forall,

    /// The non-forall lifetime found on the `target` side.
    pub lifetime: Lifetime<M>,
}

/// The forall lifetime on the `target` side can be matched with only exactly
/// one forall lifetime on the `self` side (including normal lifetimes).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ForallLifetimeMatchedMoreThanOnceError<M: Model> {
    /// The forall lifetime found on the `target` side.
    pub forall_lifetime: Forall,

    /// The lifetimes found on the `self` side that matched with the forall
    /// lifetime on the `target` side.
    ///
    /// The lifetimes here include at least one forall lifetime.
    pub lifetimes: BTreeSet<Lifetime<M>>,
}

/// An enumeration of the possible errors related to forall lifetimes when
/// determining the compatibility of two terms.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum ForallLifetimeError<M: Model> {
    NotGeneralEnoughLifetime(NotGeneralEnoughLifetimeError<M>),
    ForallLifetimeMatchedMoreThanOnce(
        ForallLifetimeMatchedMoreThanOnceError<M>,
    ),
}

/// The compatibility of two terms.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Compatibility<M: Model> {
    /// The result of matching the lifetime with the forall lifetimes.
    pub forall_lifetime_instantiations: ForallLifetimeInstantiation<M>,

    /// List of all errors related to for-all lifetimes.
    pub forall_lifetime_errors: BTreeSet<ForallLifetimeError<M>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct LifetimeMatchingPredicate<M: Model> {
    from: Type<M>,
}

impl<M: Model> unification::Predicate<Lifetime<M>>
    for LifetimeMatchingPredicate<M>
{
    fn unifiable(
        &self,
        _: &Lifetime<M>,
        _: &Lifetime<M>,
        from_logs: &[unification::Log<M>],
        _: &[unification::Log<M>],
    ) -> Result<Option<Succeeded<Satisfied, M>>, AbruptError> {
        let mut current_from = self.from.clone();

        for (idx, log) in from_logs.iter().enumerate() {
            match log {
                unification::Log::Substructural(location) => match location {
                    TermLocation::Lifetime(_) => {
                        // should be the last location
                        assert_eq!(idx, from_logs.len() - 1);
                    }
                    TermLocation::Type(location) => {
                        let SubTypeLocation::FromType(location) = location;

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

impl<M: Model> unification::Predicate<Type<M>>
    for LifetimeMatchingPredicate<M>
{
    fn unifiable(
        &self,
        _: &Type<M>,
        _: &Type<M>,
        _: &[unification::Log<M>],
        _: &[unification::Log<M>],
    ) -> Result<Option<Succeeded<Satisfied, M>>, AbruptError> {
        Ok(None)
    }
}

impl<M: Model> unification::Predicate<Constant<M>>
    for LifetimeMatchingPredicate<M>
{
    fn unifiable(
        &self,
        _: &Constant<M>,
        _: &Constant<M>,
        _: &[unification::Log<M>],
        _: &[unification::Log<M>],
    ) -> Result<Option<Succeeded<Satisfied, M>>, AbruptError> {
        Ok(None)
    }
}

#[must_use]
fn append_matchings_from_unification<M: Model>(
    mut current_from: Type<M>,
    unifier: &unification::Unifier<Type<M>>,
    parent_variance: Variance,
    environment: &Environment<M, impl Normalizer<M>>,
    matching: &mut BTreeMap<Lifetime<M>, Vec<(Lifetime<M>, Variance)>>,
) -> bool {
    if let Some(rewritten_from) = &unifier.rewritten_from {
        current_from = rewritten_from.clone();
    }

    match &unifier.matching {
        Matching::Unifiable(_, _) => {
            unreachable!("the can never be unified")
        }

        Matching::Substructural(substructural) => {
            // look for matched lifetimes
            for (location, unification) in &substructural.lifetimes {
                match environment.get_variance_of(
                    &current_from,
                    parent_variance,
                    std::iter::once(TermLocation::Lifetime(
                        SubLifetimeLocation::FromType(*location),
                    )),
                ) {
                    Ok(variance) => {
                        if let Matching::Unifiable(self_lt, target_lt) =
                            &unification.matching
                        {
                            matching
                                .entry(target_lt.clone())
                                .or_default()
                                .push((self_lt.clone(), variance));
                        }
                    }

                    Err(_) => {
                        // the variance cannot be determined, flawed term input
                        return false;
                    }
                }
            }

            // look for matched types
            for (location, unification) in &substructural.types {
                let Ok(current_variance) = environment.get_variance_of(
                    &current_from,
                    parent_variance,
                    std::iter::once(TermLocation::Type(
                        SubTypeLocation::FromType(*location),
                    )),
                ) else {
                    return false;
                };

                let new_from = location.get_sub_term(&current_from).unwrap();

                if !append_matchings_from_unification(
                    new_from,
                    unification,
                    current_variance,
                    environment,
                    matching,
                ) {
                    return false;
                }
            }
        }
        Matching::Equality => {}
    }

    true
}

/// A trait for determining the equality of two terms while considering the
/// variance of the lifetime.
pub trait Compatible: ModelOf {
    /// The implementation of [`Compatible`] algorithm.
    ///
    /// This similar to equality but allowing subtypings.
    ///
    /// # Parameters
    ///
    /// - `self`: The term to be checked.
    /// - `target`: The target term to be checked against.
    /// - `variance`: The variance to used for determining the constraint of the
    ///   lifetimes. For the most cases, the default should be
    ///   [`Variance::Covariant`]
    ///
    /// # Errors
    ///
    /// See [`OverflowError`] for more information.
    fn compatible(
        &self,
        target: &Self,
        variance: Variance,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
    ) -> type_system::Result<Compatibility<Self::Model>, Self::Model>;
}

impl<M: Model> Compatible for Lifetime<M> {
    fn compatible(
        &self,
        target: &Self,
        variance: Variance,
        _environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
    ) -> type_system::Result<Compatibility<M>, M> {
        if self == target {
            return Ok(Some(Succeeded::new(Compatibility::default())));
        }

        match (self, target) {
            (self_lifetime, Self::Forall(forall_target)) => {
                let mut compatibility = Compatibility::default();

                assert!(compatibility
                    .forall_lifetime_instantiations
                    .lifetimes_by_forall
                    .insert(forall_target.clone(), self_lifetime.clone())
                    .is_none());

                return Ok(Some(Succeeded::new(compatibility)));
            }

            // target is not forall
            (Self::Forall(self_forall), target) => {
                let error = ForallLifetimeError::NotGeneralEnoughLifetime(
                    NotGeneralEnoughLifetimeError {
                        forall_lifetime: self_forall.clone(),
                        lifetime: target.clone(),
                    },
                );

                return Ok(Some(Succeeded::new(Compatibility {
                    forall_lifetime_instantiations:
                        ForallLifetimeInstantiation::default(),
                    forall_lifetime_errors: std::iter::once(error).collect(),
                })));
            }

            _ => {}
        }

        let constraints: BTreeSet<_> = match variance {
            Variance::Covariant => {
                std::iter::once(LifetimeConstraint::LifetimeOutlives(
                    Outlives { operand: self.clone(), bound: target.clone() },
                ))
                .collect()
            }
            Variance::Contravariant => {
                std::iter::once(LifetimeConstraint::LifetimeOutlives(
                    Outlives { operand: target.clone(), bound: self.clone() },
                ))
                .collect()
            }
            Variance::Invariant => [
                LifetimeConstraint::LifetimeOutlives(Outlives::new(
                    self.clone(),
                    target.clone(),
                )),
                LifetimeConstraint::LifetimeOutlives(Outlives::new(
                    target.clone(),
                    self.clone(),
                )),
            ]
            .into_iter()
            .collect(),
        };

        Ok(Some(Succeeded::with_constraints(
            Compatibility::default(),
            constraints,
        )))
    }
}

#[allow(clippy::too_many_lines)]
fn matching_to_compatiblity<M: Model>(
    matching: BTreeMap<Lifetime<M>, Vec<(Lifetime<M>, Variance)>>,
) -> Succeeded<Compatibility<M>, M> {
    use ForallLifetimeError::ForallLifetimeMatchedMoreThanOnce as MoreThanOnceError;

    let mut compatibility = Compatibility::default();
    let mut constraints = BTreeSet::new();

    for (target_lt, self_lts) in matching {
        for (self_lt, variance) in self_lts.iter().cloned() {
            match (self_lt, target_lt.clone()) {
                (self_lt, Lifetime::Forall(target_forall)) => {
                    match compatibility
                        .forall_lifetime_instantiations
                        .lifetimes_by_forall
                        .entry(target_forall.clone())
                    {
                        Entry::Occupied(entry) => {
                            // check if this lifetime is already matched
                            if entry.get() == &self_lt {
                                continue;
                            }

                            let has_forall =
                                entry.get().is_forall() || self_lt.is_forall();

                            // if either one is forall, then it's an error
                            if has_forall {
                                let error_reported = compatibility
                                    .forall_lifetime_errors
                                    .iter()
                                    .any(|error| {
                                        let MoreThanOnceError(x) = error else {
                                            return false;
                                        };

                                        x.forall_lifetime == target_forall
                                    });

                                // report to the error list
                                if !error_reported {
                                    let error =
                                        MoreThanOnceError(ForallLifetimeMatchedMoreThanOnceError {
                                            forall_lifetime: target_forall,
                                            lifetimes: self_lts
                                                .iter()
                                                .map(|(lifetime, _)| lifetime.clone())
                                                .collect(),
                                        });

                                    compatibility
                                        .forall_lifetime_errors
                                        .insert(error);
                                }
                            } else {
                                // neither is forall lifetime, add to the
                                // constraints
                                constraints.insert(
                                    LifetimeConstraint::LifetimeOutlives(
                                        Outlives::new(
                                            self_lt.clone(),
                                            entry.get().clone(),
                                        ),
                                    ),
                                );
                                constraints.insert(
                                    LifetimeConstraint::LifetimeOutlives(
                                        Outlives::new(
                                            entry.get().clone(),
                                            self_lt,
                                        ),
                                    ),
                                );
                            }
                        }

                        // add this lifetime to instantiation
                        Entry::Vacant(entry) => {
                            entry.insert(self_lt);
                        }
                    }
                }

                (Lifetime::Forall(self_forall), target_lt) => {
                    let error = ForallLifetimeError::NotGeneralEnoughLifetime(
                        NotGeneralEnoughLifetimeError {
                            forall_lifetime: self_forall,
                            lifetime: target_lt,
                        },
                    );

                    // add to the error
                    compatibility.forall_lifetime_errors.insert(error);
                }

                (self_lt, target_lt) => match variance {
                    Variance::Covariant => {
                        constraints.insert(
                            LifetimeConstraint::LifetimeOutlives(Outlives {
                                operand: self_lt,
                                bound: target_lt,
                            }),
                        );
                    }
                    Variance::Contravariant => {
                        constraints.insert(
                            LifetimeConstraint::LifetimeOutlives(Outlives {
                                operand: target_lt,
                                bound: self_lt,
                            }),
                        );
                    }
                    Variance::Invariant => {
                        constraints.insert(
                            LifetimeConstraint::LifetimeOutlives(
                                Outlives::new(
                                    self_lt.clone(),
                                    target_lt.clone(),
                                ),
                            ),
                        );
                        constraints.insert(
                            LifetimeConstraint::LifetimeOutlives(
                                Outlives::new(
                                    target_lt.clone(),
                                    self_lt.clone(),
                                ),
                            ),
                        );
                    }
                },
            }
        }
    }

    Succeeded::with_constraints(compatibility, constraints)
}

impl<M: Model> Compatible for Type<M> {
    fn compatible(
        &self,
        target: &Self,
        variance: Variance,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
    ) -> type_system::Result<Compatibility<M>, M> {
        let Some(unifer) = environment.query(&Unification::new(
            self.clone(),
            target.clone(),
            Arc::new(LifetimeMatchingPredicate { from: self.clone() }),
        ))?
        else {
            return Ok(None);
        };

        let mut lifetime_constraints = unifer.constraints.clone();

        let mut matching = BTreeMap::new();

        if !append_matchings_from_unification(
            self.clone(),
            &unifer.result,
            variance,
            environment,
            &mut matching,
        ) {
            return Ok(None);
        }

        let Succeeded { result, constraints: new_constraint } =
            matching_to_compatiblity(matching);

        lifetime_constraints.extend(new_constraint);

        Ok(Some(Succeeded::with_constraints(result, lifetime_constraints)))
    }
}

impl<M: Model> Compatible for Constant<M> {
    fn compatible(
        &self,
        target: &Self,
        _: Variance,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
    ) -> type_system::Result<Compatibility<M>, M> {
        // use default strict equality for constant
        environment.query(&Equality::new(self.clone(), target.clone())).map(
            |x| {
                x.map(|x| {
                    Succeeded::with_constraints(
                        Compatibility::default(),
                        x.constraints.clone(),
                    )
                })
            },
        )
    }
}

impl<M: Model> GenericArguments<M> {
    /// Determines the compatibility of two generic arguments.
    ///
    /// # Errors
    ///
    /// See [`AbruptError`] for more information.
    pub fn compatible(
        &self,
        target: &Self,
        variance: Variance,
        environment: &mut Environment<M, impl Normalizer<M>>,
    ) -> type_system::Result<Compatibility<M>, M> {
        let mut constraints = BTreeSet::new();

        if self.lifetimes.len() != target.lifetimes.len()
            || self.types.len() != target.types.len()
            || self.constants.len() != target.constants.len()
        {
            return Ok(None);
        }

        let mut matching: BTreeMap<Lifetime<M>, Vec<(Lifetime<M>, Variance)>> =
            BTreeMap::new();

        for (self_lifetime, target_lifetime) in
            self.lifetimes.iter().zip(target.lifetimes.iter())
        {
            matching
                .entry(target_lifetime.clone())
                .or_default()
                .push((self_lifetime.clone(), variance));
        }

        for (self_type, target_type) in
            self.types.iter().zip(target.types.iter())
        {
            let Some(new_unifier) = environment.query(&Unification::new(
                self_type.clone(),
                target_type.clone(),
                Arc::new(LifetimeMatchingPredicate { from: self_type.clone() }),
            ))?
            else {
                return Ok(None);
            };

            constraints.extend(new_unifier.constraints.iter().cloned());

            if !append_matchings_from_unification(
                self_type.clone(),
                &new_unifier.result,
                variance,
                environment,
                &mut matching,
            ) {
                return Ok(None);
            }
        }

        for (self_constant, target_constant) in
            self.constants.iter().zip(target.constants.iter())
        {
            let Some(new_equality) = environment.query(&Equality::new(
                self_constant.clone(),
                target_constant.clone(),
            ))?
            else {
                return Ok(None);
            };

            constraints.extend(new_equality.constraints.iter().cloned());
        }

        let Succeeded { result, constraints: new_constraint } =
            matching_to_compatiblity(matching);

        constraints.extend(new_constraint);

        Ok(Some(Succeeded::with_constraints(result, constraints)))
    }
}

// TODO: bring test back
// #[cfg(test)]
// mod tests;
