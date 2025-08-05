//! Contains the definition of [`Compatible`] logic.

use std::{
    collections::{btree_map::Entry, BTreeMap, BTreeSet},
    future::Future,
};

use pernixc_term::{
    constant::Constant,
    generic_arguments::GenericArguments,
    lifetime::{Forall, Lifetime},
    predicate::Outlives,
    r#type::Type,
    sub_term::{Location, SubLifetimeLocation, SubTypeLocation, TermLocation},
    variance::Variance,
    visitor,
};

use crate::{
    environment::Environment,
    equality::Equality,
    normalizer::Normalizer,
    term::Term,
    unification::{self, Matching, Unification},
    variance::get_variance_of,
    Error, LifetimeConstraint, Satisfied, Succeeded,
};

/// The result of matching the lifetime with the forall lifetimes.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ForallLifetimeInstantiation {
    /// The instantiation of the forall lifetimes.
    pub lifetimes_by_forall: BTreeMap<Forall, Lifetime>,
}

struct ForallLifetimeInstantiationVisitor<'a> {
    instantiations: &'a ForallLifetimeInstantiation,
}

impl visitor::MutableRecursive<Lifetime>
    for ForallLifetimeInstantiationVisitor<'_>
{
    fn visit(
        &mut self,
        term: &mut Lifetime,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        let Lifetime::Forall(term_forall) = &*term else {
            return true;
        };

        if let Some(instantiated) =
            self.instantiations.lifetimes_by_forall.get(term_forall)
        {
            *term = *instantiated;
        }

        true
    }
}

impl visitor::MutableRecursive<Type>
    for ForallLifetimeInstantiationVisitor<'_>
{
    fn visit(
        &mut self,
        _: &mut Type,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

impl visitor::MutableRecursive<Constant>
    for ForallLifetimeInstantiationVisitor<'_>
{
    fn visit(
        &mut self,
        _: &mut Constant,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

impl ForallLifetimeInstantiation {
    /// Instantiates the forall lifetimes in the term.
    pub fn instantiate<T: Term>(&self, term: &mut T) {
        let mut visitor =
            ForallLifetimeInstantiationVisitor { instantiations: self };

        visitor::accept_recursive_mut(term, &mut visitor);
    }
}

/// The forall lifetime is found on the `self` side and matched with the
/// non-forall lifetime on the `target` side.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NotGeneralEnoughLifetimeError {
    /// The forall lifetime found on the `self` side.
    pub forall_lifetime: Forall,

    /// The non-forall lifetime found on the `target` side.
    pub lifetime: Lifetime,
}

/// The forall lifetime on the `target` side can be matched with only exactly
/// one forall lifetime on the `self` side (including normal lifetimes).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ForallLifetimeMatchedMoreThanOnceError {
    /// The forall lifetime found on the `target` side.
    pub forall_lifetime: Forall,

    /// The lifetimes found on the `self` side that matched with the forall
    /// lifetime on the `target` side.
    ///
    /// The lifetimes here include at least one forall lifetime.
    pub lifetimes: BTreeSet<Lifetime>,
}

/// An enumeration of the possible errors related to forall lifetimes when
/// determining the compatibility of two terms.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum ForallLifetimeError {
    NotGeneralEnoughLifetime(NotGeneralEnoughLifetimeError),
    ForallLifetimeMatchedMoreThanOnce(ForallLifetimeMatchedMoreThanOnceError),
}

/// The compatibility of two terms.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Compatibility {
    /// The result of matching the lifetime with the forall lifetimes.
    pub forall_lifetime_instantiations: ForallLifetimeInstantiation,

    /// List of all errors related to for-all lifetimes.
    pub forall_lifetime_errors: BTreeSet<ForallLifetimeError>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct LifetimeMatchingPredicate {
    from: Type,
}

impl unification::Predicate<Lifetime> for LifetimeMatchingPredicate {
    fn unifiable(
        &self,
        _: &Lifetime,
        _: &Lifetime,
        from_logs: &[unification::Log],
        _: &[unification::Log],
    ) -> Result<Option<Succeeded<Satisfied>>, Error> {
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

impl unification::Predicate<Type> for LifetimeMatchingPredicate {
    fn unifiable(
        &self,
        _: &Type,
        _: &Type,
        _: &[unification::Log],
        _: &[unification::Log],
    ) -> Result<Option<Succeeded<Satisfied>>, Error> {
        Ok(None)
    }
}

impl unification::Predicate<Constant> for LifetimeMatchingPredicate {
    fn unifiable(
        &self,
        _: &Constant,
        _: &Constant,
        _: &[unification::Log],
        _: &[unification::Log],
    ) -> Result<Option<Succeeded<Satisfied>>, Error> {
        Ok(None)
    }
}

async fn append_matchings_from_unification(
    mut current_from: Type,
    unifier: &unification::Unifier<Type>,
    parent_variance: Variance,
    environment: &Environment<'_, impl Normalizer>,
    matching: &mut BTreeMap<Lifetime, Vec<(Lifetime, Variance)>>,
) -> Result<bool, Error> {
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
                let variance = environment
                    .tracked_engine()
                    .get_variance_of(
                        &current_from,
                        parent_variance,
                        std::iter::once(TermLocation::Lifetime(
                            SubLifetimeLocation::FromType(*location),
                        )),
                    )
                    .await?;

                if let Matching::Unifiable(self_lt, target_lt) =
                    &unification.matching
                {
                    matching
                        .entry(*target_lt)
                        .or_default()
                        .push((*self_lt, variance));
                }
            }

            // look for matched types
            for (location, unification) in &substructural.types {
                let current_variance = environment
                    .tracked_engine()
                    .get_variance_of(
                        &current_from,
                        parent_variance,
                        std::iter::once(TermLocation::Type(
                            SubTypeLocation::FromType(*location),
                        )),
                    )
                    .await?;

                let new_from = location.get_sub_term(&current_from).unwrap();

                if !Box::pin(append_matchings_from_unification(
                    new_from,
                    unification,
                    current_variance,
                    environment,
                    matching,
                ))
                .await?
                {
                    return Ok(false);
                }
            }
        }
        Matching::Equality => {}
    }

    Ok(true)
}

/// A trait for determining the equality of two terms while considering the
/// variance of the lifetime.
pub trait Compatible {
    /// The implementation of compatible algorithm.
    ///
    /// This similar to equality but allowing subtypings on lifetimes.
    ///
    /// # Parameters
    ///
    /// - `self`: The term to be checked.
    /// - `target`: The target term to be checked against.
    /// - `variance`: The variance to used for determining the constraint of the
    ///   lifetimes. For the most cases, the default should be
    ///   [`Variance::Covariant`]
    #[allow(clippy::missing_errors_doc)]
    fn compatible<'s>(
        &'s self,
        target: &'s Self,
        variance: Variance,
        environment: &'s Environment<'s, impl Normalizer>,
    ) -> impl Future<Output = crate::Result<Compatibility>> + 's;
}

impl Compatible for Lifetime {
    async fn compatible(
        &self,
        target: &Self,
        variance: Variance,
        _environment: &Environment<'_, impl Normalizer>,
    ) -> crate::Result<Compatibility> {
        if self == target {
            return Ok(Some(Succeeded::new(Compatibility::default())));
        }

        match (self, target) {
            (self_lifetime, Self::Forall(forall_target)) => {
                let mut compatibility = Compatibility::default();

                assert!(compatibility
                    .forall_lifetime_instantiations
                    .lifetimes_by_forall
                    .insert(*forall_target, *self_lifetime)
                    .is_none());

                return Ok(Some(Succeeded::new(compatibility)));
            }

            // target is not forall
            (Self::Forall(self_forall), target) => {
                let error = ForallLifetimeError::NotGeneralEnoughLifetime(
                    NotGeneralEnoughLifetimeError {
                        forall_lifetime: *self_forall,
                        lifetime: *target,
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
                    Outlives { operand: *self, bound: *target },
                ))
                .collect()
            }
            Variance::Contravariant => {
                std::iter::once(LifetimeConstraint::LifetimeOutlives(
                    Outlives { operand: *target, bound: *self },
                ))
                .collect()
            }
            Variance::Bivariant => BTreeSet::new(),
            Variance::Invariant => [
                LifetimeConstraint::LifetimeOutlives(Outlives::new(
                    *self, *target,
                )),
                LifetimeConstraint::LifetimeOutlives(Outlives::new(
                    *target, *self,
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
fn matching_to_compatiblity(
    matching: BTreeMap<Lifetime, Vec<(Lifetime, Variance)>>,
) -> Succeeded<Compatibility> {
    use ForallLifetimeError::ForallLifetimeMatchedMoreThanOnce as MoreThanOnceError;

    let mut compatibility = Compatibility::default();
    let mut constraints = BTreeSet::new();

    for (target_lt, self_lts) in matching {
        for (self_lt, variance) in self_lts.iter().copied() {
            match (self_lt, target_lt) {
                (self_lt, Lifetime::Forall(target_forall)) => {
                    match compatibility
                        .forall_lifetime_instantiations
                        .lifetimes_by_forall
                        .entry(target_forall)
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
                                                .map(|(lifetime, _)| *lifetime)
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
                                        Outlives::new(self_lt, *entry.get()),
                                    ),
                                );
                                constraints.insert(
                                    LifetimeConstraint::LifetimeOutlives(
                                        Outlives::new(*entry.get(), self_lt),
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
                    Variance::Bivariant => {}
                    Variance::Invariant => {
                        constraints.insert(
                            LifetimeConstraint::LifetimeOutlives(
                                Outlives::new(self_lt, target_lt),
                            ),
                        );
                        constraints.insert(
                            LifetimeConstraint::LifetimeOutlives(
                                Outlives::new(target_lt, self_lt),
                            ),
                        );
                    }
                },
            }
        }
    }

    Succeeded::with_constraints(compatibility, constraints)
}

impl Compatible for Type {
    async fn compatible(
        &self,
        target: &Self,
        variance: Variance,
        environment: &Environment<'_, impl Normalizer>,
    ) -> crate::Result<Compatibility> {
        let Some(unifer) = Box::pin(environment.query(&Unification::new(
            self.clone(),
            target.clone(),
            LifetimeMatchingPredicate { from: self.clone() },
        )))
        .await?
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
        )
        .await?
        {
            return Ok(None);
        }

        let Succeeded { result, constraints: new_constraint } =
            matching_to_compatiblity(matching);

        lifetime_constraints.extend(new_constraint);

        Ok(Some(Succeeded::with_constraints(result, lifetime_constraints)))
    }
}

impl Compatible for Constant {
    async fn compatible(
        &self,
        target: &Self,
        _: Variance,
        environment: &Environment<'_, impl Normalizer>,
    ) -> crate::Result<Compatibility> {
        // use default strict equality for constant
        environment
            .query(&Equality::new(self.clone(), target.clone()))
            .await
            .map(|x| {
                x.map(|x| {
                    Succeeded::with_constraints(
                        Compatibility::default(),
                        x.constraints.clone(),
                    )
                })
            })
    }
}

impl<N: Normalizer> Environment<'_, N> {
    /// Delegates the method from [`Compatible::compatible`]
    #[allow(clippy::missing_errors_doc)]
    pub async fn compatible<T: Compatible>(
        &self,
        this: &T,
        target: &T,
        variance: Variance,
    ) -> crate::Result<Compatibility> {
        this.compatible(target, variance, self).await
    }

    /// Determines the compatibility of two generic arguments.
    #[allow(clippy::missing_errors_doc)]
    pub async fn generic_arguments_compatible(
        &self,
        this: &GenericArguments,
        target: &GenericArguments,
        variance: Variance,
    ) -> crate::Result<Compatibility> {
        let mut constraints = BTreeSet::new();

        if this.lifetimes.len() != target.lifetimes.len()
            || this.types.len() != target.types.len()
            || this.constants.len() != target.constants.len()
        {
            return Ok(None);
        }

        let mut matching: BTreeMap<Lifetime, Vec<(Lifetime, Variance)>> =
            BTreeMap::new();

        for (self_lifetime, target_lifetime) in
            this.lifetimes.iter().zip(target.lifetimes.iter())
        {
            matching
                .entry(*target_lifetime)
                .or_default()
                .push((*self_lifetime, variance));
        }

        for (self_type, target_type) in
            this.types.iter().zip(target.types.iter())
        {
            let Some(new_unifier) = self
                .query(&Unification::new(
                    self_type.clone(),
                    target_type.clone(),
                    LifetimeMatchingPredicate { from: self_type.clone() },
                ))
                .await?
            else {
                return Ok(None);
            };

            constraints.extend(new_unifier.constraints.iter().cloned());

            if !append_matchings_from_unification(
                self_type.clone(),
                &new_unifier.result,
                variance,
                self,
                &mut matching,
            )
            .await?
            {
                return Ok(None);
            }
        }

        for (self_constant, target_constant) in
            this.constants.iter().zip(target.constants.iter())
        {
            let Some(new_equality) = self
                .query(&Equality::new(
                    self_constant.clone(),
                    target_constant.clone(),
                ))
                .await?
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
