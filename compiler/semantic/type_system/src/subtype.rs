//! Defines the [`Subtype`] query.

use std::{
    collections::{btree_map::Entry, BTreeMap, BTreeSet},
    sync::Arc,
};

use pernixc_semantic_element::variance::Variance;
use pernixc_term::{
    constant::Constant,
    generic_arguments::{GenericArguments, TraitMember},
    lifetime::{Forall, Lifetime},
    matching::{Match, Matching},
    predicate::{self, Outlives},
    r#type::Type,
    sub_term::{SubLifetimeLocation, SubTypeLocation, TermLocation},
    visitor,
};

use crate::{
    environment::{BoxedFuture, Environment, Query},
    equality::Equality,
    lifetime_constraint::LifetimeConstraint,
    normalizer::Normalizer,
    term::Term,
    variance::get_variance_of,
    Succeeded,
};

/// A query for subtyping.
///
/// Subtyping is similar to the [`crate::equality::Equality`] query buy allows
/// sub-typing between lifetimes. Given the variance.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, derive_new::new,
)]
pub struct Subtype<T> {
    /// Represents the term that will "be assigned" by the [`Self::source`] in
    /// the subtyping relation.
    pub target: T,

    /// Represents the term that will "assign to" the [`Self::target`] in
    /// the subtyping relation.
    pub source: T,

    /// The variance of ambient where the term subtyping relation is in.
    /// Typically, it will be [`Variance::Covariant`]
    pub variance: Variance,
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

/// The forall lifetime on the `source` side can be matched with only exactly
/// one forall lifetime on the `target` side (including normal lifetimes).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ForallLifetimeMatchedMoreThanOnceError {
    /// The forall lifetime found on the `target` side.
    pub forall_lifetime: Forall,
}

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

/// An enumeration of the possible errors related to forall lifetimes when
/// determining the compatibility of two terms.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum ForallLifetimeError {
    NotGeneralEnoughLifetime(NotGeneralEnoughLifetimeError),
    ForallLifetimeMatchedMoreThanOnce(ForallLifetimeMatchedMoreThanOnceError),
}

/// A result of successfully checking subtyping relation. Note that there's
/// still some lifetime errors possible but overall, the term has a "compatible"
/// shape.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Subtypable {
    /// The result of matching the lifetime with the forall lifetimes.
    pub forall_lifetime_instantiations: ForallLifetimeInstantiation,

    /// List of all errors related to for-all lifetimes.
    pub forall_lifetime_errors: BTreeSet<ForallLifetimeError>,
}

impl<T: Term> Query for Subtype<T> {
    type Parameter = ();
    type InProgress = ();
    type Result = Succeeded<Subtypable>;
    type Error = crate::Error;

    fn query<'x, N: Normalizer>(
        &'x self,
        environment: &'x Environment<'x, N>,
        (): Self::Parameter,
        (): Self::InProgress,
    ) -> BoxedFuture<'x, Self::Result, Self::Error> {
        Box::pin(Impl::query(self, environment))
    }
}

#[doc(hidden)]
pub trait Impl: Sized {
    fn query(
        subtype: &Subtype<Self>,
        environment: &Environment<impl Normalizer>,
    ) -> impl std::future::Future<
        Output = Result<Option<Arc<Succeeded<Subtypable>>>, crate::Error>,
    > + Send;
}

impl Impl for Lifetime {
    async fn query(
        subtype: &Subtype<Self>,
        _: &Environment<'_, impl Normalizer>,
    ) -> Result<Option<Arc<Succeeded<Subtypable>>>, crate::Error> {
        if subtype.source == subtype.target {
            return Ok(Some(Arc::new(Succeeded::new(Subtypable::default()))));
        }

        match (subtype.target, subtype.source) {
            (self_lifetime, Self::Forall(forall_source)) => {
                let mut subtypable = Subtypable::default();

                assert!(subtypable
                    .forall_lifetime_instantiations
                    .lifetimes_by_forall
                    .insert(forall_source, self_lifetime)
                    .is_none());

                return Ok(Some(Arc::new(Succeeded::new(subtypable))));
            }

            // source is not forall.
            //
            // this is like declaring a variable `for<'x> fn(&'x i32)` but got
            // assigned by some function pointer with specific lifetime like
            // `fn(&'static i32)`
            (Self::Forall(self_forall), sourcve) => {
                let error = ForallLifetimeError::NotGeneralEnoughLifetime(
                    NotGeneralEnoughLifetimeError {
                        forall_lifetime: self_forall,
                        lifetime: sourcve,
                    },
                );

                return Ok(Some(Arc::new(Succeeded::new(Subtypable {
                    forall_lifetime_instantiations:
                        ForallLifetimeInstantiation::default(),
                    forall_lifetime_errors: std::iter::once(error).collect(),
                }))));
            }

            _ => {}
        }

        let constraints: BTreeSet<_> = match subtype.variance {
            Variance::Covariant => {
                std::iter::once(LifetimeConstraint::LifetimeOutlives(
                    Outlives { operand: subtype.target, bound: subtype.source },
                ))
                .collect()
            }
            Variance::Contravariant => {
                std::iter::once(LifetimeConstraint::LifetimeOutlives(
                    Outlives { operand: subtype.source, bound: subtype.target },
                ))
                .collect()
            }
            Variance::Bivariant => BTreeSet::new(),
            Variance::Invariant => [
                LifetimeConstraint::LifetimeOutlives(Outlives::new(
                    subtype.target,
                    subtype.source,
                )),
                LifetimeConstraint::LifetimeOutlives(Outlives::new(
                    subtype.source,
                    subtype.target,
                )),
            ]
            .into_iter()
            .collect(),
        };

        Ok(Some(Arc::new(Succeeded::with_constraints(
            Subtypable::default(),
            constraints,
        ))))
    }
}

fn merge_result(
    current: &mut Succeeded<Subtypable>,
    new: &Succeeded<Subtypable>,
) {
    use ForallLifetimeError::ForallLifetimeMatchedMoreThanOnce as MoreThanOnce;

    current.constraints.extend(new.constraints.iter().cloned());
    current
        .result
        .forall_lifetime_errors
        .extend(new.result.forall_lifetime_errors.iter().copied());

    // try to compose a new forall lifetime instantiation
    for (from, to) in new
        .result
        .forall_lifetime_instantiations
        .lifetimes_by_forall
        .iter()
        .map(|(from, to)| (*from, *to))
    {
        match current
            .result
            .forall_lifetime_instantiations
            .lifetimes_by_forall
            .entry(from)
        {
            Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(to);
            }

            Entry::Occupied(occupied_entry) => {
                // check if the lifetime is the same
                if *occupied_entry.get() == to {
                    // nothing to worry about
                    continue;
                }

                // if both are forall lifetime and aren't the same, it's
                // impossible to create lifetime constraints on both forall
                // lifetimes
                let has_forall =
                    occupied_entry.get().is_forall() || to.is_forall();

                if has_forall {
                    let error_reported =
                        current.result.forall_lifetime_errors.iter().any(
                            |error| {
                                let MoreThanOnce(x) = error else {
                                    return false;
                                };

                                x.forall_lifetime == from
                            },
                        );

                    // report to the error list
                    if !error_reported {
                        let error = MoreThanOnce(
                            ForallLifetimeMatchedMoreThanOnceError {
                                forall_lifetime: from,
                            },
                        );

                        current.result.forall_lifetime_errors.insert(error);
                    }
                } else {
                    // neither is forall lifetime, add to the
                    // constraints
                    current.constraints.insert(
                        LifetimeConstraint::LifetimeOutlives(Outlives::new(
                            to,
                            *occupied_entry.get(),
                        )),
                    );
                    current.constraints.insert(
                        LifetimeConstraint::LifetimeOutlives(Outlives::new(
                            *occupied_entry.get(),
                            to,
                        )),
                    );
                }
            }
        }
    }
}

async fn subtypable_with_normalization(
    target: &Type,
    source: &Type,
    current_variance: Variance,
    environment: &Environment<'_, impl Normalizer>,
) -> Result<Option<Succeeded<Subtypable>>, crate::Error> {
    if let Some(Succeeded { result: eq, mut constraints }) =
        target.normalize(environment).await?
    {
        if let Some(result) = environment
            .query(&Subtype::new(eq, source.clone(), current_variance))
            .await?
        {
            constraints.extend(result.constraints.iter().cloned());
            return Ok(Some(Succeeded::with_constraints(
                result.result.clone(),
                constraints,
            )));
        }
    }

    if let Some(Succeeded { result: eq, mut constraints }) =
        source.normalize(environment).await?
    {
        if let Some(result) = environment
            .query(&Subtype::new(target.clone(), eq, current_variance))
            .await?
        {
            constraints.extend(result.constraints.iter().cloned());
            return Ok(Some(Succeeded::with_constraints(
                result.result.clone(),
                constraints,
            )));
        }
    }

    Ok(None)
}

// Matches each of the inner term one by one and then recursively call subtype
// query with the variance correctly transformed.
async fn subtypable_with_unification(
    target: &Type,
    source: &Type,
    current_variance: Variance,
    environment: &Environment<'_, impl Normalizer>,
) -> Result<Option<Succeeded<Subtypable>>, crate::Error> {
    let Some(matching) = target.substructural_match(source) else {
        return Ok(None);
    };

    let mut result = Succeeded::new(Subtypable::default());

    // normally, the `lhs_location` and `rhs_location` are the same except when
    // matching the tuple packing range

    for Matching {
        lhs: target_lt,
        rhs: source_lt,
        lhs_location: source_location,
        ..
    } in matching.lifetimes
    {
        let inner_variance = environment
            .tracked_engine()
            .get_variance_of(
                target,
                current_variance,
                std::iter::once(TermLocation::Lifetime(
                    SubLifetimeLocation::FromType(source_location),
                )),
            )
            .await?;

        let Some(new_result) = environment
            .query(&Subtype::new(target_lt, source_lt, inner_variance))
            .await?
        else {
            return Ok(None);
        };

        merge_result(&mut result, &new_result);
    }

    for Matching {
        lhs: target_ty,
        rhs: source_ty,
        lhs_location: source_location,
        ..
    } in matching.types
    {
        let inner_variance = environment
            .tracked_engine()
            .get_variance_of(
                target,
                current_variance,
                std::iter::once(TermLocation::Type(SubTypeLocation::FromType(
                    source_location,
                ))),
            )
            .await?;

        let Some(new_result) = environment
            .query(&Subtype::new(target_ty, source_ty, inner_variance))
            .await?
        else {
            return Ok(None);
        };

        merge_result(&mut result, &new_result);
    }

    for Matching { lhs: target_const, rhs: source_const, .. } in
        matching.constants
    {
        // constant should have variance influenced, as of now, we'll simply
        // propagate the current variance down
        let Some(new_result) = environment
            .query(&Subtype::new(target_const, source_const, current_variance))
            .await?
        else {
            return Ok(None);
        };

        merge_result(&mut result, &new_result);
    }

    Ok(Some(result))
}

async fn subtypable_without_mapping(
    target: &Type,
    source: &Type,
    variance: Variance,
    environment: &Environment<'_, impl Normalizer>,
) -> Result<Option<Succeeded<Subtypable>>, crate::Error> {
    // trivially satisfied
    if target == source {
        return Ok(Some(Succeeded::new(Subtypable::default())));
    }

    if let Some(result) =
        subtypable_with_unification(target, source, variance, environment)
            .await?
    {
        return Ok(Some(result));
    }

    if let Some(result) =
        subtypable_with_normalization(target, source, variance, environment)
            .await?
    {
        return Ok(Some(result));
    }

    Ok(None)
}

async fn handle_mapping(
    first: &Type,
    last: &Type,
    variance: Variance,
    last_is_source: bool,
    predicate: &predicate::Compatible<TraitMember, Type>,
    environment: &Environment<'_, impl Normalizer>,
) -> Result<Option<Arc<Succeeded<Subtypable>>>, crate::Error> {
    if first.as_trait_member().is_none() {
        return Ok(None);
    }

    let trait_member_term = Type::TraitMember(predicate.lhs.clone());

    let Some(mut result) = subtypable_without_mapping(
        first,
        &trait_member_term,
        // lifetimes in the trait members are always invariant
        Variance::Invariant,
        environment,
    )
    .await?
    else {
        return Ok(None);
    };

    // should have no forall lifetime errors
    if !result.result.forall_lifetime_errors.is_empty() {
        return Ok(None);
    }

    // apply the forall lifetime instantiation to the rhs side of the predicate
    let mut rhs = predicate.rhs.clone();
    result.result.forall_lifetime_instantiations.instantiate(&mut rhs);

    let mut final_query_target = last.clone();
    let mut final_query_source = rhs;

    // swap place so that the `last` is in the source position
    if last_is_source {
        std::mem::swap(&mut final_query_target, &mut final_query_source);
    }

    // finally perform subtyping with the trait member equivalent with the
    // `last`
    let Some(inner_result) = environment
        .query(&Subtype::new(final_query_target, final_query_source, variance))
        .await?
    else {
        return Ok(None);
    };

    result.constraints.extend(inner_result.constraints.iter().cloned());

    Ok(Some(Arc::new(Succeeded {
        result: inner_result.result.clone(),
        constraints: result.constraints,
    })))
}

impl Impl for Type {
    async fn query(
        subtype: &Subtype<Self>,
        environment: &Environment<'_, impl Normalizer>,
    ) -> Result<Option<Arc<Succeeded<Subtypable>>>, crate::Error> {
        if let Some(result) = subtypable_without_mapping(
            &subtype.target,
            &subtype.source,
            subtype.variance,
            environment,
        )
        .await?
        {
            return Ok(Some(Arc::new(result)));
        }

        // can no longer be rewritten via trait member equality predicates
        if !subtype.source.is_trait_member()
            && !subtype.target.is_trait_member()
        {
            return Ok(None);
        }

        // looking at the premise
        for predicate in environment
            .premise()
            .predicates
            .iter()
            .filter_map(|x| x.as_trait_type_compatible())
        {
            if let Some(result) = handle_mapping(
                &subtype.source,
                &subtype.target,
                subtype.variance,
                false,
                predicate,
                environment,
            )
            .await?
            {
                return Ok(Some(result));
            }

            if let Some(result) = handle_mapping(
                &subtype.target,
                &subtype.source,
                subtype.variance,
                true,
                predicate,
                environment,
            )
            .await?
            {
                return Ok(Some(result));
            }
        }

        Ok(None)
    }
}

impl Impl for Constant {
    async fn query(
        subtype: &Subtype<Self>,
        environment: &Environment<'_, impl Normalizer>,
    ) -> Result<Option<Arc<Succeeded<Subtypable>>>, crate::Error> {
        // use strict equality in case of constant since the constant terms
        // don't havel lifetime anyway
        environment
            .query(&Equality::new(
                subtype.source.clone(),
                subtype.target.clone(),
            ))
            .await
            .map(|x| x.map(|_| Arc::new(Succeeded::new(Subtypable::default()))))
    }
}

impl<N: Normalizer> Environment<'_, N> {
    /// Determines the sub-typing relationship between two terms.
    pub async fn subtypes<T: Term>(
        &self,
        target: T,
        source: T,
        variance: Variance,
    ) -> Result<Option<Arc<Succeeded<Subtypable>>>, crate::Error> {
        self.query(&Subtype::new(target, source, variance)).await
    }

    /// Determines the sub-typing relationship between two generic arguments.
    /// This function assumes [`Variance::Invariant`]
    pub async fn subtypes_generic_arguments(
        &self,
        target: &GenericArguments,
        source: &GenericArguments,
    ) -> Result<Option<Succeeded<Subtypable>>, crate::Error> {
        if target.lifetimes.len() != source.lifetimes.len()
            || target.types.len() != source.types.len()
            || target.constants.len() != source.constants.len()
        {
            return Ok(None);
        }

        let mut result = Succeeded::with_constraints(
            Subtypable::default(),
            BTreeSet::default(),
        );

        for (target_lt, source_lt) in
            source.lifetimes.iter().zip(target.lifetimes.iter())
        {
            let Some(new_result) = self
                .query(&Subtype::new(
                    *source_lt,
                    *target_lt,
                    Variance::Invariant,
                ))
                .await?
            else {
                return Ok(None);
            };

            merge_result(&mut result, &new_result);
        }

        for (target_ty, source_ty) in
            source.types.iter().zip(target.types.iter())
        {
            let Some(new_result) = self
                .query(&Subtype::new(
                    source_ty.clone(),
                    target_ty.clone(),
                    Variance::Invariant,
                ))
                .await?
            else {
                return Ok(None);
            };

            merge_result(&mut result, &new_result);
        }

        for (target_const, source_const) in
            source.constants.iter().zip(target.constants.iter())
        {
            let Some(new_result) = self
                .query(&Subtype::new(
                    source_const.clone(),
                    target_const.clone(),
                    Variance::Invariant,
                ))
                .await?
            else {
                return Ok(None);
            };

            merge_result(&mut result, &new_result);
        }

        Ok(Some(result))
    }
}

#[cfg(test)]
mod test;
