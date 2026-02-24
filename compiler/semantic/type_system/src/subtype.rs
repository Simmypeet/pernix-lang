//! Defines the [`Subtype`] query.

use std::{
    collections::{BTreeMap, BTreeSet, btree_map::Entry},
    sync::Arc,
};

use pernixc_semantic_element::variance::Variance;
use pernixc_term::{
    constant::Constant,
    generic_arguments::GenericArguments,
    instance::{Instance, InstanceAssociated},
    lifetime::{Forall, Lifetime},
    matching::Match,
    predicate::{self, Outlives},
    sub_term::{SubLifetimeLocation, SubTypeLocation, TermLocation},
    r#type::Type,
    visitor,
};

use crate::{
    Succeeded,
    environment::{BoxedFuture, Environment, Query, QueryResult},
    equality::Equality,
    lifetime_constraint::LifetimeConstraint,
    normalizer::Normalizer,
    term::Term,
    variance::get_variance_of,
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NotGeneralEnoughLifetimeError {
    /// The forall lifetime found on the `self` side.
    pub forall_lifetime: Forall,

    /// The non-forall lifetime found on the `target` side.
    pub lifetime: Lifetime,
}

/// The forall lifetime on the `source` side can be matched with only exactly
/// one forall lifetime on the `target` side (including normal lifetimes).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
            *term = instantiated.clone();
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

impl visitor::MutableRecursive<Instance>
    for ForallLifetimeInstantiationVisitor<'_>
{
    fn visit(
        &mut self,
        _: &mut Instance,
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    type InProgress = ();
    type Result = Option<Arc<Succeeded<Subtypable>>>;

    fn query<'x, N: Normalizer>(
        &'x self,
        environment: &'x Environment<'x, N>,
        (): Self::InProgress,
    ) -> BoxedFuture<'x, Self::Result> {
        Box::pin(Impl::query(self, environment))
    }
}

#[doc(hidden)]
pub trait Impl: Sized {
    fn query(
        subtype: &Subtype<Self>,
        environment: &Environment<impl Normalizer>,
    ) -> impl std::future::Future<
        Output = QueryResult<Option<Arc<Succeeded<Subtypable>>>>,
    > + Send;
}

impl Impl for Lifetime {
    async fn query(
        subtype: &Subtype<Self>,
        _: &Environment<'_, impl Normalizer>,
    ) -> QueryResult<Option<Arc<Succeeded<Subtypable>>>> {
        if subtype.source == subtype.target {
            return Ok(Some(Arc::new(Succeeded::new(Subtypable::default()))));
        }

        match (subtype.target.clone(), subtype.source.clone()) {
            (self_lifetime, Self::Forall(forall_source)) => {
                let mut subtypable = Subtypable::default();

                assert!(
                    subtypable
                        .forall_lifetime_instantiations
                        .lifetimes_by_forall
                        .insert(forall_source, self_lifetime)
                        .is_none()
                );

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
            Variance::Covariant => std::iter::once(
                LifetimeConstraint::LifetimeOutlives(Outlives {
                    operand: subtype.target.clone(),
                    bound: subtype.source.clone(),
                }),
            )
            .collect(),
            Variance::Contravariant => std::iter::once(
                LifetimeConstraint::LifetimeOutlives(Outlives {
                    operand: subtype.source.clone(),
                    bound: subtype.target.clone(),
                }),
            )
            .collect(),
            Variance::Bivariant => BTreeSet::new(),
            Variance::Invariant => [
                LifetimeConstraint::LifetimeOutlives(Outlives::new(
                    subtype.target.clone(),
                    subtype.source.clone(),
                )),
                LifetimeConstraint::LifetimeOutlives(Outlives::new(
                    subtype.source.clone(),
                    subtype.target.clone(),
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
        .extend(new.result.forall_lifetime_errors.iter().cloned());

    // try to compose a new forall lifetime instantiation
    for (from, to) in new
        .result
        .forall_lifetime_instantiations
        .lifetimes_by_forall
        .iter()
        .map(|(from, to)| (from.clone(), to.clone()))
    {
        match current
            .result
            .forall_lifetime_instantiations
            .lifetimes_by_forall
            .entry(from.clone())
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
                            to.clone(),
                            occupied_entry.get().clone(),
                        )),
                    );
                    current.constraints.insert(
                        LifetimeConstraint::LifetimeOutlives(Outlives::new(
                            occupied_entry.get().clone(),
                            to,
                        )),
                    );
                }
            }
        }
    }
}

async fn subtypable_with_normalization<T: Term>(
    target: &T,
    source: &T,
    current_variance: Variance,
    environment: &Environment<'_, impl Normalizer>,
) -> QueryResult<Option<Succeeded<Subtypable>>> {
    if let Some(Succeeded { result: eq, mut constraints }) =
        target.normalize(environment).await?
        && let Some(result) = environment
            .query(&Subtype::new(eq, source.clone(), current_variance))
            .await?
    {
        constraints.extend(result.constraints.iter().cloned());
        return Ok(Some(Succeeded::with_constraints(
            result.result.clone(),
            constraints,
        )));
    }

    if let Some(Succeeded { result: eq, mut constraints }) =
        source.normalize(environment).await?
        && let Some(result) = environment
            .query(&Subtype::new(target.clone(), eq, current_variance))
            .await?
    {
        constraints.extend(result.constraints.iter().cloned());
        return Ok(Some(Succeeded::with_constraints(
            result.result.clone(),
            constraints,
        )));
    }

    Ok(None)
}

// Matches each of the inner term one by one and then recursively call subtype
// query with the variance correctly transformed.
async fn subtypable_with_substructural_type(
    target: &Type,
    source: &Type,
    current_variance: Variance,
    environment: &Environment<'_, impl Normalizer>,
) -> QueryResult<Option<Succeeded<Subtypable>>> {
    let Some(matching) = target.substructural_match(source) else {
        return Ok(None);
    };

    let mut result = Succeeded::new(Subtypable::default());

    let (lifetimes, types, constants, instances) = matching.destructure();

    // normally, the `lhs_location` and `rhs_location` are the same except when
    // matching the tuple packing range

    for matching in lifetimes {
        let (target_lt, source_lt, source_location, _) = matching.destructure();

        let inner_variance = environment
            .tracked_engine()
            .get_variance_of(
                target,
                current_variance,
                std::iter::once(TermLocation::Lifetime(
                    SubLifetimeLocation::FromType(source_location),
                )),
            )
            .await;

        let Some(new_result) = environment
            .query(&Subtype::new(target_lt, source_lt, inner_variance))
            .await?
        else {
            return Ok(None);
        };

        merge_result(&mut result, &new_result);
    }

    for matching in types {
        let (target_ty, source_ty, source_location, _) = matching.destructure();

        let inner_variance = environment
            .tracked_engine()
            .get_variance_of(
                target,
                current_variance,
                std::iter::once(TermLocation::Type(SubTypeLocation::FromType(
                    source_location,
                ))),
            )
            .await;

        let Some(new_result) = environment
            .query(&Subtype::new(target_ty, source_ty, inner_variance))
            .await?
        else {
            return Ok(None);
        };

        merge_result(&mut result, &new_result);
    }

    for matching in constants {
        let (target_const, source_const, _, _) = matching.destructure();

        // constant shouldn't have variance influenced, as of now, we'll simply
        // propagate the current variance down
        let Some(new_result) = environment
            .query(&Subtype::new(target_const, source_const, current_variance))
            .await?
        else {
            return Ok(None);
        };

        merge_result(&mut result, &new_result);
    }

    for matching in instances {
        let (target_inst, source_inst, _, _) = matching.destructure();

        let Some(new_result) = environment
            // the lifetimes in instances should match exactly
            .query(&Subtype::new(target_inst, source_inst, Variance::Invariant))
            .await?
        else {
            return Ok(None);
        };

        merge_result(&mut result, &new_result);
    }

    Ok(Some(result))
}

async fn subtypable_without_mapping_type(
    target: &Type,
    source: &Type,
    variance: Variance,
    environment: &Environment<'_, impl Normalizer>,
) -> QueryResult<Option<Succeeded<Subtypable>>> {
    // trivially satisfied
    if target == source {
        return Ok(Some(Succeeded::new(Subtypable::default())));
    }

    if let Some(result) = subtypable_with_substructural_type(
        target,
        source,
        variance,
        environment,
    )
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
    predicate: &predicate::Compatible<InstanceAssociated, Type>,
    environment: &Environment<'_, impl Normalizer>,
) -> QueryResult<Option<Arc<Succeeded<Subtypable>>>> {
    if first.as_instance_associated().is_none() {
        return Ok(None);
    }

    let instance_associated_term =
        Type::InstanceAssociated(predicate.lhs.clone());

    let Some(mut result) = subtypable_without_mapping_type(
        first,
        &instance_associated_term,
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
    ) -> QueryResult<Option<Arc<Succeeded<Subtypable>>>> {
        if let Some(result) = subtypable_without_mapping_type(
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
        if !subtype.source.is_instance_associated()
            && !subtype.target.is_instance_associated()
        {
            return Ok(None);
        }

        // looking at the premise
        for predicate in environment
            .premise()
            .predicates
            .iter()
            .filter_map(|x| x.as_instance_associated_type_equality())
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
    ) -> QueryResult<Option<Arc<Succeeded<Subtypable>>>> {
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
    ) -> QueryResult<Option<Arc<Succeeded<Subtypable>>>> {
        self.query(&Subtype::new(target, source, variance)).await
    }

    /// Determines the sub-typing relationship between two generic arguments.
    /// This function assumes [`Variance::Invariant`]
    pub async fn subtypes_generic_arguments(
        &self,
        target: &GenericArguments,
        source: &GenericArguments,
    ) -> QueryResult<Option<Succeeded<Subtypable>>> {
        assert!(
            target.arity_matches(source),
            "the arity of the generic arguments should match"
        );

        let mut result = Succeeded::with_constraints(
            Subtypable::default(),
            BTreeSet::default(),
        );

        for (target_lt, source_lt) in
            source.lifetimes().iter().zip(target.lifetimes().iter())
        {
            let Some(new_result) = self
                .query(&Subtype::new(
                    source_lt.clone(),
                    target_lt.clone(),
                    Variance::Invariant,
                ))
                .await?
            else {
                return Ok(None);
            };

            merge_result(&mut result, &new_result);
        }

        for (target_ty, source_ty) in
            source.types().iter().zip(target.types().iter())
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
            source.constants().iter().zip(target.constants().iter())
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

        for (target_inst, source_inst) in
            source.instances().iter().zip(target.instances().iter())
        {
            let Some(new_result) = self
                .query(&Subtype::new(
                    source_inst.clone(),
                    target_inst.clone(),
                    // The lifetime in the instances should match exactly
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

// Matches each of the inner term one by one and then recursively call subtype
// query with the variance correctly transformed.
async fn subtypable_with_substructural_instance(
    target: &Instance,
    source: &Instance,
    current_variance: Variance,
    environment: &Environment<'_, impl Normalizer>,
) -> QueryResult<Option<Succeeded<Subtypable>>> {
    let Some(matching) = target.substructural_match(source) else {
        return Ok(None);
    };

    let mut result = Succeeded::new(Subtypable::default());

    let (lifetimes, types, constants, instances) = matching.destructure();

    // normally, the `lhs_location` and `rhs_location` are the same except when
    // matching the tuple packing range

    for matching in lifetimes {
        let (target_lt, source_lt, _, _) = matching.destructure();

        let Some(new_result) = environment
            .query(&Subtype::new(
                target_lt,
                source_lt,
                current_variance.xfrom(Variance::Invariant),
            ))
            .await?
        else {
            return Ok(None);
        };

        merge_result(&mut result, &new_result);
    }

    for matching in types {
        let (target_ty, source_ty, _, _) = matching.destructure();

        let Some(new_result) = environment
            .query(&Subtype::new(
                target_ty,
                source_ty,
                current_variance.xfrom(Variance::Invariant),
            ))
            .await?
        else {
            return Ok(None);
        };

        merge_result(&mut result, &new_result);
    }

    for matching in constants {
        let (target_const, source_const, _, _) = matching.destructure();

        // constant shouldn't have variance influenced, as of now, we'll simply
        // propagate the current variance down
        let Some(new_result) = environment
            .query(&Subtype::new(
                target_const,
                source_const,
                current_variance.xfrom(Variance::Invariant),
            ))
            .await?
        else {
            return Ok(None);
        };

        merge_result(&mut result, &new_result);
    }

    for matching in instances {
        let (target_inst, source_inst, _, _) = matching.destructure();

        let Some(new_result) = environment
            // the lifetimes in instances should match exactly
            .query(&Subtype::new(
                target_inst,
                source_inst,
                current_variance.xfrom(Variance::Invariant),
            ))
            .await?
        else {
            return Ok(None);
        };

        merge_result(&mut result, &new_result);
    }

    Ok(Some(result))
}

impl Impl for Instance {
    async fn query(
        subtype: &Subtype<Self>,
        environment: &Environment<'_, impl Normalizer>,
    ) -> QueryResult<Option<Arc<Succeeded<Subtypable>>>> {
        // trivially satisfied
        if subtype.target == subtype.source {
            return Ok(Some(Arc::new(Succeeded::new(Subtypable::default()))));
        }

        if let Some(result) = subtypable_with_substructural_instance(
            &subtype.target,
            &subtype.source,
            subtype.variance,
            environment,
        )
        .await?
        {
            return Ok(Some(Arc::new(result)));
        }

        if let Some(result) = subtypable_with_normalization(
            &subtype.target,
            &subtype.source,
            subtype.variance,
            environment,
        )
        .await?
        {
            return Ok(Some(Arc::new(result)));
        }

        Ok(None)
    }
}

#[cfg(test)]
mod test;
