//! Contains the the unification logic.

use std::{
    collections::{BTreeMap, BTreeSet},
    hash::Hash,
    sync::Arc,
};

use derive_new::new;
use enum_as_inner::EnumAsInner;
use pernixc_term::{
    constant::Constant,
    instance::Instance,
    lifetime::Lifetime,
    sub_term::{SubTerm, TermLocation},
    r#type::Type,
};

use crate::{
    Satisfied, Succeeded,
    environment::{BoxedFuture, Environment, Query, QueryResult},
    equality::Equality,
    normalizer::Normalizer,
    term::Term,
};

/// A query for performing unification.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, new)]
#[allow(missing_docs)]
pub struct Unification<T, P> {
    pub from: T,
    pub to: T,
    pub predicate: P,
}

/// An object used for determining if two terms are unifiable.
pub trait Predicate<T> {
    /// Determines if the two given terms are unifiable.
    ///
    /// # Errors
    ///
    /// See [`type_system::Error`] for more information.
    fn unifiable(
        &self,
        from: &T,
        to: &T,
    ) -> QueryResult<Option<Succeeded<Satisfied>>>;
}

/// A trait implemented by terms that can be unified.
pub trait Element {
    /// Accepts a predicate and determines if the two terms are unifiable.
    ///
    /// # Errors
    ///
    /// See [`AbruptError`] for more information.
    fn unifiable(
        from: &Self,
        to: &Self,
        predicate: &(
             impl Predicate<Lifetime>
             + Predicate<Type>
             + Predicate<Constant>
             + Predicate<Instance>
         ),
    ) -> QueryResult<Option<Succeeded<Satisfied>>>;
}

impl Element for Lifetime {
    fn unifiable(
        from: &Self,
        to: &Self,
        predicate: &(
             impl Predicate<Self>
             + Predicate<Type>
             + Predicate<Constant>
             + Predicate<Instance>
         ),
    ) -> QueryResult<Option<Succeeded<Satisfied>>> {
        predicate.unifiable(from, to)
    }
}

impl Element for Type {
    fn unifiable(
        from: &Self,
        to: &Self,
        predicate: &(
             impl Predicate<Lifetime>
             + Predicate<Self>
             + Predicate<Constant>
             + Predicate<Instance>
         ),
    ) -> QueryResult<Option<Succeeded<Satisfied>>> {
        predicate.unifiable(from, to)
    }
}

impl Element for Constant {
    fn unifiable(
        from: &Self,
        to: &Self,
        predicate: &(
             impl Predicate<Lifetime>
             + Predicate<Type>
             + Predicate<Self>
             + Predicate<Instance>
         ),
    ) -> QueryResult<Option<Succeeded<Satisfied>>> {
        predicate.unifiable(from, to)
    }
}

impl Element for Instance {
    fn unifiable(
        from: &Self,
        to: &Self,
        predicate: &(
             impl Predicate<Lifetime>
             + Predicate<Type>
             + Predicate<Constant>
             + Predicate<Self>
         ),
    ) -> QueryResult<Option<Succeeded<Satisfied>>> {
        predicate.unifiable(from, to)
    }
}

/// Contains all the unification of substructural components.
#[derive(Debug, Clone, PartialEq, Eq, new)]
#[allow(missing_docs)]
pub struct Substructural<T: SubTerm> {
    lifetimes: BTreeMap<T::SubLifetimeLocation, Unifier<Lifetime>>,
    types: BTreeMap<T::SubTypeLocation, Unifier<Type>>,
    constants: BTreeMap<T::SubConstantLocation, Unifier<Constant>>,
    instances: BTreeMap<T::SubInstanceLocation, Unifier<Instance>>,
}

impl<T: SubTerm> Substructural<T> {
    /// Destructures the substructural unification into its components.
    #[must_use]
    #[allow(clippy::type_complexity)]
    pub fn destructure(
        self,
    ) -> (
        BTreeMap<T::SubLifetimeLocation, Unifier<Lifetime>>,
        BTreeMap<T::SubTypeLocation, Unifier<Type>>,
        BTreeMap<T::SubConstantLocation, Unifier<Constant>>,
        BTreeMap<T::SubInstanceLocation, Unifier<Instance>>,
    ) {
        (self.lifetimes, self.types, self.constants, self.instances)
    }
}

impl<T: SubTerm> Default for Substructural<T> {
    fn default() -> Self {
        Self {
            lifetimes: BTreeMap::new(),
            types: BTreeMap::new(),
            constants: BTreeMap::new(),
            instances: BTreeMap::new(),
        }
    }
}

/// Represents the record tracking of matching/modification made to a term to
/// unify it with another term.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Log {
    Substructural(TermLocation),
    RewrittenLifetime(Lifetime),
    RewrittenType(Type),
    RewrittenConstant(Constant),
}

/// Specifies how a term matches another term.
#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
pub enum Matching<T: SubTerm> {
    /// The two terms are unified by passing the predicate check in the
    /// [`Predicate`].
    Unifiable(T, T),

    /// The two terms are substructural and can be unified by unifying their
    /// substructural components.
    Substructural(Substructural<T>),

    /// The two terms are equal, no unification is needed.
    Equality,
}

/// Represents a unification between two terms.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Unifier<T: SubTerm> {
    /// The rewritten left-hand side term after unification.
    pub rewritten_from: Option<T>,

    /// The rewritten right-hand side term after unification.
    pub rewritten_to: Option<T>,

    /// The unification of the `lhs` and `rhs` terms.
    pub matching: Matching<T>,
}

/// A super trait for the predicate object that can be used as a part of the
/// query.
pub trait PredicateA:
    Predicate<Lifetime>
    + Predicate<Type>
    + Predicate<Constant>
    + Predicate<Instance>
    + Hash
    + Ord
    + Clone
    + Send
    + Sync
    + 'static
{
}

impl<
    T: Predicate<Lifetime>
        + Predicate<Type>
        + Predicate<Constant>
        + Predicate<Instance>
        + Hash
        + Ord
        + Clone
        + Send
        + Sync
        + 'static,
> PredicateA for T
{
}

impl<T: Term, P: PredicateA> Query for Unification<T, P> {
    type InProgress = ();
    type Result = Option<Arc<Succeeded<Unifier<T>>>>;

    fn query<'x, N: Normalizer>(
        &'x self,
        environment: &'x Environment<'x, N>,
        (): Self::InProgress,
    ) -> BoxedFuture<'x, Self::Result> {
        Box::pin(async move {
            unify(&self.from, &self.to, &self.predicate, environment).await
        })
    }

    fn on_cyclic(
        &self,
        _: Self::InProgress,
        _: Self::InProgress,
        _: &[crate::environment::Call<
            crate::environment::DynArc,
            crate::environment::DynArc,
        >],
    ) -> Self::Result {
        None
    }
}

async fn substructural_unify<T: Term>(
    from: &T,
    to: &T,
    predicate: &impl PredicateA,
    environment: &Environment<'_, impl Normalizer>,
) -> QueryResult<Option<Succeeded<Unifier<T>>>> {
    let Some(substructural) = from.substructural_match(to) else {
        return Ok(None);
    };

    let mut result = Substructural::default();
    let mut constraints = BTreeSet::new();

    let (lifetimes, types, constants, instances) = substructural.destructure();

    for matching in lifetimes {
        let (from, to, from_location, _) = matching.destructure();

        let Some(new) = environment
            .query_with(&Unification::new(from, to, predicate.clone()), ())
            .await?
        else {
            return Ok(None);
        };

        constraints.extend(new.constraints.iter().cloned());

        assert!(
            result
                .lifetimes
                .insert(from_location, new.result.clone())
                .is_none()
        );
    }

    for matching in types {
        let (from, to, from_location, _) = matching.destructure();

        let Some(new) = environment
            .query_with(&Unification::new(from, to, predicate.clone()), ())
            .await?
        else {
            return Ok(None);
        };

        constraints.extend(new.constraints.iter().cloned());
        assert!(
            result.types.insert(from_location, new.result.clone()).is_none()
        );
    }

    for matching in constants {
        let (from, to, from_location, _) = matching.destructure();

        let Some(new) = environment
            .query_with(&Unification::new(from, to, predicate.clone()), ())
            .await?
        else {
            return Ok(None);
        };

        constraints.extend(new.constraints.iter().cloned());
        assert!(
            result
                .constants
                .insert(from_location, new.result.clone())
                .is_none()
        );
    }

    for matching in instances {
        let (from, to, from_location, _) = matching.destructure();

        let Some(new) = environment
            .query_with(&Unification::new(from, to, predicate.clone()), ())
            .await?
        else {
            return Ok(None);
        };

        constraints.extend(new.constraints.iter().cloned());
        assert!(
            result
                .instances
                .insert(from_location, new.result.clone())
                .is_none()
        );
    }

    Ok(Some(Succeeded::with_constraints(
        Unifier {
            matching: Matching::Substructural(result),
            rewritten_from: None,
            rewritten_to: None,
        },
        constraints,
    )))
}

pub(super) async fn unify<T: Term>(
    from: &T,
    to: &T,
    predicate: &impl PredicateA,
    environment: &Environment<'_, impl Normalizer>,
) -> QueryResult<Option<Arc<Succeeded<Unifier<T>>>>> {
    if let Some(result) =
        environment.query(&Equality::new(from.clone(), to.clone())).await?
    {
        return Ok(Some(Arc::new(Succeeded::with_constraints(
            Unifier {
                matching: Matching::Equality,
                rewritten_from: None,
                rewritten_to: None,
            },
            result.constraints.clone(),
        ))));
    }

    // check if the predicate can unify the two terms
    if let Some(constraint) = T::unifiable(from, to, predicate)? {
        return Ok(Some(Arc::new(Succeeded::with_constraints(
            Unifier {
                matching: Matching::Unifiable(from.clone(), to.clone()),
                rewritten_from: None,
                rewritten_to: None,
            },
            constraint.constraints,
        ))));
    }

    if let Some(unification) =
        substructural_unify(from, to, predicate, environment).await?
    {
        return Ok(Some(Arc::new(unification)));
    }

    // try to look for equivalences
    for Succeeded { result: eq_from, constraints } in
        environment.get_equivalences(from).await?.iter()
    {
        if let Some(unifier_result) = environment
            .query_with(
                &Unification::new(
                    eq_from.clone(),
                    to.clone(),
                    predicate.clone(),
                ),
                (),
            )
            .await?
        {
            let mut unifier = unifier_result.result.clone();
            unifier.rewritten_from =
                unifier.rewritten_from.or_else(|| Some(eq_from.clone()));

            return Ok(Some(Arc::new(Succeeded::with_constraints(
                unifier,
                constraints
                    .iter()
                    .cloned()
                    .chain(unifier_result.constraints.iter().cloned())
                    .collect(),
            ))));
        }
    }
    for Succeeded { result: eq_to, constraints } in
        environment.get_equivalences(to).await?.iter()
    {
        if let Some(unifier_result) = environment
            .query_with(
                &Unification::new(
                    from.clone(),
                    eq_to.clone(),
                    predicate.clone(),
                ),
                (),
            )
            .await?
        {
            let mut unifier = unifier_result.result.clone();
            unifier.rewritten_to =
                unifier.rewritten_to.or_else(|| Some(eq_to.clone()));

            return Ok(Some(Arc::new(Succeeded::with_constraints(
                unifier,
                constraints
                    .iter()
                    .cloned()
                    .chain(unifier_result.constraints.iter().cloned())
                    .collect(),
            ))));
        }
    }

    Ok(None)
}

#[cfg(test)]
mod test;
