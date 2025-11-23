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
    lifetime::Lifetime,
    matching,
    sub_term::{SubTerm, TermLocation},
    r#type::Type,
};

use crate::{
    Error, Satisfied, Succeeded,
    environment::{BoxedFuture, Environment, Query},
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
        from_logs: &[Log],
        to_logs: &[Log],
    ) -> Result<Option<Succeeded<Satisfied>>, Error>;
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
        from_logs: &[Log],
        to_logs: &[Log],
        predicate: &(
             impl Predicate<Lifetime> + Predicate<Type> + Predicate<Constant>
         ),
    ) -> Result<Option<Succeeded<Satisfied>>, Error>;

    /// Converts the term into a [`Log`] record as a rewitten term.
    fn into_rewritten(self) -> Log;
}

impl Element for Lifetime {
    fn unifiable(
        from: &Self,
        to: &Self,
        from_logs: &[Log],
        to_logs: &[Log],
        predicate: &(impl Predicate<Self> + Predicate<Type> + Predicate<Constant>),
    ) -> Result<Option<Succeeded<Satisfied>>, Error> {
        predicate.unifiable(from, to, from_logs, to_logs)
    }

    fn into_rewritten(self) -> Log { Log::RewrittenLifetime(self) }
}

impl Element for Type {
    fn unifiable(
        from: &Self,
        to: &Self,
        from_logs: &[Log],
        to_logs: &[Log],
        predicate: &(
             impl Predicate<Lifetime> + Predicate<Self> + Predicate<Constant>
         ),
    ) -> Result<Option<Succeeded<Satisfied>>, Error> {
        predicate.unifiable(from, to, from_logs, to_logs)
    }

    fn into_rewritten(self) -> Log { Log::RewrittenType(self) }
}

impl Element for Constant {
    fn unifiable(
        from: &Self,
        to: &Self,
        from_logs: &[Log],
        to_logs: &[Log],
        predicate: &(impl Predicate<Lifetime> + Predicate<Type> + Predicate<Self>),
    ) -> Result<Option<Succeeded<Satisfied>>, Error> {
        predicate.unifiable(from, to, from_logs, to_logs)
    }

    fn into_rewritten(self) -> Log { Log::RewrittenConstant(self) }
}

/// Contains all the unification of substructural components.
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(missing_docs)]
pub struct Substructural<T: SubTerm> {
    pub lifetimes: BTreeMap<T::SubLifetimeLocation, Unifier<Lifetime>>,
    pub types: BTreeMap<T::SubTypeLocation, Unifier<Type>>,
    pub constants: BTreeMap<T::SubConstantLocation, Unifier<Constant>>,
}

impl<T: SubTerm> Default for Substructural<T> {
    fn default() -> Self {
        Self {
            lifetimes: BTreeMap::new(),
            types: BTreeMap::new(),
            constants: BTreeMap::new(),
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
    /// If the `lhs` has been rewritten into another form, this field will be
    /// `Some` of the rewritten term.
    ///
    /// The rewritten term can occur from normalizing the term or using trait
    /// member equivalences.
    pub rewritten_from: Option<T>,

    /// If the `rhs` has been rewritten into another form, this field will be
    /// `Some` of the rewritten term.
    ///
    /// The rewritten term can occur from normalizing the term or using trait
    /// member equivalences.
    pub rewritten_right: Option<T>,

    /// The unification of the `lhs` and `rhs` terms.
    pub matching: Matching<T>,
}

/// A super trait for the predicate object that can be used as a part of the
/// query.
pub trait PredicateA:
    Predicate<Lifetime>
    + Predicate<Type>
    + Predicate<Constant>
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
    type Parameter =
        (Vec<Log> /* from logs */, Vec<Log> /* to logs */);
    type InProgress = ();
    type Result = Succeeded<Unifier<T>>;
    type Error = Error;

    fn query<'x, N: Normalizer>(
        &'x self,
        environment: &'x Environment<'x, N>,
        (from_logs, to_logs): Self::Parameter,
        (): Self::InProgress,
    ) -> BoxedFuture<'x, Self::Result, Self::Error> {
        Box::pin(async move {
            unify(
                &self.from,
                &self.to,
                &from_logs,
                &to_logs,
                &self.predicate,
                environment,
            )
            .await
        })
    }
}

async fn substructural_unify<T: Term>(
    from: &T,
    to: &T,
    from_logs: &[Log],
    to_logs: &[Log],
    predicate: &impl PredicateA,
    environment: &Environment<'_, impl Normalizer>,
) -> crate::Result<Unifier<T>> {
    let Some(substructural) = from.substructural_match(to) else {
        return Ok(None);
    };

    let mut result = Substructural::default();
    let mut constraints = BTreeSet::new();

    for matching::Matching {
        lhs: from,
        rhs: to,
        lhs_location: from_location,
        rhs_location: to_location,
    } in substructural.lifetimes
    {
        let mut from_logs = from_logs.to_vec();
        from_logs.push(Log::Substructural(from_location.into()));

        let mut to_logs = to_logs.to_vec();
        to_logs.push(Log::Substructural(to_location.into()));

        let Some(new) = environment
            .query_with(
                &Unification::new(from, to, predicate.clone()),
                (from_logs, to_logs),
                (),
            )
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

    for matching::Matching {
        lhs: from,
        rhs: to,
        lhs_location: from_location,
        rhs_location: to_location,
    } in substructural.types
    {
        let mut from_logs = from_logs.to_vec();
        from_logs.push(Log::Substructural(from_location.into()));

        let mut to_logs = to_logs.to_vec();
        to_logs.push(Log::Substructural(to_location.into()));

        let Some(new) = environment
            .query_with(
                &Unification::new(from.clone(), to.clone(), predicate.clone()),
                (from_logs, to_logs),
                (),
            )
            .await?
        else {
            return Ok(None);
        };

        constraints.extend(new.constraints.iter().cloned());
        assert!(
            result.types.insert(from_location, new.result.clone()).is_none()
        );
    }

    for matching::Matching {
        lhs: from,
        rhs: to,
        lhs_location: from_location,
        rhs_location: to_location,
    } in substructural.constants
    {
        let mut from_logs = from_logs.to_vec();
        from_logs.push(Log::Substructural(from_location.into()));

        let mut to_logs = to_logs.to_vec();
        to_logs.push(Log::Substructural(to_location.into()));

        let Some(new) = environment
            .query_with(
                &Unification::new(from.clone(), to.clone(), predicate.clone()),
                (from_logs, to_logs),
                (),
            )
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

    Ok(Some(Succeeded::with_constraints(
        Unifier {
            rewritten_from: None,
            rewritten_right: None,
            matching: Matching::Substructural(result),
        },
        constraints,
    )))
}

pub(super) async fn unify<T: Term>(
    from: &T,
    to: &T,
    from_logs: &[Log],
    to_logs: &[Log],
    predicate: &impl PredicateA,
    environment: &Environment<'_, impl Normalizer>,
) -> crate::ResultArc<Unifier<T>> {
    if let Some(result) =
        environment.query(&Equality::new(from.clone(), to.clone())).await?
    {
        return Ok(Some(Arc::new(Succeeded::with_constraints(
            Unifier {
                rewritten_from: None,
                rewritten_right: None,
                matching: Matching::Equality,
            },
            result.constraints.clone(),
        ))));
    }

    // check if the predicate can unify the two terms
    if let Some(constraint) =
        T::unifiable(from, to, from_logs, to_logs, predicate)?
    {
        return Ok(Some(Arc::new(Succeeded::with_constraints(
            Unifier {
                rewritten_from: None,
                rewritten_right: None,
                matching: Matching::Unifiable(from.clone(), to.clone()),
            },
            constraint.constraints,
        ))));
    }

    if let Some(unification) = substructural_unify(
        from,
        to,
        from_logs,
        to_logs,
        predicate,
        environment,
    )
    .await?
    {
        return Ok(Some(Arc::new(unification)));
    }

    // try to look for equivalences
    for Succeeded { result: eq_from, constraints } in
        environment.get_equivalences(from).await?.iter()
    {
        let mut from_logs = from_logs.to_vec();
        from_logs.push(eq_from.clone().into_rewritten());

        if let Some(unifier_result) = environment
            .query_with(
                &Unification::new(
                    eq_from.clone(),
                    to.clone(),
                    predicate.clone(),
                ),
                (from_logs, to_logs.to_vec()),
                (),
            )
            .await?
        {
            let mut unifier = unifier_result.result.clone();
            unifier.rewritten_from =
                unifier.rewritten_from.or(Some(eq_from.clone()));

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
        let mut to_logs = to_logs.to_vec();
        to_logs.push(eq_to.clone().into_rewritten());

        if let Some(unifier_result) = environment
            .query_with(
                &Unification::new(
                    from.clone(),
                    eq_to.clone(),
                    predicate.clone(),
                ),
                (from_logs.to_vec(), to_logs),
                (),
            )
            .await?
        {
            let mut unifier = unifier_result.result.clone();
            unifier.rewritten_right =
                unifier.rewritten_right.or(Some(eq_to.clone()));

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
