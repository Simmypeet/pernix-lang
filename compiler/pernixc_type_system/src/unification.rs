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
    r#type::Type,
    sub_term::{SubTerm, TermLocation},
    Model, ModelOf,
};

use crate::{
    environment::{Environment, Query},
    equality::Equality,
    normalizer::Normalizer,
    term::Term,
    AbruptError, Satisfied, Succeeded,
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
pub trait Predicate<T: ModelOf> {
    /// Determines if the two given terms are unifiable.
    ///
    /// # Errors
    ///
    /// See [`type_system::Error`] for more information.
    fn unifiable(
        &self,
        from: &T,
        to: &T,
        from_logs: &[Log<T::Model>],
        to_logs: &[Log<T::Model>],
    ) -> Result<Option<Succeeded<Satisfied, T::Model>>, AbruptError>;
}

/// A trait implemented by terms that can be unified.
pub trait Element: ModelOf {
    /// Accepts a predicate and determines if the two terms are unifiable.
    ///
    /// # Errors
    ///
    /// See [`AbruptError`] for more information.
    fn unifiable(
        from: &Self,
        to: &Self,
        from_logs: &[Log<Self::Model>],
        to_logs: &[Log<Self::Model>],
        predicate: &(impl Predicate<Lifetime<Self::Model>>
              + Predicate<Type<Self::Model>>
              + Predicate<Constant<Self::Model>>),
    ) -> Result<Option<Succeeded<Satisfied, Self::Model>>, AbruptError>;

    /// Converts the term into a [`Log`] record as a rewitten term.
    fn into_rewritten(self) -> Log<Self::Model>;
}

impl<M: Model> Element for Lifetime<M> {
    fn unifiable(
        from: &Self,
        to: &Self,
        from_logs: &[Log<M>],
        to_logs: &[Log<M>],
        predicate: &(impl Predicate<Lifetime<Self::Model>>
              + Predicate<Type<Self::Model>>
              + Predicate<Constant<Self::Model>>),
    ) -> Result<Option<Succeeded<Satisfied, M>>, AbruptError> {
        predicate.unifiable(from, to, from_logs, to_logs)
    }

    fn into_rewritten(self) -> Log<M> { Log::RewrittenLifetime(self) }
}

impl<M: Model> Element for Type<M> {
    fn unifiable(
        from: &Self,
        to: &Self,
        from_logs: &[Log<M>],
        to_logs: &[Log<M>],
        predicate: &(impl Predicate<Lifetime<Self::Model>>
              + Predicate<Type<Self::Model>>
              + Predicate<Constant<Self::Model>>),
    ) -> Result<Option<Succeeded<Satisfied, M>>, AbruptError> {
        predicate.unifiable(from, to, from_logs, to_logs)
    }

    fn into_rewritten(self) -> Log<M> { Log::RewrittenType(self) }
}

impl<M: Model> Element for Constant<M> {
    fn unifiable(
        from: &Self,
        to: &Self,
        from_logs: &[Log<M>],
        to_logs: &[Log<M>],
        predicate: &(impl Predicate<Lifetime<Self::Model>>
              + Predicate<Type<Self::Model>>
              + Predicate<Constant<Self::Model>>),
    ) -> Result<Option<Succeeded<Satisfied, M>>, AbruptError> {
        predicate.unifiable(from, to, from_logs, to_logs)
    }

    fn into_rewritten(self) -> Log<M> { Log::RewrittenConstant(self) }
}

/// Contains all the unification of substructural components.
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(missing_docs)]
pub struct Substructural<T: SubTerm> {
    pub lifetimes:
        BTreeMap<T::SubLifetimeLocation, Unifier<Lifetime<T::Model>>>,
    pub types: BTreeMap<T::SubTypeLocation, Unifier<Type<T::Model>>>,
    pub constants:
        BTreeMap<T::SubConstantLocation, Unifier<Constant<T::Model>>>,
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
pub enum Log<M: Model> {
    Substructural(TermLocation),
    RewrittenLifetime(Lifetime<M>),
    RewrittenType(Type<M>),
    RewrittenConstant(Constant<M>),
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
pub trait PredicateA<M: Model>:
    Predicate<Lifetime<M>>
    + Predicate<Type<M>>
    + Predicate<Constant<M>>
    + Hash
    + Ord
    + Clone
    + Send
    + Sync
    + 'static
{
}

impl<
        M: Model,
        T: Predicate<Lifetime<M>>
            + Predicate<Type<M>>
            + Predicate<Constant<M>>
            + Hash
            + Ord
            + Clone
            + Send
            + Sync
            + 'static,
    > PredicateA<M> for T
{
}

impl<T: Term, P: PredicateA<T::Model>> Query for Unification<T, P> {
    type Model = T::Model;
    type Parameter = (
        Vec<Log<T::Model>>, /* from logs */
        Vec<Log<T::Model>>, /* to logs */
    );
    type InProgress = ();
    type Result = Succeeded<Unifier<T>, T::Model>;
    type Error = AbruptError;

    fn query(
        &self,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
        (from_logs, to_logs): Self::Parameter,
        (): Self::InProgress,
    ) -> Result<Option<Arc<Self::Result>>, Self::Error> {
        unify(
            &self.from,
            &self.to,
            &from_logs,
            &to_logs,
            &self.predicate,
            environment,
        )
    }
}

fn substructural_unify<T: Term>(
    from: &T,
    to: &T,
    from_logs: &[Log<T::Model>],
    to_logs: &[Log<T::Model>],
    predicate: &impl PredicateA<T::Model>,
    environment: &Environment<T::Model, impl Normalizer<T::Model>>,
) -> crate::Result<Unifier<T>, T::Model> {
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

        let Some(new) = environment.query_with(
            &Unification::new(from.clone(), to.clone(), predicate.clone()),
            (from_logs, to_logs),
            (),
        )?
        else {
            return Ok(None);
        };

        constraints.extend(new.constraints.iter().cloned());
        assert!(result
            .lifetimes
            .insert(from_location, new.result.clone())
            .is_none());
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

        let Some(new) = environment.query_with(
            &Unification::new(from.clone(), to.clone(), predicate.clone()),
            (from_logs, to_logs),
            (),
        )?
        else {
            return Ok(None);
        };

        constraints.extend(new.constraints.iter().cloned());
        assert!(result
            .types
            .insert(from_location, new.result.clone())
            .is_none());
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

        let Some(new) = environment.query_with(
            &Unification::new(from.clone(), to.clone(), predicate.clone()),
            (from_logs, to_logs),
            (),
        )?
        else {
            return Ok(None);
        };

        constraints.extend(new.constraints.iter().cloned());
        assert!(result
            .constants
            .insert(from_location, new.result.clone())
            .is_none());
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

pub(super) fn unify<T: Term>(
    from: &T,
    to: &T,
    from_logs: &[Log<T::Model>],
    to_logs: &[Log<T::Model>],
    predicate: &impl PredicateA<T::Model>,
    environment: &Environment<T::Model, impl Normalizer<T::Model>>,
) -> crate::ResultArc<Unifier<T>, T::Model> {
    if let Some(result) =
        environment.query(&Equality::new(from.clone(), to.clone()))?
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
    )? {
        return Ok(Some(Arc::new(unification)));
    }

    // try to look for equivalences
    for Succeeded { result: eq_from, mut constraints } in
        environment.get_equivalences(from)?
    {
        let mut from_logs = from_logs.to_vec();
        from_logs.push(eq_from.clone().into_rewritten());

        if let Some(unifier) = environment.query_with(
            &Unification::new(eq_from.clone(), to.clone(), predicate.clone()),
            (from_logs, to_logs.to_vec()),
            (),
        )? {
            constraints.extend(unifier.constraints.iter().cloned());

            let mut unifier = unifier.result.clone();
            unifier.rewritten_from = unifier.rewritten_from.or(Some(eq_from));

            return Ok(Some(Arc::new(Succeeded::with_constraints(
                unifier,
                constraints,
            ))));
        }
    }
    for Succeeded { result: eq_to, mut constraints } in
        environment.get_equivalences(to)?
    {
        let mut to_logs = to_logs.to_vec();
        to_logs.push(eq_to.clone().into_rewritten());

        if let Some(unifier) = environment.query_with(
            &Unification::new(from.clone(), eq_to.clone(), predicate.clone()),
            (from_logs.to_vec(), to_logs),
            (),
        )? {
            constraints.extend(unifier.constraints.iter().cloned());

            let mut unifier = unifier.result.clone();
            unifier.rewritten_right = unifier.rewritten_right.or(Some(eq_to));

            return Ok(Some(Arc::new(Succeeded::with_constraints(
                unifier,
                constraints,
            ))));
        }
    }

    Ok(None)
}

// TODO: bring test back
// #[cfg(test)]
// mod tests;
