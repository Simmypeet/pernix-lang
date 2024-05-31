//! Contains the the unification logic.

use std::collections::HashMap;

use super::{
    equality, get_equivalences_impl, matching,
    model::Model,
    normalizer::Normalizer,
    session::{self, Limit, Session},
    sub_term::SubTerm,
    term::{
        constant::Constant, lifetime::Lifetime, r#type::Type, ModelOf, Term,
    },
    Environment, ExceedLimitError,
};
use crate::symbol::table::State;

/// Represents a record of unifying two terms.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Query<'a, T> {
    pub lhs: &'a T,
    pub rhs: &'a T,
}

/// An object used for determining if two terms are unifiable.
pub trait Config<T: Term> {
    /// Determines if the two given terms are unifiable.
    fn unifiable(&mut self, from: &T, to: &T)
        -> Result<bool, ExceedLimitError>;
}

/// A trait implemented by terms that can be unified.
pub trait Element: ModelOf {
    /// Accepts a configuration and returns `true` if the term is unifiable.
    ///
    /// # Errors
    ///
    /// See [`ExceedLimitError`].
    fn unifiable(
        from: &Self,
        to: &Self,
        config: &mut (impl Config<Lifetime<Self::Model>>
                  + Config<Type<Self::Model>>
                  + Config<Constant<Self::Model>>),
    ) -> Result<bool, ExceedLimitError>;
}

impl<M: Model> Element for Lifetime<M> {
    fn unifiable(
        from: &Self,
        to: &Self,
        config: &mut (impl Config<Lifetime<Self::Model>>
                  + Config<Type<Self::Model>>
                  + Config<Constant<Self::Model>>),
    ) -> Result<bool, ExceedLimitError> {
        config.unifiable(from, to)
    }
}

impl<M: Model> Element for Type<M> {
    fn unifiable(
        from: &Self,
        to: &Self,
        config: &mut (impl Config<Lifetime<Self::Model>>
                  + Config<Type<Self::Model>>
                  + Config<Constant<Self::Model>>),
    ) -> Result<bool, ExceedLimitError> {
        config.unifiable(from, to)
    }
}

impl<M: Model> Element for Constant<M> {
    fn unifiable(
        from: &Self,
        to: &Self,
        config: &mut (impl Config<Lifetime<Self::Model>>
                  + Config<Type<Self::Model>>
                  + Config<Constant<Self::Model>>),
    ) -> Result<bool, ExceedLimitError> {
        config.unifiable(from, to)
    }
}

/// Contains all the unification of substructural components.
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(missing_docs)]
pub struct Substructural<T: SubTerm> {
    pub lifetimes:
        HashMap<T::SubLifetimeLocation, Unification<Lifetime<T::Model>>>,
    pub types: HashMap<T::SubTypeLocation, Unification<Type<T::Model>>>,
    pub constants:
        HashMap<T::SubConstantLocation, Unification<Constant<T::Model>>>,
}

impl<T> Default for Substructural<T>
where
    T: Term,
{
    fn default() -> Self {
        Self {
            lifetimes: HashMap::new(),
            types: HashMap::new(),
            constants: HashMap::new(),
        }
    }
}

/// Specifies how a term matches another term.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Matching<T: SubTerm> {
    /// The two terms are unified by passing the predicate check in the
    /// [`Config`].
    Unifiable(T, T),

    /// The two terms are substructural and can be unified by unifying their
    /// substructural components.
    Substructural(Substructural<T>),

    /// The two terms are equal, no unification is needed.
    Equality,
}

/// Represents a unification between two terms.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Unification<T: SubTerm> {
    /// If the `lhs` has been rewritten into another form, this field will be
    /// `Some` of the rewritten term.
    pub rewritten_lhs: Option<T>,

    /// If the `rhs` has been rewritten into another form, this field will be
    /// `Some` of the rewritten term.
    pub rewritten_rhs: Option<T>,

    /// The unification of the `lhs` and `rhs` terms.
    pub r#match: Matching<T>,
}

fn substructural_unify<T: Term>(
    lhs: &T,
    rhs: &T,
    config: &mut (impl Config<Lifetime<T::Model>>
              + Config<Type<T::Model>>
              + Config<Constant<T::Model>>),
    environment: &Environment<T::Model, impl State, impl Normalizer<T::Model>>,
    limit: &mut Limit<
        impl Session<Lifetime<T::Model>>
            + Session<Type<T::Model>>
            + Session<Constant<T::Model>>,
    >,
) -> Result<Option<Unification<T>>, ExceedLimitError> {
    let Some(substructural) = lhs.substructural_match(rhs) else {
        return Ok(None);
    };

    let mut result = Substructural::default();

    for matching::Matching { lhs, rhs, lhs_location, .. } in substructural.types
    {
        let Some(new) = unify_impl(&lhs, &rhs, config, environment, limit)?
        else {
            return Ok(None);
        };

        assert!(result.types.insert(lhs_location, new).is_none());
    }

    for matching::Matching { lhs, rhs, lhs_location, .. } in
        substructural.lifetimes
    {
        let Some(new) = unify_impl(&lhs, &rhs, config, environment, limit)?
        else {
            return Ok(None);
        };

        assert!(result.lifetimes.insert(lhs_location, new).is_none());
    }

    for matching::Matching { lhs, rhs, lhs_location, .. } in
        substructural.constants
    {
        let Some(new) = unify_impl(&lhs, &rhs, config, environment, limit)?
        else {
            return Ok(None);
        };

        assert!(result.constants.insert(lhs_location, new).is_none());
    }

    Ok(Some(Unification {
        rewritten_lhs: None,
        rewritten_rhs: None,
        r#match: Matching::Substructural(result),
    }))
}

/// Unifies two terms.
///
/// # Errors
///
/// See [`ExceedLimitError`] for more information.
pub fn unify<T: Term>(
    from: &T,
    to: &T,
    config: &mut (impl Config<Lifetime<T::Model>>
              + Config<Type<T::Model>>
              + Config<Constant<T::Model>>),
    environment: &Environment<T::Model, impl State, impl Normalizer<T::Model>>,
) -> Result<Option<Unification<T>>, ExceedLimitError> {
    let mut limit = Limit::<session::Default<_>>::default();
    unify_impl(from, to, config, environment, &mut limit)
}

pub(super) fn unify_impl<T: Term>(
    from: &T,
    to: &T,
    config: &mut (impl Config<Lifetime<T::Model>>
              + Config<Type<T::Model>>
              + Config<Constant<T::Model>>),
    environment: &Environment<T::Model, impl State, impl Normalizer<T::Model>>,
    limit: &mut Limit<
        impl Session<Lifetime<T::Model>>
            + Session<Type<T::Model>>
            + Session<Constant<T::Model>>,
    >,
) -> Result<Option<Unification<T>>, ExceedLimitError> {
    if equality::equals_impl(from, to, environment, limit)? {
        return Ok(Some(Unification {
            rewritten_lhs: None,
            rewritten_rhs: None,
            r#match: Matching::Equality,
        }));
    }

    let query = Query { lhs: from, rhs: to };

    match limit.mark_as_in_progress::<T, _>(query.clone(), ())? {
        Some(session::Cached::Done(result)) => return Ok(Some(result)),
        Some(session::Cached::InProgress(())) => {
            return Ok(None);
        }
        None => {}
    }

    if T::unifiable(from, to, config)? {
        let result = Unification {
            rewritten_lhs: None,
            rewritten_rhs: None,
            r#match: Matching::Unifiable(from.clone(), to.clone()),
        };

        limit.mark_as_done::<T, _>(query, result.clone());
        return Ok(Some(result));
    }

    if let Some(unification) =
        substructural_unify(from, to, config, environment, limit)?
    {
        limit.mark_as_done::<T, _>(query, unification.clone());
        return Ok(Some(unification));
    }

    // try to look for equivalences
    for eq_lhs in get_equivalences_impl(from, environment, limit)? {
        if let Some(mut unification) =
            unify_impl(&eq_lhs, to, config, environment, limit)?
        {
            unification.rewritten_lhs =
                unification.rewritten_lhs.or(Some(eq_lhs));

            limit.mark_as_done::<T, _>(query, unification.clone());
            return Ok(Some(unification));
        }
    }

    for eq_rhs in get_equivalences_impl(to, environment, limit)? {
        if let Some(mut unification) =
            unify_impl(from, &eq_rhs, config, environment, limit)?
        {
            unification.rewritten_rhs =
                unification.rewritten_rhs.or(Some(eq_rhs));

            limit.mark_as_done::<T, _>(query, unification.clone());
            return Ok(Some(unification));
        }
    }

    limit.clear_query::<T, _>(query);
    Ok(None)
}

#[cfg(test)]
mod tests;
