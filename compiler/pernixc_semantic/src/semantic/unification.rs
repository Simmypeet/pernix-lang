//! Contains the the unification logic.

use std::collections::HashMap;

use super::{
    equality, get_equivalences, matching,
    model::Model,
    normalizer::Normalizer,
    session::{self, ExceedLimitError, Limit, Session},
    term::{
        constant::Constant, lifetime::Lifetime, r#type::Type, ModelOf, Term,
    },
    Environment,
};
use crate::table::State;

/// Represents a record of unifying two terms.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Query<'a, T> {
    pub lhs: &'a T,
    pub rhs: &'a T,
}

/// A customization point for the unification logic.
pub trait Config<M: Model> {
    /// Determines if the two given lifetimes are unifiable.
    ///
    /// # Errors
    ///
    /// See [`ExceedLimitError`].
    fn lifetime_unifiable(
        &mut self,
        from: &Lifetime<M>,
        to: &Lifetime<M>,
    ) -> Result<bool, ExceedLimitError>;

    /// Determines if the two given types are unifiable.
    ///
    /// # Errors
    ///
    /// See [`ExceedLimitError`].
    fn type_unifiable(
        &mut self,
        from: &Type<M>,
        to: &Type<M>,
    ) -> Result<bool, ExceedLimitError>;

    /// Determines if the two given constants are unifiable.
    ///
    /// # Errors
    ///
    /// See [`ExceedLimitError`].
    fn constant_unifiable(
        &mut self,
        from: &Constant<M>,
        to: &Constant<M>,
    ) -> Result<bool, ExceedLimitError>;
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
        config: &mut impl Config<Self::Model>,
    ) -> Result<bool, ExceedLimitError>;
}

impl<M: Model> Element for Lifetime<M> {
    fn unifiable(
        from: &Self,
        to: &Self,
        config: &mut impl Config<M>,
    ) -> Result<bool, ExceedLimitError> {
        config.lifetime_unifiable(from, to)
    }
}

impl<M: Model> Element for Type<M> {
    fn unifiable(
        from: &Self,
        to: &Self,
        config: &mut impl Config<M>,
    ) -> Result<bool, ExceedLimitError> {
        config.type_unifiable(from, to)
    }
}

impl<M: Model> Element for Constant<M> {
    fn unifiable(
        from: &Self,
        to: &Self,
        config: &mut impl Config<M>,
    ) -> Result<bool, ExceedLimitError> {
        config.constant_unifiable(from, to)
    }
}

/// Contains all the unification of substructural components.
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(missing_docs)]
pub struct Substructural<T: Term> {
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
pub enum Matching<T: Term> {
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
pub struct Unification<T: Term> {
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
    config: &mut impl Config<T::Model>,
    environment: &Environment<T::Model, impl State, impl Normalizer<T::Model>>,
    limit: &mut Limit<
        impl Session<T>
            + Session<Lifetime<T::Model>>
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
        let Some(new) = unify(&lhs, &rhs, config, environment, limit)? else {
            return Ok(None);
        };

        assert!(result.types.insert(lhs_location, new).is_none());
    }

    for matching::Matching { lhs, rhs, lhs_location, .. } in
        substructural.lifetimes
    {
        let Some(new) = unify(&lhs, &rhs, config, environment, limit)? else {
            return Ok(None);
        };

        assert!(result.lifetimes.insert(lhs_location, new).is_none());
    }

    for matching::Matching { lhs, rhs, lhs_location, .. } in
        substructural.constants
    {
        let Some(new) = unify(&lhs, &rhs, config, environment, limit)? else {
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

/// Unifies the two given terms.
///
/// # Errors
///
/// See [`ExceedLimitError`].
pub fn unify<T: Term>(
    from: &T,
    to: &T,
    config: &mut impl Config<T::Model>,
    environment: &Environment<T::Model, impl State, impl Normalizer<T::Model>>,
    limit: &mut Limit<
        impl Session<T>
            + Session<Lifetime<T::Model>>
            + Session<Type<T::Model>>
            + Session<Constant<T::Model>>,
    >,
) -> Result<Option<Unification<T>>, ExceedLimitError> {
    if equality::equals(from, to, environment, limit)? {
        return Ok(Some(Unification {
            rewritten_lhs: None,
            rewritten_rhs: None,
            r#match: Matching::Equality,
        }));
    }

    let query = Query { lhs: from, rhs: to };

    match limit.mark_as_in_progress(query.clone(), ())? {
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

        limit.mark_as_done(query, result.clone());
        return Ok(Some(result));
    }

    if let Some(unification) =
        substructural_unify(from, to, config, environment, limit)?
    {
        limit.mark_as_done(query, unification.clone());
        return Ok(Some(unification));
    }

    // try to look for equivalences
    for eq_lhs in get_equivalences(from, environment, limit)? {
        if let Some(mut unification) =
            unify(&eq_lhs, to, config, environment, limit)?
        {
            unification.rewritten_lhs =
                unification.rewritten_lhs.or(Some(eq_lhs));

            limit.mark_as_done(query, unification.clone());
            return Ok(Some(unification));
        }
    }

    for eq_rhs in get_equivalences(to, environment, limit)? {
        if let Some(mut unification) =
            unify(from, &eq_rhs, config, environment, limit)?
        {
            unification.rewritten_rhs =
                unification.rewritten_rhs.or(Some(eq_rhs));

            limit.mark_as_done(query, unification.clone());
            return Ok(Some(unification));
        }
    }

    limit.clear_query(query);
    Ok(None)
}

#[cfg(test)]
mod tests;
