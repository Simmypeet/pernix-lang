//! Contains the the unification logic.

use std::collections::HashMap;

use super::{
    equality,
    session::{self, ExceedLimitError, Limit, Session},
    term::{self, constant::Constant, lifetime::Lifetime, r#type::Type, Term},
    Premise, Semantic,
};
use crate::table::{State, Table};

/// Represents a record of unifying two terms.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Query<'a, T> {
    pub lhs: &'a T,
    pub rhs: &'a T,
}

/// A customization point for the unification logic.
pub trait Config {
    /// Determines if the two given lifetimes are unifiable.
    #[must_use]
    fn lifetime_unifiable(&mut self, from: &Lifetime, to: &Lifetime) -> bool;

    /// Determines if the two given types are unifiable.
    #[must_use]
    fn type_unifiable(&mut self, from: &Type, to: &Type) -> bool;

    /// Determines if the two given constants are unifiable.
    #[must_use]
    fn constant_unifiable(&mut self, from: &Constant, to: &Constant) -> bool;
}

/// A trait implemented by terms that can be unified.
pub trait Element {
    /// Accepts a configuration and returns `true` if the term is unifiable.
    fn unifiable(from: &Self, to: &Self, config: &mut impl Config) -> bool;
}

impl Element for Lifetime {
    fn unifiable(from: &Self, to: &Self, config: &mut impl Config) -> bool {
        config.lifetime_unifiable(from, to)
    }
}

impl Element for Type {
    fn unifiable(from: &Self, to: &Self, config: &mut impl Config) -> bool {
        config.type_unifiable(from, to)
    }
}

impl Element for Constant {
    fn unifiable(from: &Self, to: &Self, config: &mut impl Config) -> bool {
        config.constant_unifiable(from, to)
    }
}

/// Contains all the unification of substructural components.
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(missing_docs)]
pub struct Substructural<T: Term> {
    pub lifetimes: HashMap<T::SubLifetimeLocation, Unification<Lifetime>>,
    pub types: HashMap<T::SubTypeLocation, Unification<Type>>,
    pub constants: HashMap<T::SubConstantLocation, Unification<Constant>>,
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
pub enum Match<T: Term> {
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
    pub r#match: Match<T>,
}

fn substructural_unify<
    T: Term,
    S: Semantic<T> + Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
    R: Session<T> + Session<Lifetime> + Session<Type> + Session<Constant>,
>(
    lhs: &T,
    rhs: &T,
    premise: &Premise,
    table: &Table<impl State>,
    config: &mut impl Config,
    semantic: &mut S,
    session: &mut Limit<R>,
) -> Result<Option<Unification<T>>, ExceedLimitError> {
    let Some(substructural) = lhs.substructural_match(rhs) else {
        return Ok(None);
    };

    let mut result = Substructural::default();

    for term::Match { lhs, rhs, lhs_location, .. } in substructural.types {
        let Some(new) =
            unify(&lhs, &rhs, premise, table, config, semantic, session)?
        else {
            return Ok(None);
        };

        assert!(result.types.insert(lhs_location, new).is_none());
    }

    for term::Match { lhs, rhs, lhs_location, .. } in substructural.lifetimes {
        let Some(new) =
            unify(&lhs, &rhs, premise, table, config, semantic, session)?
        else {
            return Ok(None);
        };

        assert!(result.lifetimes.insert(lhs_location, new).is_none());
    }

    for term::Match { lhs, rhs, lhs_location, .. } in substructural.constants {
        let Some(new) =
            unify(&lhs, &rhs, premise, table, config, semantic, session)?
        else {
            return Ok(None);
        };

        assert!(result.constants.insert(lhs_location, new).is_none());
    }

    Ok(Some(Unification {
        rewritten_lhs: None,
        rewritten_rhs: None,
        r#match: Match::Substructural(result),
    }))
}

/// Unifies the two given terms.
///
/// # Errors
///
/// See [`ExceedLimitError`].
pub fn unify<
    T: Term,
    S: Semantic<T> + Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
    R: Session<T> + Session<Lifetime> + Session<Type> + Session<Constant>,
>(
    from: &T,
    to: &T,
    premise: &Premise,
    table: &Table<impl State>,
    config: &mut impl Config,
    semantic: &mut S,
    session: &mut Limit<R>,
) -> Result<Option<Unification<T>>, ExceedLimitError> {
    if equality::equals(from, to, premise, table, semantic, session)? {
        return Ok(Some(Unification {
            rewritten_lhs: None,
            rewritten_rhs: None,
            r#match: Match::Equality,
        }));
    }

    let query = Query { lhs: from, rhs: to };

    match session.mark_as_in_progress(query.clone(), ())? {
        Some(session::Cached::Done(result)) => return Ok(Some(result)),
        Some(session::Cached::InProgress(())) => {
            return Ok(None);
        }
        None => {}
    }

    if T::unifiable(from, to, config) {
        let unification = Unification {
            rewritten_lhs: None,
            rewritten_rhs: None,
            r#match: Match::Unifiable(from.clone(), to.clone()),
        };

        session.mark_as_done(query, unification.clone());
        return Ok(Some(unification));
    }

    if let Some(unification) = substructural_unify(
        from, to, premise, table, config, semantic, session,
    )? {
        session.mark_as_done(query, unification.clone());
        return Ok(Some(unification));
    }

    // try to normalize lhs, rhs
    if let Some(normalized_lhs) =
        semantic.normalize(from, premise, table, session)?
    {
        if let Some(mut unification) = unify(
            &normalized_lhs,
            to,
            premise,
            table,
            config,
            semantic,
            session,
        )? {
            unification.rewritten_lhs =
                unification.rewritten_lhs.or(Some(normalized_lhs));

            session.mark_as_done(query, unification.clone());
            return Ok(Some(unification));
        }
    }

    if let Some(normalized_rhs) =
        semantic.normalize(to, premise, table, session)?
    {
        if let Some(mut unification) = unify(
            from,
            &normalized_rhs,
            premise,
            table,
            config,
            semantic,
            session,
        )? {
            unification.rewritten_rhs =
                unification.rewritten_rhs.or(Some(normalized_rhs));

            session.mark_as_done(query, unification.clone());
            return Ok(Some(unification));
        }
    }

    // try to look for equivalent terms in the premise
    for (key, values) in T::get_mapping(&premise.equalities_mapping) {
        if equality::equals(from, key, premise, table, semantic, session)? {
            for value in values {
                if let Some(mut unification) =
                    unify(value, to, premise, table, config, semantic, session)?
                {
                    unification.rewritten_lhs =
                        unification.rewritten_lhs.or(Some(value.clone()));

                    session.mark_as_done(query, unification.clone());
                    return Ok(Some(unification));
                }
            }
        }

        if equality::equals(key, to, premise, table, semantic, session)? {
            for value in values {
                if let Some(mut unification) = unify(
                    from, value, premise, table, config, semantic, session,
                )? {
                    unification.rewritten_rhs =
                        unification.rewritten_rhs.or(Some(value.clone()));

                    session.mark_as_done(query, unification.clone());
                    return Ok(Some(unification));
                }
            }
        }
    }

    session.clear_query(query);
    Ok(None)
}

#[cfg(test)]
mod tests;
