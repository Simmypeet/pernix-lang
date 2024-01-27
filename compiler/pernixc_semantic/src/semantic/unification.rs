//! Contains the the unification logic.

use std::collections::HashMap;

use super::{
    equality,
    mapping::Map,
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
pub trait Config<T> {
    /// Determines if the two given terms are unifiable.
    fn unifiable(&mut self, lhs: &T, rhs: &T) -> bool;
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
    C: Config<T> + Config<Lifetime> + Config<Type> + Config<Constant>,
>(
    lhs: &T,
    rhs: &T,
    premise: &Premise,
    table: &Table<impl State>,
    config: &mut C,
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

        result.types.insert(lhs_location, new).unwrap();
    }

    for term::Match { lhs, rhs, lhs_location, .. } in substructural.lifetimes {
        let Some(new) =
            unify(&lhs, &rhs, premise, table, config, semantic, session)?
        else {
            return Ok(None);
        };

        result.lifetimes.insert(lhs_location, new).unwrap();
    }

    for term::Match { lhs, rhs, lhs_location, .. } in substructural.constants {
        let Some(new) =
            unify(&lhs, &rhs, premise, table, config, semantic, session)?
        else {
            return Ok(None);
        };

        result.constants.insert(lhs_location, new).unwrap();
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
    C: Config<T> + Config<Lifetime> + Config<Type> + Config<Constant>,
>(
    lhs: &T,
    rhs: &T,
    premise: &Premise,
    table: &Table<impl State>,
    config: &mut C,
    semantic: &mut S,
    session: &mut Limit<R>,
) -> Result<Option<Unification<T>>, ExceedLimitError> {
    if equality::equals(lhs, rhs, premise, table, semantic, session)? {
        return Ok(Some(Unification {
            rewritten_lhs: None,
            rewritten_rhs: None,
            r#match: Match::Equality,
        }));
    }

    let query = Query { lhs, rhs };

    match session.mark_as_in_progress(query.clone())? {
        Some(session::Cached::Done(result)) => return Ok(Some(result)),
        Some(session::Cached::InProgress) => {
            return Ok(None);
        }
        None => {}
    }

    if config.unifiable(lhs, rhs) {
        let unification = Unification {
            rewritten_lhs: None,
            rewritten_rhs: None,
            r#match: Match::Unifiable(lhs.clone(), rhs.clone()),
        };

        session.mark_as_done(query, unification.clone());
        return Ok(Some(unification));
    }

    if let Some(unification) = substructural_unify(
        lhs, rhs, premise, table, config, semantic, session,
    )? {
        session.mark_as_done(query, unification.clone());
        return Ok(Some(unification));
    }

    // try to normalize lhs, rhs
    if let Some(normalized_lhs) =
        semantic.normalize(lhs, premise, table, session)?
    {
        if let Some(mut unification) = unify(
            &normalized_lhs,
            rhs,
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
        semantic.normalize(rhs, premise, table, session)?
    {
        if let Some(mut unification) = unify(
            lhs,
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
    for (key, values) in <T as Map>::get(&premise.equalities_mapping) {
        if equality::equals(lhs, key, premise, table, semantic, session)? {
            for value in values {
                if let Some(mut unification) = unify(
                    value, rhs, premise, table, config, semantic, session,
                )? {
                    unification.rewritten_lhs =
                        unification.rewritten_lhs.or(Some(value.clone()));

                    session.mark_as_done(query, unification.clone());
                    return Ok(Some(unification));
                }
            }
        }

        if equality::equals(key, rhs, premise, table, semantic, session)? {
            for value in values {
                if let Some(mut unification) = unify(
                    lhs, value, premise, table, config, semantic, session,
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

// #[cfg(test)]
// mod tests;
