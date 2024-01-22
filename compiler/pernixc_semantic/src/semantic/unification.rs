//! Contains the the unification logic.

use std::collections::{HashMap, HashSet};

use super::{
    equality::equals,
    session::{ExceedLimitError, Limit, Session},
    term::{constant::Constant, lifetime::Lifetime, r#type::Type, Match, Term},
    Premise, Semantic,
};
use crate::{
    semantic::{mapping::Map, session},
    table::{State, Table},
};

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

/// The result of unification.
///
/// The terms on the left-hand side are stored as the keys, and the terms on the right-hand side are
/// stored as the values.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[allow(missing_docs)]
pub struct Unification {
    pub lifetimes: HashMap<Lifetime, HashSet<Lifetime>>,
    pub types: HashMap<Type, HashSet<Type>>,
    pub constants: HashMap<Constant, HashSet<Constant>>,
}

impl Unification {
    /// Combines the other unification into this unification.
    pub fn combine(&mut self, other: Self) {
        for (lhs, rhs) in other.lifetimes {
            self.lifetimes.entry(lhs).or_default().extend(rhs);
        }

        for (lhs, rhs) in other.types {
            self.types.entry(lhs).or_default().extend(rhs);
        }

        for (lhs, rhs) in other.constants {
            self.constants.entry(lhs).or_default().extend(rhs);
        }
    }
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
) -> Result<Option<Unification>, ExceedLimitError> {
    let Some(substructural) = lhs.substructural_match(rhs) else {
        return Ok(None);
    };

    let mut unification = Unification::default();

    for Match { lhs, rhs, .. } in substructural.types {
        let Some(new) = unify(&lhs, &rhs, premise, table, config, semantic, session)? else {
            return Ok(None);
        };

        unification.combine(new);
    }

    for Match { lhs, rhs, .. } in substructural.lifetimes {
        let Some(new) = unify(&lhs, &rhs, premise, table, config, semantic, session)? else {
            return Ok(None);
        };

        unification.combine(new);
    }

    for Match { lhs, rhs, .. } in substructural.constants {
        let Some(new) = unify(&lhs, &rhs, premise, table, config, semantic, session)? else {
            return Ok(None);
        };

        unification.combine(new);
    }

    Ok(Some(unification))
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
) -> Result<Option<Unification>, ExceedLimitError> {
    if equals(lhs, rhs, premise, table, semantic, session)? {
        return Ok(Some(Unification::default()));
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
        let mut unification = Unification::default();

        T::get_unification_mut(&mut unification)
            .entry(lhs.clone())
            .or_default()
            .insert(rhs.clone());

        return Ok(Some(unification));
    }

    if let Some(unification) =
        substructural_unify(lhs, rhs, premise, table, config, semantic, session)?
    {
        session.mark_as_done(query, unification.clone());
        return Ok(Some(unification));
    }

    // try to normalize lhs, rhs
    if let Some(normalized_lhs) = semantic.normalize(lhs, premise, table, session)? {
        if let Some(unification) = unify(
            &normalized_lhs,
            rhs,
            premise,
            table,
            config,
            semantic,
            session,
        )? {
            session.mark_as_done(query, unification.clone());
            return Ok(Some(unification));
        }
    }

    if let Some(normalized_rhs) = semantic.normalize(rhs, premise, table, session)? {
        if let Some(unification) = unify(
            lhs,
            &normalized_rhs,
            premise,
            table,
            config,
            semantic,
            session,
        )? {
            session.mark_as_done(query, unification.clone());
            return Ok(Some(unification));
        }
    }

    // try to look for equivalent terms in the preimse
    for (key, values) in <T as Map>::get(&premise.equalities_mapping) {
        if equals(lhs, key, premise, table, semantic, session)? {
            for value in values {
                if let Some(unification) =
                    unify(value, rhs, premise, table, config, semantic, session)?
                {
                    session.mark_as_done(query, unification.clone());
                    return Ok(Some(unification));
                }
            }
        }

        if equals(key, rhs, premise, table, semantic, session)? {
            for value in values {
                if let Some(unification) =
                    unify(lhs, value, premise, table, config, semantic, session)?
                {
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
