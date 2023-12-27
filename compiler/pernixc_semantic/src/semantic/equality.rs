//! Implements the logic for equality checking.

use super::{
    mapping::Map,
    session::Session,
    term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
    Premise, Semantic,
};
use crate::table::{State, Table};

/// A query for checking equality.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(missing_docs)]
pub struct Query<'a, T> {
    pub lhs: &'a T,
    pub rhs: &'a T,
}

fn equals_by_unification<
    T: Term,
    S: Semantic<T> + Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
    R: Session<T> + Session<Lifetime> + Session<Type> + Session<Constant>,
>(
    lhs: &T,
    rhs: &T,
    premise: &Premise,
    table: &Table<impl State>,
    semantic: &mut S,
    session: &mut R,
) -> bool {
    let Some(matching) = lhs.substructural_match(rhs) else {
        return false;
    };

    for (lhs, rhs) in matching.lifetimes {
        if !equals(&lhs, &rhs, premise, table, semantic, session) {
            return false;
        }
    }

    for (lhs, rhs) in matching.types {
        if !equals(&lhs, &rhs, premise, table, semantic, session) {
            return false;
        }
    }

    for (lhs, rhs) in matching.constants {
        if !equals(&lhs, &rhs, premise, table, semantic, session) {
            return false;
        }
    }

    true
}

fn equals_by_normalization<
    T: Term,
    S: Semantic<T> + Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
    R: Session<T> + Session<Lifetime> + Session<Type> + Session<Constant>,
>(
    lhs: &T,
    rhs: &T,
    premise: &Premise,
    table: &Table<impl State>,
    semantic: &mut S,
    session: &mut R,
) -> bool {
    for lhs in semantic.normalize(lhs, premise, table, session) {
        if equals(&lhs, rhs, premise, table, semantic, session) {
            return true;
        }
    }

    for rhs in semantic.normalize(rhs, premise, table, session) {
        if equals(lhs, &rhs, premise, table, semantic, session) {
            return true;
        }
    }

    false
}

/// Checks if the two given terms are equal.
///
/// Equality is one of the most important parts of the type system. The equality model is based on
/// the first-order logic of equality.
///
/// These are the axioms of equality:
///
/// - Reflexivity: for all `x`, `x = x`.
/// - Symmetry: for all `x` and `y`, if `x = y`, then `y = x`.
/// - Transitivity: for all `x`, `y`, and `z`, if `x = y` and `y = z`, then `x = z`.
/// - Congruence: for all `x, ..., xn` and `y, ..., yn`, if `x = y`, ..., and `xn = yn`, then `f(x,
///   ..., xn) = f(y, ..., yn)`.
pub fn equals<
    T: Term,
    S: Semantic<T> + Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
    R: Session<T> + Session<Lifetime> + Session<Type> + Session<Constant>,
>(
    lhs: &T,
    rhs: &T,
    premise: &Premise,
    table: &Table<impl State>,
    semantic: &mut S,
    session: &mut R,
) -> bool {
    if semantic.trivially_equals(lhs, rhs) {
        return true;
    }

    if let Some(result) = session.mark_as_in_progress(Query { lhs, rhs }) {
        match result {
            super::session::Result::InProgress => {
                session.mark_as_done(Query { lhs, rhs }, false);
                return false;
            }
            super::session::Result::Done(result) => {
                return result;
            }
        }
    }

    if equals_by_unification(lhs, rhs, premise, table, semantic, session)
        || equals_by_normalization(lhs, rhs, premise, table, semantic, session)
    {
        session.mark_as_done(Query { lhs, rhs }, true);
        return true;
    }

    for (key, values) in <T as Map>::get(&premise.equalities_mapping) {
        if semantic.trivially_equals(lhs, key)
            || equals_by_unification(lhs, key, premise, table, semantic, session)
            || equals_by_normalization(lhs, key, premise, table, semantic, session)
        {
            for value in values {
                if equals(value, rhs, premise, table, semantic, session) {
                    session.mark_as_done(Query { lhs, rhs }, true);
                    return true;
                }
            }
        }

        if semantic.trivially_equals(key, rhs)
            || equals_by_unification(key, rhs, premise, table, semantic, session)
            || equals_by_normalization(key, rhs, premise, table, semantic, session)
        {
            for value in values {
                if equals(lhs, value, premise, table, semantic, session) {
                    session.mark_as_done(Query { lhs, rhs }, true);
                    return true;
                }
            }
        }
    }

    session.clear_query(Query { lhs, rhs });
    false
}

#[cfg(test)]
mod tests;
