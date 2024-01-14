//! Implements the logic for equality checking.

use super::{
    mapping::Map,
    session::{self, ExceedLimitError, Limit, Session},
    term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
    Premise, Semantic,
};
use crate::table::{State, Table};

/// A query for checking equality
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
    session: &mut Limit<R>,
) -> Result<bool, ExceedLimitError> {
    let Some(matching) = lhs.substructural_match(rhs) else {
        return Ok(false);
    };

    for (lhs, rhs) in matching.lifetimes {
        if !equals(&lhs, &rhs, premise, table, semantic, session)? {
            return Ok(false);
        }
    }

    for (lhs, rhs) in matching.types {
        if !equals(&lhs, &rhs, premise, table, semantic, session)? {
            return Ok(false);
        }
    }

    for (lhs, rhs) in matching.constants {
        if !equals(&lhs, &rhs, premise, table, semantic, session)? {
            return Ok(false);
        }
    }

    Ok(true)
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
    session: &mut Limit<R>,
) -> Result<bool, ExceedLimitError> {
    if let Some(lhs) = semantic.normalize(lhs, premise, table, session)? {
        if equals(&lhs, rhs, premise, table, semantic, session)? {
            return Ok(true);
        }
    }

    if let Some(rhs) = semantic.normalize(rhs, premise, table, session)? {
        if equals(lhs, &rhs, premise, table, semantic, session)? {
            return Ok(true);
        }
    }

    Ok(false)
}

fn equals_withour_mapping<
    T: Term,
    S: Semantic<T> + Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
    R: Session<T> + Session<Lifetime> + Session<Type> + Session<Constant>,
>(
    lhs: &T,
    rhs: &T,
    premise: &Premise,
    table: &Table<impl State>,
    semantic: &mut S,
    session: &mut Limit<R>,
) -> Result<bool, ExceedLimitError> {
    if semantic.trivially_equals(lhs, rhs)
        || equals_by_unification(lhs, rhs, premise, table, semantic, session)?
        || equals_by_normalization(lhs, rhs, premise, table, semantic, session)?
    {
        session.mark_as_done(Query { lhs, rhs }, true);
        return Ok(true);
    }

    Ok(false)
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
///
/// # Decidability
///
/// Equality is **partially-decidable**. In other words, the function can halt in the case of
/// equal terms, but it might output `false` or never halt in the case of unequal terms.
///
/// However, the function has a **soundness** guarantee. In other words, the function will never
/// output `true` in the case of unequal terms. Also, the algorithm has a hard-limit on the number
/// of recursive calls.
///
/// # Errors
///
/// See [`ExceedLimitError`] for more information.
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
    session: &mut Limit<R>,
) -> Result<bool, ExceedLimitError> {
    let query = Query { lhs, rhs };

    if semantic.trivially_equals(lhs, rhs) {
        return Ok(true);
    }

    match session.mark_as_in_progress(query.clone())? {
        Some(session::Cached::Done(result)) => return Ok(result),
        Some(session::Cached::InProgress) => {
            return Ok(false);
        }
        None => {}
    }

    if equals_by_unification(lhs, rhs, premise, table, semantic, session)?
        || equals_by_normalization(lhs, rhs, premise, table, semantic, session)?
    {
        session.mark_as_done(Query { lhs, rhs }, true);
        return Ok(true);
    }

    for (key, values) in <T as Map>::get(&premise.equalities_mapping) {
        if equals_withour_mapping(lhs, key, premise, table, semantic, session)? {
            for value in values {
                if equals(value, rhs, premise, table, semantic, session)? {
                    session.mark_as_done(Query { lhs, rhs }, true);
                    return Ok(true);
                }
            }
        }

        if equals_withour_mapping(key, rhs, premise, table, semantic, session)? {
            for value in values {
                if equals(lhs, value, premise, table, semantic, session)? {
                    session.mark_as_done(Query { lhs, rhs }, true);
                    return Ok(true);
                }
            }
        }
    }

    session.clear_query(query);
    Ok(false)
}

#[cfg(test)]
mod tests;
