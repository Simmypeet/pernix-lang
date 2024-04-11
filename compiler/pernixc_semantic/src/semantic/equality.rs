//! Implements the logic for equality checking.

use super::{
    matching::Matching,
    session::{self, ExceedLimitError, Limit, Session},
    term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
    Environment, Satisfied,
};
use crate::table::State;

/// A query for checking equality
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(missing_docs)]
pub struct Query<'a, T> {
    pub lhs: &'a T,
    pub rhs: &'a T,
}

fn equals_by_unification<T: Term>(
    lhs: &T,
    rhs: &T,
    environment: &Environment<impl State>,
    limit: &mut Limit<
        impl Session<T> + Session<Lifetime> + Session<Type> + Session<Constant>,
    >,
) -> Result<bool, ExceedLimitError> {
    let Some(matching) = lhs.substructural_match(rhs) else {
        return Ok(false);
    };

    for Matching { lhs, rhs, .. } in matching.lifetimes {
        if !equals(&lhs, &rhs, environment, limit)? {
            return Ok(false);
        }
    }

    for Matching { lhs, rhs, .. } in matching.types {
        if !equals(&lhs, &rhs, environment, limit)? {
            return Ok(false);
        }
    }

    for Matching { lhs, rhs, .. } in matching.constants {
        if !equals(&lhs, &rhs, environment, limit)? {
            return Ok(false);
        }
    }

    Ok(true)
}

fn equals_by_normalization<T: Term>(
    lhs: &T,
    rhs: &T,
    environment: &Environment<impl State>,
    limit: &mut Limit<
        impl Session<T> + Session<Lifetime> + Session<Type> + Session<Constant>,
    >,
) -> Result<bool, ExceedLimitError> {
    if let Some(lhs) = lhs.normalize(environment, limit)? {
        if equals(&lhs, rhs, environment, limit)? {
            return Ok(true);
        }
    }

    if let Some(rhs) = rhs.normalize(environment, limit)? {
        if equals(lhs, &rhs, environment, limit)? {
            return Ok(true);
        }
    }

    Ok(false)
}

fn equals_without_mapping<T: Term>(
    lhs: &T,
    rhs: &T,
    environment: &Environment<impl State>,
    limit: &mut Limit<
        impl Session<T> + Session<Lifetime> + Session<Type> + Session<Constant>,
    >,
) -> Result<bool, ExceedLimitError> {
    if lhs == rhs
        || equals_by_unification(lhs, rhs, environment, limit)?
        || equals_by_normalization(lhs, rhs, environment, limit)?
    {
        limit.mark_as_done(Query { lhs, rhs }, Satisfied);
        return Ok(true);
    }

    Ok(false)
}

/// Checks if the two given terms are equal.
///
/// Equality is one of the most important parts of the type system. The equality
/// model is based on the first-order logic of equality.
///
/// These are the axioms of equality:
///
/// - Reflexivity: for all `x`, `x = x`.
/// - Symmetry: for all `x` and `y`, if `x = y`, then `y = x`.
/// - Transitivity: for all `x`, `y`, and `z`, if `x = y` and `y = z`, then `x =
///   z`.
/// - Congruence: for all `x, ..., xn` and `y, ..., yn`, if `x = y`, ..., and
///   `xn = yn`, then `f(x, ..., xn) = f(y, ..., yn)`.
///
/// # Decidability
///
/// Equality is **partially-decidable**. In other words, the function can halt
/// in the case of equal terms, but it might output `false` or never halt in the
/// case of unequal terms.
///
/// However, the function has a **soundness** guarantee. In other words, the
/// function will never output `true` in the case of unequal terms. Also, the
/// algorithm has a hard-limit on the number of recursive calls.
///
/// # Errors
///
/// See [`ExceedLimitError`] for more information.
pub fn equals<T: Term>(
    lhs: &T,
    rhs: &T,
    environment: &Environment<impl State>,
    limit: &mut Limit<
        impl Session<T> + Session<Lifetime> + Session<Type> + Session<Constant>,
    >,
) -> Result<bool, ExceedLimitError> {
    let query = Query { lhs, rhs };

    if lhs == rhs {
        return Ok(true);
    }

    match limit.mark_as_in_progress(query.clone(), ())? {
        Some(session::Cached::Done(Satisfied)) => return Ok(true),
        Some(session::Cached::InProgress(())) => {
            return Ok(false);
        }
        None => {}
    }

    if equals_by_normalization(lhs, rhs, environment, limit)?
        || equals_by_unification(lhs, rhs, environment, limit)?
    {
        limit.mark_as_done(query, Satisfied);
        return Ok(true);
    }

    for class in T::get_equivalent_classes(&environment.premise.equivalent) {
        for value in class.iter() {
            if equals_without_mapping(lhs, value, environment, limit)? {
                for value in class.iter() {
                    if equals(value, rhs, environment, limit)? {
                        limit.mark_as_done(query, Satisfied);
                        return Ok(true);
                    }
                }
            }

            if equals_without_mapping(value, rhs, environment, limit)? {
                for value in class.iter() {
                    if equals(lhs, value, environment, limit)? {
                        limit.mark_as_done(query, Satisfied);
                        return Ok(true);
                    }
                }
            }
        }
    }

    limit.clear_query(query);
    Ok(false)
}

#[cfg(test)]
pub(super) mod tests;
