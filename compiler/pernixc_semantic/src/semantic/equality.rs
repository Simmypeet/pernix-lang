//! Contains the code related to checking equality of entities.

use super::{
    map::Map,
    predicate::Premises,
    session::Session,
    term::{constant::Constant, lifetime::Lifetime, r#type::Type},
    unification, Semantic, Term,
};
use crate::table::{State, Table};

/// Records object for the [`equals()`] query.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Record<'a, T> {
    pub lhs: &'a T,
    pub rhs: &'a T,
}

fn equals_by_mapping<
    T: Term,
    S: Semantic<T>
        + Semantic<Type<<T as Term>::Model>>
        + Semantic<Lifetime<<T as Term>::Model>>
        + Semantic<Constant<<T as Term>::Model>>,
    R: Session<T>
        + Session<Type<<T as Term>::Model>>
        + Session<Lifetime<<T as Term>::Model>>
        + Session<Constant<<T as Term>::Model>>,
>(
    lhs: &T,
    rhs: &T,
    premises: &Premises<<T as Term>::Model>,
    table: &Table<impl State>,
    semantic: &mut S,
    session: &mut R,
) -> bool {
    for (source, targets) in <T as Map>::get(&premises.mapping) {
        for target in targets {
            // avoid redudant works
            if semantic.trivially_equals(source, rhs)
                || (semantic.trivially_equals(rhs, lhs) && semantic.trivially_equals(target, lhs))
            {
                continue;
            }

            // check if the source and target are equal
            if lhs.equals(source, premises, table, semantic, session)
                && rhs.equals(target, premises, table, semantic, session)
            {
                return true;
            }
        }
    }

    false
}

fn equals_by_normalization<
    T: Term,
    S: Semantic<T>
        + Semantic<Type<<T as Term>::Model>>
        + Semantic<Lifetime<<T as Term>::Model>>
        + Semantic<Constant<<T as Term>::Model>>,
    R: Session<T>
        + Session<Type<<T as Term>::Model>>
        + Session<Lifetime<<T as Term>::Model>>
        + Session<Constant<<T as Term>::Model>>,
>(
    lhs: &T,
    rhs: &T,
    premises: &Premises<<T as Term>::Model>,
    table: &Table<impl State>,
    semantic: &mut S,
    session: &mut R,
) -> bool {
    if let Some(lhs_normalized) = lhs.normalize(premises, table, semantic, session) {
        lhs_normalized.equals(rhs, premises, table, semantic, session)
    } else if let Some(rhs_normalized) = rhs.normalize(premises, table, semantic, session) {
        lhs.equals(&rhs_normalized, premises, table, semantic, session)
    } else {
        false
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
struct All;

impl<T> unification::Config<T> for All {
    fn unifiable(&mut self, _: &T, _: &T) -> bool { true }
}

fn equals_by_unification<
    T: Term,
    S: Semantic<T>
        + Semantic<Type<<T as Term>::Model>>
        + Semantic<Lifetime<<T as Term>::Model>>
        + Semantic<Constant<<T as Term>::Model>>,
    R: Session<T>
        + Session<Type<<T as Term>::Model>>
        + Session<Lifetime<<T as Term>::Model>>
        + Session<Constant<<T as Term>::Model>>,
>(
    lhs: &T,
    rhs: &T,
    premises: &Premises<<T as Term>::Model>,
    table: &Table<impl State>,
    semantic: &mut S,
    session: &mut R,
) -> bool {
    let Ok(unification) =
        unification::sub_structural_unify(lhs, rhs, premises, table, semantic, session, &mut All)
    else {
        return false;
    };

    for (source, target) in &unification.types {
        if !source.equals(target, premises, table, semantic, session) {
            return false;
        }
    }

    for (source, target) in &unification.lifetimes {
        if !source.equals(target, premises, table, semantic, session) {
            return false;
        }
    }

    for (source, target) in &unification.constants {
        if !source.equals(target, premises, table, semantic, session) {
            return false;
        }
    }

    true
}

/// Checks if the two given terms are equal based on the given premises.
pub(super) fn equals<
    T: Term,
    S: Semantic<T>
        + Semantic<Type<<T as Term>::Model>>
        + Semantic<Lifetime<<T as Term>::Model>>
        + Semantic<Constant<<T as Term>::Model>>,
    R: Session<T>
        + Session<Type<<T as Term>::Model>>
        + Session<Lifetime<<T as Term>::Model>>
        + Session<Constant<<T as Term>::Model>>,
>(
    lhs: &T,
    rhs: &T,
    premises: &Premises<<T as Term>::Model>,
    table: &Table<impl State>,
    semantic: &mut S,
    session: &mut R,
) -> bool {
    if semantic.trivially_equals(lhs, rhs) {
        return true;
    }

    if !session.mark_as_working_on(Record { lhs, rhs }) {
        return false;
    }

    // equals by unification
    if equals_by_unification(lhs, rhs, premises, table, semantic, session) {
        session.mark_as_done(Record { lhs, rhs });
        return true;
    }

    // equals by mapping
    if equals_by_mapping(lhs, rhs, premises, table, semantic, session) {
        session.mark_as_done(Record { lhs, rhs });
        return true;
    }

    // equals by normalization
    if equals_by_normalization(lhs, rhs, premises, table, semantic, session) {
        session.mark_as_done(Record { lhs, rhs });
        return true;
    }

    session.mark_as_done(Record { lhs, rhs });
    false
}

#[cfg(test)]
mod tests;
