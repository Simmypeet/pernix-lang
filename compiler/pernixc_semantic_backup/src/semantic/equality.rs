//! Contains the code related to checking equality of entities.

use std::collections::HashMap;

use super::{
    map::{Map, Mapping},
    model::Model,
    predicate::Premises,
    session::Session,
    term::{constant::Constant, lifetime::Lifetime, r#type::Type},
    unification, visitor, Semantic, Term,
};
use crate::table::{State, Table};

/// Records object for the [`equals()`] query.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Record<'a, T> {
    pub lhs: &'a T,
    pub rhs: &'a T,
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
    semantic: &S,
    session: &mut R,
) -> bool {
    for lhs_normalized in lhs.normalize(premises, table, semantic, session) {
        if rhs.equals(&lhs_normalized, premises, table, semantic, session) {
            return true;
        }
    }

    for rhs_normalized in rhs.normalize(premises, table, semantic, session) {
        if lhs.equals(&rhs_normalized, premises, table, semantic, session) {
            return true;
        }
    }

    false
}

struct Visitor<'a, S: Model> {
    captured_type_mappings: HashMap<Type<S>, Vec<Type<S>>>,
    captured_lifetime_mappings: HashMap<Lifetime<S>, Vec<Lifetime<S>>>,
    captured_constant_mappings: HashMap<Constant<S>, Vec<Constant<S>>>,

    premise_mapping: &'a Mapping<S>,
}

impl<S: Model> visitor::Visitor for Visitor<'_, S> {
    type Model = S;

    fn visit_type(
        &mut self,
        ty: &Type<Self::Model>,
        _: visitor::Source,
    ) -> bool {
        if let Some(terms) =
            <Type<Self::Model> as Map>::get(self.premise_mapping).get(ty)
        {
            for term in terms {
                self.captured_type_mappings
                    .entry(ty.clone())
                    .or_default()
                    .push(term.clone());
            }
        }

        true
    }

    fn visit_lifetime(
        &mut self,
        lifetime: &Lifetime<Self::Model>,
        _: visitor::Source,
    ) -> bool {
        if let Some(terms) =
            <Lifetime<Self::Model> as Map>::get(self.premise_mapping)
                .get(lifetime)
        {
            for term in terms {
                self.captured_lifetime_mappings
                    .entry(lifetime.clone())
                    .or_default()
                    .push(term.clone());
            }
        }

        true
    }

    fn visit_constant(
        &mut self,
        constant: &Constant<Self::Model>,
        _: visitor::Source,
    ) -> bool {
        if let Some(terms) =
            <Constant<Self::Model> as Map>::get(self.premise_mapping)
                .get(constant)
        {
            for term in terms {
                self.captured_constant_mappings
                    .entry(constant.clone())
                    .or_default()
                    .push(term.clone());
            }
        }

        true
    }
}

struct UnifyAll;

impl<T> unification::Config<T> for UnifyAll {
    fn unifiable(&mut self, _: &T, _: &T) -> bool { true }
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
    semantic: &S,
    session: &mut R,
) -> bool {
    for (lhs_eq, rhs_eqs) in <T as Map>::get(&premises.mapping) {
        if !lhs.equals(lhs_eq, premises, table, semantic, session) {
            continue;
        }

        for rhs_eq in rhs_eqs {
            if rhs.equals(rhs_eq, premises, table, semantic, session) {
                return true;
            }
        }
    }

    false
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
    semantic: &S,
    session: &mut R,
) -> bool {
    todo!()
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
    semantic: &S,
    session: &mut R,
) -> bool {
    if semantic.trivially_equals(lhs, rhs) {
        return true;
    }

    todo!()
}

#[cfg(test)]
mod tests;
