//! Contains the logic for checking if the terms are definite or not.

use super::{
    map::Map,
    predicate::Premises,
    session::Session,
    term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
    visitor::{self, Source, VisitMode},
    Semantic,
};
use crate::table::{State, Success, Table};

/// The definitiveness property of the term.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Property {
    Positive,
    Negative,
    Applicative,
}

/// Records object for the [`definite()`] query.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Record<'a, T>(pub &'a T);

#[derive(Debug)]
struct Visitor<
    'p,
    's,
    'r,
    Ts: State,
    T: Term,
    S: Semantic<T>
        + Semantic<Type<<T as Term>::Model>>
        + Semantic<Lifetime<<T as Term>::Model>>
        + Semantic<Constant<<T as Term>::Model>>,
    R: Session<T>
        + Session<Type<<T as Term>::Model>>
        + Session<Lifetime<<T as Term>::Model>>
        + Session<Constant<<T as Term>::Model>>,
> {
    premises: &'p Premises<<T as Term>::Model>,
    table: &'p Table<Ts>,
    semantic: &'s S,
    session: &'r mut R,

    first: bool,
    definite: bool,
}

impl<
        Ts: State,
        T: Term,
        S: Semantic<T>
            + Semantic<Type<<T as Term>::Model>>
            + Semantic<Lifetime<<T as Term>::Model>>
            + Semantic<Constant<<T as Term>::Model>>,
        R: Session<T>
            + Session<Type<<T as Term>::Model>>
            + Session<Lifetime<<T as Term>::Model>>
            + Session<Constant<<T as Term>::Model>>,
    > visitor::Visitor for Visitor<'_, '_, '_, Ts, T, S, R>
{
    type Model = <T as Term>::Model;

    fn visit_type(&mut self, ty: &Type<Self::Model>, _: Source) -> bool {
        if self.first {
            self.first = false;
            return true;
        }

        let result = ty.definite(self.premises, self.table, self.semantic, self.session);

        if !result {
            self.definite = false;
        }

        result
    }

    fn visit_lifetime(&mut self, ty: &Lifetime<Self::Model>, _: Source) -> bool {
        if self.first {
            self.first = false;
            return true;
        }

        let result = ty.definite(self.premises, self.table, self.semantic, self.session);

        if !result {
            self.definite = false;
        }

        result
    }

    fn visit_constant(&mut self, ty: &Constant<Self::Model>, _: Source) -> bool {
        if self.first {
            self.first = false;
            return true;
        }

        let result = ty.definite(self.premises, self.table, self.semantic, self.session);

        if !result {
            self.definite = false;
        }

        result
    }
}

pub(super) fn definite<
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
    term: &T,
    premises: &Premises<<T as Term>::Model>,
    table: &Table<impl State>,
    mut semantic: &S,
    mut session: &mut R,
) -> bool {
    let applicative = match semantic.definitive_property(term) {
        Property::Positive => return true,
        Property::Negative => false,
        Property::Applicative => true,
    };

    if !session.mark_as_working_on(Record(term)) {
        return false;
    }

    if applicative {
        let mut visitor: Visitor<'_, '_, '_, _, T, S, R> = Visitor {
            premises,
            table,
            semantic,
            session,
            first: true,
            definite: true,
        };

        let _ = term.accept(&mut visitor, VisitMode::<Success>::OnlySubTerms);

        let Visitor {
            definite,
            semantic: semantic_new,
            session: session_new,
            ..
        } = visitor;

        semantic = semantic_new;
        session = session_new;

        if definite {
            session.mark_as_done(Record(term));
            return true;
        }
    }

    // try to normalize the term
    for normalized in term.normalize(premises, table, semantic, session) {
        if normalized.definite(premises, table, semantic, session) {
            session.mark_as_done(Record(term));
            return true;
        }
    }

    // try to look for equivalences
    for (lhs_eq, rhs_eqs) in <T as Map>::get(&premises.mapping) {
        if !lhs_eq.equals(term, premises, table, semantic, session) {
            continue;
        }

        for rhs_eq in rhs_eqs {
            if rhs_eq.definite(premises, table, semantic, session) {
                session.mark_as_done(Record(term));
                return true;
            }
        }
    }

    session.mark_as_done(Record(term));
    false
}

#[cfg(test)]
mod tests;
