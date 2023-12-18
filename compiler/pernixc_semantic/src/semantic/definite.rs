//! Contains the logic for checking if the terms are definite or not.

use super::{
    predicate::Premises,
    session::Session,
    term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
    visitor, Semantic,
};
use crate::table::{State, Table};

/// The definitiveness property of the term.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Property {
    /// The term is definite.
    Definite,

    /// The term is indefinite.
    Indefinite,

    /// The term is applicative, needs to check the sub-terms.
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
    semantic: &'s mut S,
    session: &'r mut R,

    is_trivially_definite: bool,
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

    fn visit_type(&mut self, ty: &Type<Self::Model>) -> bool {
        let result = ty.definite(self.premises, self.table, self.semantic, self.session);

        if !result {
            self.is_trivially_definite = false;
        }

        result
    }

    fn visit_lifetime(&mut self, ty: &Lifetime<Self::Model>) -> bool {
        let result = ty.definite(self.premises, self.table, self.semantic, self.session);

        if !result {
            self.is_trivially_definite = false;
        }

        result
    }

    fn visit_constant(&mut self, ty: &Constant<Self::Model>) -> bool {
        let result = ty.definite(self.premises, self.table, self.semantic, self.session);

        if !result {
            self.is_trivially_definite = false;
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
    mut semantic: &mut S,
    mut session: &mut R,
) -> bool {
    match semantic.definitive_property(term) {
        Property::Definite => true,
        Property::Indefinite => false,
        Property::Applicative => {
            if !session.mark_as_working_on(Record(term)) {
                return false;
            }

            let mut all_trivially_definite: Visitor<'_, '_, '_, _, T, S, R> = Visitor {
                premises,
                table,
                semantic,
                session,
                is_trivially_definite: true,
            };

            term.accept(&mut all_trivially_definite, true);

            let Visitor {
                is_trivially_definite,
                semantic: semantic_new,
                session: session_new,
                ..
            } = all_trivially_definite;

            semantic = semantic_new;
            session = session_new;

            if is_trivially_definite {
                session.mark_as_done(Record(term));
                return true;
            }

            // try to normalize the term
            if let Some(normalized) = term.normalize(premises, table, semantic, session) {
                if normalized.definite(premises, table, semantic, session) {
                    session.mark_as_done(Record(term));
                    return true;
                }
            }

            session.mark_as_done(Record(term));
            false
        }
    }
}

#[cfg(test)]
mod tests;
