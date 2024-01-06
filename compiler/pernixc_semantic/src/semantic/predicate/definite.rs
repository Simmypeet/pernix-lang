use super::Satisfiability;
use crate::{
    semantic::{
        equality,
        mapping::Map,
        session::{self, Session},
        term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
        visitor::{self, VisitMode},
        Premise, Semantic,
    },
    table::{State, Success, Table},
};

#[derive(Debug)]
struct Visitor<
    'a,
    's,
    'r,
    T: State,
    S: Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
    R: Session<Lifetime> + Session<Type> + Session<Constant>,
> {
    found_indefinite: bool,
    premise: &'a Premise,
    table: &'a Table<T>,
    semantic: &'s mut S,
    session: &'r mut R,
}

impl<
        'a,
        's,
        'r,
        T: State,
        S: Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
        R: Session<Lifetime> + Session<Type> + Session<Constant>,
    > visitor::Visitor for Visitor<'a, 's, 'r, T, S, R>
{
    fn visit_type(&mut self, ty: &Type, _: visitor::Source) -> bool {
        if !definite(ty, self.premise, self.table, self.semantic, self.session) {
            self.found_indefinite = true;
            return false;
        }

        true
    }

    fn visit_lifetime(&mut self, lifetime: &Lifetime, _: visitor::Source) -> bool {
        if !definite(
            lifetime,
            self.premise,
            self.table,
            self.semantic,
            self.session,
        ) {
            self.found_indefinite = true;
            return false;
        }

        true
    }

    fn visit_constant(&mut self, constant: &Constant, _: visitor::Source) -> bool {
        if !definite(
            constant,
            self.premise,
            self.table,
            self.semantic,
            self.session,
        ) {
            self.found_indefinite = true;
            return false;
        }

        true
    }
}

/// A query for checking definite predicate satisfiability.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Record<'a, T>(pub &'a T);

/// Determines wheter a term is definite.
///
/// A term is definite if the term doesn't contain any generic parameters.
pub fn definite<
    T: Term,
    S: Semantic<T> + Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
    R: Session<T> + Session<Lifetime> + Session<Type> + Session<Constant>,
>(
    term: &T,
    premise: &Premise,
    table: &Table<impl State>,
    semantic: &mut S,
    session: &mut R,
) -> bool {
    let satisfiability = term.definite_satisfiability();

    // trivially satisfiable
    if satisfiability == Satisfiability::Satisfied {
        session.mark_as_done(Record(term), true);
        return true;
    }

    match session.mark_as_in_progress(Record(term)) {
        Some(session::Result::Done(result)) => return result,
        Some(session::Result::InProgress) => {
            return false;
        }
        None => {}
    }

    // satisfiable with congruence
    if satisfiability == Satisfiability::Congruent {
        let mut visitor = Visitor {
            found_indefinite: false,
            premise,
            table,
            semantic,
            session,
        };

        let _ = term.accept_one_level(&mut visitor, VisitMode::<Success>::OnlySubTerms);

        if !visitor.found_indefinite {
            session.mark_as_done(Record(term), true);
            return true;
        }
    }

    // satisfiable with normalization
    // if let Some(normalized) = semantic.normalize(term, premise, table, session) {
    //     if definite(&normalized, premise, table, semantic, session) {
    //         session.mark_as_done(Record(term), true);
    //         return true;
    //     }
    // }

    for (key, values) in <T as Map>::get(&premise.equalities_mapping) {
        if equality::equals(term, key, premise, table, semantic, session) {
            for value in values {
                if definite(value, premise, table, semantic, session) {
                    session.mark_as_done(Record(term), true);
                    return true;
                }
            }
        }
    }

    session.clear_query(Record(term));
    false
}

#[cfg(test)]
mod tests;
