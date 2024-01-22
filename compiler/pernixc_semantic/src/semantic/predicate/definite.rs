use super::Satisfiability;
use crate::{
    semantic::{
        equality,
        mapping::Map,
        session::{self, ExceedLimitError, Limit, Satisfied, Session},
        term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
        visitor, Premise, Semantic,
    },
    table::{State, Table},
};

#[derive(Debug)]
struct Visitor<
    'a,
    's,
    'r,
    'l,
    T: State,
    S: Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
    R: Session<Lifetime> + Session<Type> + Session<Constant>,
> {
    definite: Result<bool, ExceedLimitError>,
    premise: &'a Premise,
    table: &'a Table<T>,
    semantic: &'s mut S,
    session: &'l mut Limit<'r, R>,
}

impl<
        'a,
        's,
        'r,
        'l,
        T: State,
        S: Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
        R: Session<Lifetime> + Session<Type> + Session<Constant>,
    > visitor::Visitor for Visitor<'a, 's, 'r, 'l, T, S, R>
{
    fn visit_type(&mut self, ty: &Type) -> bool {
        match definite(ty, self.premise, self.table, self.semantic, self.session) {
            result @ (Err(_) | Ok(false)) => {
                self.definite = result;
                return false;
            }

            Ok(true) => {}
        }

        true
    }

    fn visit_lifetime(&mut self, lifetime: &Lifetime) -> bool {
        match definite(
            lifetime,
            self.premise,
            self.table,
            self.semantic,
            self.session,
        ) {
            result @ (Err(_) | Ok(false)) => {
                self.definite = result;
                return false;
            }

            Ok(true) => {}
        }

        true
    }

    fn visit_constant(&mut self, constant: &Constant) -> bool {
        match definite(
            constant,
            self.premise,
            self.table,
            self.semantic,
            self.session,
        ) {
            result @ (Err(_) | Ok(false)) => {
                self.definite = result;
                return false;
            }

            Ok(true) => {}
        }

        true
    }
}

/// A query for checking definite predicate satisfiability.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Query<'a, T>(pub &'a T);

/// Determines whether a term is definite.
///
/// A term is definite if the term doesn't contain any generic parameters.
///
/// # Errors
///
/// See [`ExceedLimitError`] for more information.
pub fn definite<
    T: Term,
    S: Semantic<T> + Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
    R: Session<T> + Session<Lifetime> + Session<Type> + Session<Constant>,
>(
    term: &T,
    premise: &Premise,
    table: &Table<impl State>,
    semantic: &mut S,
    session: &mut Limit<R>,
) -> Result<bool, ExceedLimitError> {
    let satisfiability = term.definite_satisfiability();

    // trivially satisfiable
    if satisfiability == Satisfiability::Satisfied {
        session.mark_as_done(Query(term), Satisfied);
        return Ok(true);
    }

    match session.mark_as_in_progress(Query(term))? {
        Some(session::Cached::Done(Satisfied)) => return Ok(true),
        Some(session::Cached::InProgress) => {
            return Ok(false);
        }
        None => {}
    }

    // satisfiable with congruence
    if satisfiability == Satisfiability::Congruent {
        let mut visitor = Visitor {
            definite: Ok(true),
            premise,
            table,
            semantic,
            session,
        };

        let _ = term.accept_one_level(&mut visitor);

        if visitor.definite? {
            session.mark_as_done(Query(term), Satisfied);
            return Ok(true);
        }
    }

    // satisfiable with normalization
    if let Some(normalized) = semantic.normalize(term, premise, table, session)? {
        if definite(&normalized, premise, table, semantic, session)? {
            session.mark_as_done(Query(term), Satisfied);
            return Ok(true);
        }
    }

    for (key, values) in <T as Map>::get(&premise.equalities_mapping) {
        if equality::equals(term, key, premise, table, semantic, session)? {
            for value in values {
                if definite(value, premise, table, semantic, session)? {
                    session.mark_as_done(Query(term), Satisfied);
                    return Ok(true);
                }
            }
        }
    }

    session.clear_query(Query(term));
    Ok(false)
}

#[cfg(test)]
mod tests;
