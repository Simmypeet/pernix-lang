use crate::{
    semantic::{
        equality,
        mapping::Map,
        session::{Cached, ExceedLimitError, Limit, Satisfied, Session},
        term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
        Premise, Semantic,
    },
    table::{State, Table},
};

/// A query for checking [`Tuple`] predicate satisfiability.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Query<'a, T>(pub &'a T);

/// The predicate meaning that the term is a tuple and is unpackable.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple<T>(T);

impl<T: Term> Tuple<T> {
    /// Determines whether a predicate of the term is satisfiable.
    ///
    /// # Errors
    ///
    /// See [`ExceedLimitError`] for more information.
    pub fn satisfied<
        S: Semantic<T> + Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
        R: Session<T> + Session<Lifetime> + Session<Type> + Session<Constant>,
    >(
        term: &T,
        premise: &Premise,
        table: &Table<impl State>,
        semantic: &mut S,
        session: &mut Limit<R>,
    ) -> Result<bool, ExceedLimitError> {
        // trivially satisfied
        if term.is_tuple() {
            return Ok(true);
        }

        match session.mark_as_in_progress(Query(term))? {
            Some(Cached::Done(Satisfied)) => return Ok(true),
            Some(Cached::InProgress) => return Ok(false),
            None => {}
        }

        // normalize the term
        if let Some(normalized) = semantic.normalize(term, premise, table, session)? {
            if Self::satisfied(&normalized, premise, table, semantic, session)? {
                session.mark_as_done(Query(term), Satisfied);
                return Ok(true);
            }
        }

        // check for equivalence
        for (key, values) in <T as Map>::get(&premise.equalities_mapping) {
            if equality::equals(term, key, premise, table, semantic, session)? {
                for value in values {
                    if Self::satisfied(value, premise, table, semantic, session)? {
                        session.mark_as_done(Query(term), Satisfied);
                        return Ok(true);
                    }
                }
            }
        }

        session.clear_query(Query(term));
        Ok(false)
    }
}
