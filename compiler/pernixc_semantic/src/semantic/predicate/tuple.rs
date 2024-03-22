use super::contains_forall_lifetime;
use crate::{
    semantic::{
        equality,
        instantiation::{self, Instantiation},
        session::{Cached, ExceedLimitError, Limit, Satisfied, Session},
        term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
        Premise, Semantic,
    },
    table::{self, DisplayObject, State, Table},
};

/// A query for checking [`Tuple`] predicate satisfiability.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Query<'a, T>(pub &'a T);

/// The predicate meaning that the term is a tuple and is unpackable.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple<T>(pub T);

impl<S: State, T: table::Display<S>> table::Display<S> for Tuple<T> {
    fn fmt(
        &self,
        table: &Table<S>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "tuple {}", DisplayObject { display: &self.0, table })
    }
}

impl<T: Term> Tuple<T> {
    /// Checks if the term contains a `forall` lifetime.
    #[must_use]
    pub fn contains_forall_lifetime(&self) -> bool {
        contains_forall_lifetime(&self.0)
    }

    /// Applies a instantiation to the [`Tuple`] term.
    pub fn instantiate(&mut self, instantiation: &Instantiation) {
        instantiation::instantiate(&mut self.0, instantiation);
    }
}

impl<T: Term> Tuple<T> {
    /// Determines whether a predicate of the term is satisfiable.
    ///
    /// # Errors
    ///
    /// See [`ExceedLimitError`] for more information.
    pub fn satisfies<
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
        if term.as_tuple().is_some() {
            return Ok(true);
        }

        match session.mark_as_in_progress(Query(term), ())? {
            Some(Cached::Done(Satisfied)) => return Ok(true),
            Some(Cached::InProgress(())) => return Ok(false),
            None => {}
        }

        // normalize the term
        if let Some(normalized) =
            semantic.normalize(term, premise, table, session)?
        {
            if Self::satisfies(&normalized, premise, table, semantic, session)?
            {
                session.mark_as_done(Query(term), Satisfied);
                return Ok(true);
            }
        }

        // check for equivalence
        for (key, values) in T::get_mapping(&premise.equalities_mapping) {
            if equality::equals(term, key, premise, table, semantic, session)? {
                for value in values {
                    if Self::satisfies(
                        value, premise, table, semantic, session,
                    )? {
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
