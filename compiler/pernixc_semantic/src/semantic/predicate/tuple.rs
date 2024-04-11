use super::contains_forall_lifetime;
use crate::{
    semantic::{
        equality, get_equivalences,
        instantiation::{self, Instantiation},
        session::{Cached, ExceedLimitError, Limit, Session},
        term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
        Environment, Satisfied,
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
    pub fn satisfies(
        term: &T,
        environment: &Environment<impl State>,
        limit: &mut Limit<
            impl Session<T> + Session<Lifetime> + Session<Type> + Session<Constant>,
        >,
    ) -> Result<bool, ExceedLimitError> {
        // trivially satisfied
        if term.as_tuple().is_some() {
            return Ok(true);
        }

        match limit.mark_as_in_progress(Query(term), ())? {
            Some(Cached::Done(Satisfied)) => return Ok(true),
            Some(Cached::InProgress(())) => return Ok(false),
            None => {}
        }

        // check from predicates
        for predicate in T::tuple_predicates(environment.premise) {
            if equality::equals(predicate, term, environment, limit)? {
                limit.mark_as_done(Query(term), Satisfied);
                return Ok(true);
            }
        }

        // get the equivalences
        for eq in get_equivalences(term, environment, limit)? {
            if Self::satisfies(&eq, environment, limit)? {
                limit.mark_as_done(Query(term), Satisfied);
                return Ok(true);
            }
        }

        limit.clear_query(Query(term));
        Ok(false)
    }
}
