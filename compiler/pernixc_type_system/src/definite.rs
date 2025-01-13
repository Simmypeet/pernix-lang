//! Contains the definition of [`Definite`].

use std::sync::Arc;

use pernixc_term::{visitor, Model};

use crate::{
    environment::{Environment, Query},
    normalizer::Normalizer,
    term::Term,
    AbruptError, Satisfiability, Satisfied, Succeeded,
};

#[derive(Debug)]
struct Visitor<'a, N: Normalizer<M>, M: Model> {
    definite: Result<Option<Succeeded<Satisfied, M>>, AbruptError>,

    environment: &'a Environment<'a, M, N>,
}

impl<'a, 'v, U: Term, N: Normalizer<U::Model>> visitor::Visitor<'v, U>
    for Visitor<'a, N, U::Model>
{
    fn visit(&mut self, term: &'v U, _: U::Location) -> bool {
        // early return
        if let Ok(None) | Err(_) = self.definite {
            return false;
        }

        match self.environment.query(&Definite(term.clone())) {
            Err(err) => {
                self.definite = Err(err);
                false
            }

            Ok(None) => {
                self.definite = Ok(None);
                false
            }

            Ok(Some(result)) => match &mut self.definite {
                Ok(Some(current)) => {
                    current
                        .constraints
                        .extend(result.constraints.iter().cloned());
                    true
                }

                _ => false,
            },
        }
    }
}

/// A query for checking definite predicate satisfiability.
///
/// A term is definite if the term doesn't contain any generic parameters.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Definite<T>(pub T);

impl<T> Definite<T> {
    /// Creates a new `Definite` query.
    #[must_use]
    pub const fn new(term: T) -> Self { Self(term) }
}

impl<T: Term> Query for Definite<T> {
    type Model = T::Model;
    type Parameter = ();
    type InProgress = ();
    type Result = Succeeded<Satisfied, T::Model>;
    type Error = super::AbruptError;

    fn query(
        &self,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
        (): Self::Parameter,
        (): Self::InProgress,
    ) -> Result<Option<Arc<Self::Result>>, Self::Error> {
        let satisfiability = self.0.definite_satisfiability();

        // trivially satisfiable
        if satisfiability == Satisfiability::Satisfied {
            return Ok(Some(Arc::new(Succeeded::satisfied())));
        }

        // satisfiable with congruence
        if satisfiability == Satisfiability::Congruent {
            let mut visitor = Visitor {
                definite: Ok(Some(Succeeded::satisfied())),
                environment,
            };

            assert!(self.0.accept_one_level(&mut visitor).is_ok());

            if let Some(result) = visitor.definite? {
                return Ok(Some(Arc::new(result)));
            }
        }

        // get the equivalences
        for Succeeded { result: eq, mut constraints } in
            environment.get_equivalences(&self.0)?
        {
            if let Some(result) = environment.query(&Self(eq))? {
                constraints.extend(result.constraints.iter().cloned());

                return Ok(Some(Arc::new(Succeeded::satisfied_with(
                    constraints,
                ))));
            }
        }

        Ok(None)
    }
}

#[cfg(test)]
mod test;
