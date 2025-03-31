//! Contains the definition of [`Definite`].

use std::{collections::BTreeSet, sync::Arc};

use pernixc_semantic::term::{
    generic_arguments::GenericArguments, visitor, Model,
};

use crate::{
    environment::{Environment, Query},
    normalizer::Normalizer,
    term::Term,
    Error, Satisfiability, Satisfied, Succeeded,
};

#[derive(Debug)]
struct Visitor<'a, N: Normalizer<M>, M: Model> {
    definite: Result<Option<Succeeded<Satisfied, M>>, Error>,

    environment: &'a Environment<'a, M, N>,
}

impl<'v, U: Term, N: Normalizer<U::Model>> visitor::Visitor<'v, U>
    for Visitor<'_, N, U::Model>
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
    type Error = super::Error;

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

impl<M: Model, N: Normalizer<M>> Environment<'_, M, N> {
    /// Checks if all the generic arguments are definite.
    ///
    /// See [`Definite`] for more information.
    #[allow(clippy::missing_errors_doc)]
    pub fn generic_arguments_definite(
        &self,
        generic_arguments: &GenericArguments<M>,
    ) -> crate::Result<Satisfied, M> {
        let mut constraints = BTreeSet::new();

        for lifetime in &generic_arguments.lifetimes {
            let Some(result) = self.query(&Definite::new(lifetime.clone()))?
            else {
                return Ok(None);
            };

            constraints.extend(result.constraints.iter().cloned());
        }

        for r#type in &generic_arguments.types {
            let Some(result) = self.query(&Definite::new(r#type.clone()))?
            else {
                return Ok(None);
            };

            constraints.extend(result.constraints.iter().cloned());
        }

        for constant in &generic_arguments.constants {
            let Some(result) = self.query(&Definite::new(constant.clone()))?
            else {
                return Ok(None);
            };

            constraints.extend(result.constraints.iter().cloned());
        }

        Ok(Some(Succeeded::satisfied_with(constraints)))
    }
}

#[cfg(test)]
mod test;
