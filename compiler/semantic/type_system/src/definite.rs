//! Contains the definition of [`Definite`].

use std::{collections::BTreeSet, sync::Arc};

use pernixc_term::{generic_arguments::GenericArguments, visitor};

use crate::{
    environment::{BoxedFuture, Environment, Query},
    normalizer::Normalizer,
    term::Term,
    Error, Satisfiability, Satisfied, Succeeded,
};

#[derive(Debug)]
struct Visitor<'a, N: Normalizer> {
    definite: Result<Option<Succeeded<Satisfied>>, Error>,

    environment: &'a Environment<'a, N>,
}

impl<U: Term, N: Normalizer> visitor::AsyncVisitor<U> for Visitor<'_, N> {
    async fn visit(&mut self, term: &U, _: U::Location) -> bool {
        // early return
        if let Ok(None) | Err(_) = self.definite {
            return false;
        }

        match self.environment.query(&Definite(term.clone())).await {
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
    type Parameter = ();
    type InProgress = ();
    type Result = Succeeded<Satisfied>;
    type Error = super::Error;

    fn query<'x, N: Normalizer>(
        &'x self,
        environment: &'x Environment<'x, N>,
        (): Self::Parameter,
        (): Self::InProgress,
    ) -> BoxedFuture<'x, Self::Result, Self::Error> {
        Box::pin(async move {
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

                assert!(self
                    .0
                    .accept_one_level_async(&mut visitor)
                    .await
                    .is_ok());

                if let Some(result) = visitor.definite? {
                    return Ok(Some(Arc::new(result)));
                }
            }

            // get the equivalences
            for Succeeded { result: eq, constraints } in
                environment.get_equivalences(&self.0).await?.iter()
            {
                if let Some(result) =
                    environment.query(&Self(eq.clone())).await?
                {
                    return Ok(Some(Arc::new(Succeeded::satisfied_with(
                        constraints
                            .iter()
                            .cloned()
                            .chain(result.constraints.iter().cloned())
                            .collect(),
                    ))));
                }
            }

            Ok(None)
        })
    }
}

impl<N: Normalizer> Environment<'_, N> {
    /// Checks if all the generic arguments are definite.
    ///
    /// See [`Definite`] for more information.
    #[allow(clippy::missing_errors_doc)]
    pub async fn generic_arguments_definite(
        &self,
        generic_arguments: &GenericArguments,
    ) -> crate::Result<Satisfied> {
        let mut constraints = BTreeSet::new();

        for lifetime in &generic_arguments.lifetimes {
            let Some(result) = self.query(&Definite::new(*lifetime)).await?
            else {
                return Ok(None);
            };

            constraints.extend(result.constraints.iter().cloned());
        }

        for r#type in &generic_arguments.types {
            let Some(result) =
                self.query(&Definite::new(r#type.clone())).await?
            else {
                return Ok(None);
            };

            constraints.extend(result.constraints.iter().cloned());
        }

        for constant in &generic_arguments.constants {
            let Some(result) =
                self.query(&Definite::new(constant.clone())).await?
            else {
                return Ok(None);
            };

            constraints.extend(result.constraints.iter().cloned());
        }

        Ok(Some(Succeeded::satisfied_with(constraints)))
    }
}

impl<N: Normalizer> Environment<'_, N> {
    /// Checks if the term is "defininte".
    ///
    /// In the context of type-checking, the term is defininte if it doesn't
    /// contain any parameterized terms such as generic type or constant
    /// parameters (except lifetime parameters).
    pub async fn definite<T: Term>(
        &self,
        term: T,
    ) -> crate::ResultArc<Satisfied> {
        self.query(&Definite::new(term)).await
    }
}

#[cfg(test)]
mod test;
