//! Contains the definition of [`Definite`].

use super::{
    equivalence::get_equivalences_with_context, observer::Observer,
    predicate::Satisfiability, query::Context, Compute,
};
use crate::{
    symbol::table::State,
    type_system::{
        model::Model, normalizer::Normalizer, term::Term, visitor, Environment,
        Output, OverflowError, Satisfied, Succeeded,
    },
};

#[derive(Debug)]
struct Visitor<
    'a,
    'c,
    T: State,
    N: Normalizer<M, T>,
    O: Observer<M, T>,
    M: Model,
> {
    definite: Result<Output<Satisfied, M>, OverflowError>,

    environment: &'a Environment<'a, M, T, N, O>,
    context: &'c mut Context<M>,
}

impl<
        'a,
        'c,
        'v,
        U: Term,
        T: State,
        N: Normalizer<U::Model, T>,
        O: Observer<U::Model, T>,
    > visitor::Visitor<'v, U> for Visitor<'a, 'c, T, N, O, U::Model>
{
    fn visit(&mut self, term: &'v U, _: U::Location) -> bool {
        // early return
        if let Ok(None) | Err(_) = self.definite {
            return false;
        }

        match Definite(term.clone())
            .query_with_context(self.environment, self.context)
        {
            result @ (Err(_) | Ok(None)) => {
                self.definite = result;
                false
            }

            Ok(Some(result)) => match &mut self.definite {
                Ok(Some(current)) => {
                    current.constraints.extend(result.constraints);
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

impl<T: Term> Compute for Definite<T> {
    type Error = OverflowError;
    type Parameter = ();

    #[allow(private_bounds, private_interfaces)]
    fn implementation<S: State>(
        &self,
        environment: &Environment<
            Self::Model,
            S,
            impl Normalizer<Self::Model, S>,
            impl Observer<Self::Model, S>,
        >,
        context: &mut Context<Self::Model>,
        (): Self::Parameter,
        (): Self::InProgress,
    ) -> Result<Option<Self::Result>, Self::Error> {
        let satisfiability = self.0.definite_satisfiability();

        // trivially satisfiable
        if satisfiability == Satisfiability::Satisfied {
            return Ok(Some(Succeeded::satisfied()));
        }

        // satisfiable with congruence
        if satisfiability == Satisfiability::Congruent {
            let mut visitor = Visitor {
                definite: Ok(Some(Succeeded::satisfied())),
                environment,
                context,
            };

            assert!(self.0.accept_one_level(&mut visitor).is_ok());

            if let Some(result) = visitor.definite? {
                return Ok(Some(result));
            }
        }

        // get the equivalences
        for Succeeded { result: eq, constraints } in
            get_equivalences_with_context(&self.0, environment, context)?
        {
            if let Some(mut result) =
                Self(eq).query_with_context(environment, context)?
            {
                result.constraints.extend(constraints);

                return Ok(Some(result));
            }
        }

        Ok(None)
    }
}

#[cfg(test)]
mod tests;
