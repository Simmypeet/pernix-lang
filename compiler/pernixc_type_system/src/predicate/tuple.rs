use std::sync::Arc;

use pernixc_term::{predicate::Tuple, variance::Variance};

use crate::{
    compatible::Compatibility,
    environment::{Environment, Query},
    normalizer::Normalizer,
    term::Term,
    AbruptError, Satisfied, Succeeded,
};

impl<T: Term> Query for Tuple<T> {
    type Model = T::Model;
    type Parameter = ();
    type InProgress = ();
    type Result = Succeeded<Satisfied, T::Model>;
    type Error = AbruptError;

    fn query(
        &self,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
        (): Self::Parameter,
        (): Self::InProgress,
    ) -> Result<Option<Arc<Self::Result>>, Self::Error> {
        // trivially satisfied
        if self.0.as_tuple().is_some() {
            return Ok(Some(Arc::new(Succeeded::satisfied())));
        }

        // check from predicates
        for predicate in environment
            .premise()
            .predicates
            .iter()
            .filter_map(|x| T::as_tuple_predicate(x))
        {
            if let Some(Succeeded {
                result: Compatibility { forall_lifetime_errors, .. },
                constraints,
            }) = environment.compatible(
                &self.0,
                &predicate.0,
                Variance::Covariant,
            )? {
                if !forall_lifetime_errors.is_empty() {
                    continue;
                }

                return Ok(Some(Arc::new(Succeeded::satisfied_with(
                    constraints,
                ))));
            }
        }

        // get the equivalences
        for Succeeded { result: eq, mut constraints } in
            environment.get_equivalences(&self.0)?
        {
            if let Some(result) = environment.query(&Self(eq))? {
                constraints.extend(result.constraints.iter().cloned());
                return Ok(Some(result));
            }
        }

        Ok(None)
    }
}
