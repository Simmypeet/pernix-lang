//! Implements the [`Query`] for the [`Tuple`]

use std::sync::Arc;

use pernixc_term::{predicate::Tuple, r#type::Type, variance::Variance};

use crate::{
    environment::{Environment, Query},
    normalizer::Normalizer,
    Error, Satisfied, Succeeded,
};

impl Query for Tuple<Type> {
    type Parameter = ();
    type InProgress = ();
    type Result = Succeeded<Satisfied>;
    type Error = Error;

    async fn query(
        &self,
        environment: &Environment<'_, impl Normalizer>,
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
            .filter_map(|x| x.as_tuple_type())
        {
            if let Some(result) = environment
                .subtypes(
                    self.0.clone(),
                    predicate.0.clone(),
                    Variance::Covariant,
                )
                .await?
            {
                if !result.result.forall_lifetime_errors.is_empty() {
                    continue;
                }

                return Ok(Some(Arc::new(Succeeded::satisfied_with(
                    result.constraints.clone(),
                ))));
            }
        }

        // get the equivalences
        for Succeeded { result: eq, constraints } in
            environment.get_equivalences(&self.0).await?.iter()
        {
            if let Some(result) =
                Box::pin(environment.query(&Self(eq.clone()))).await?
            {
                return Ok(Some(Arc::new(Succeeded::satisfied_with(
                    result
                        .constraints
                        .iter()
                        .cloned()
                        .chain(constraints.iter().cloned())
                        .collect(),
                ))));
            }
        }

        Ok(None)
    }
}
