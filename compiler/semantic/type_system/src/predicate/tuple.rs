//! Implements the [`Query`] for the [`Tuple`]

use std::sync::Arc;

use pernixc_semantic_element::variance::Variance;
use pernixc_term::{predicate::Tuple, r#type::Type};

use crate::{
    Satisfied, Succeeded,
    environment::{BoxedFuture, Environment, Query},
    normalizer::Normalizer,
};

impl Query for Tuple<Type> {
    type InProgress = ();
    type Result = Option<Arc<Succeeded<Satisfied>>>;

    fn query<'x, N: Normalizer>(
        &'x self,
        environment: &'x Environment<'x, N>,
    ) -> BoxedFuture<'x, Self::Result> {
        Box::pin(async move {
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
                    environment.query(&Self(eq.clone())).await?
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
        })
    }

    fn on_cyclic(
        &self,
        (): Self::InProgress,
        (): Self::InProgress,
        _: &[crate::environment::Call<
            crate::environment::DynArc,
            crate::environment::DynArc,
        >],
    ) -> Self::Result {
        None
    }
}
