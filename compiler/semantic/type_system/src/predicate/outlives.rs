//! Implements the [`Query`] for the [`Outlives`]

use std::{future::Future, sync::Arc};

use pernixc_term::{
    constant::Constant,
    lifetime::Lifetime,
    predicate::Outlives,
    r#type::Type,
    variance::Variance,
    visitor::{self, Element},
};

use crate::{
    environment::{BoxedFuture, Environment, Query},
    normalizer::Normalizer,
    term::Term,
    Error, LifetimeConstraint, Satisfiability, Satisfied, Succeeded,
};

struct Visitor<'a, 'e, N: Normalizer> {
    outlives: Result<Option<Satisfied>, Error>,
    bound: &'a Lifetime,
    environment: &'e Environment<'e, N>,
}

impl<U: Term, N: Normalizer> visitor::AsyncVisitor<U> for Visitor<'_, '_, N> {
    async fn visit(&mut self, term: &U, _: U::Location) -> bool {
        match Box::pin(
            self.environment.query(&Outlives::new(term.clone(), *self.bound)),
        )
        .await
        {
            Err(err) => {
                self.outlives = Err(err);
                false
            }

            Ok(None) => {
                self.outlives = Ok(None);
                false
            }

            Ok(Some(_)) => true,
        }
    }
}

impl LifetimeConstraint {
    /// Checks if this lifetime constraints is satisfiable.
    pub async fn satisfies(
        &self,
        environment: &Environment<'_, impl Normalizer>,
    ) -> Result<Option<Arc<Satisfied>>, Error> {
        match self {
            Self::LifetimeOutlives(outlives) => {
                environment.query(outlives).await
            }
        }
    }
}

// TODO: optimize this query to use transitive closure
impl<T: Term> Query for Outlives<T> {
    type Parameter = ();
    type InProgress = ();
    type Result = Satisfied;
    type Error = Error;

    #[allow(clippy::too_many_lines)]
    fn query<'x, N: Normalizer>(
        &'x self,
        environment: &'x Environment<'x, N>,
        (): Self::Parameter,
        (): Self::InProgress,
    ) -> BoxedFuture<'x, Self::Result, Self::Error> {
        Box::pin(Impl::outlives(self, environment))
    }
}

#[doc(hidden)]
pub trait Impl: Sized {
    fn outlives(
        query: &Outlives<Self>,
        environment: &Environment<impl Normalizer>,
    ) -> impl Future<Output = Result<Option<Arc<Satisfied>>, Error>> + Send;
}

impl Impl for Lifetime {
    async fn outlives(
        query: &Outlives<Self>,
        environment: &Environment<'_, impl Normalizer>,
    ) -> Result<Option<Arc<Satisfied>>, Error> {
        let satisfiability =
            query.operand.outlives_satisfiability(&query.bound);

        if satisfiability == Satisfiability::Satisfied {
            return Ok(Some(Arc::new(Satisfied)));
        }

        for Outlives { operand: next_operand, bound: next_bound } in environment
            .premise()
            .predicates
            .iter()
            .filter_map(|x| x.as_lifetime_outlives())
        {
            if *next_operand != query.operand {
                continue;
            }

            if let Some(result) = Box::pin(
                environment.query(&Outlives::new(*next_bound, query.bound)),
            )
            .await?
            {
                return Ok(Some(result));
            }
        }

        Ok(None)
    }
}

impl Impl for Type {
    #[allow(clippy::too_many_lines)]
    async fn outlives(
        query: &Outlives<Self>,
        environment: &Environment<'_, impl Normalizer>,
    ) -> Result<Option<Arc<Satisfied>>, Error> {
        let satisfiability =
            query.operand.outlives_satisfiability(&query.bound);

        if satisfiability == Satisfiability::Satisfied {
            return Ok(Some(Arc::new(Satisfied)));
        }

        // check if all sub-terms are satisfiable
        if satisfiability == Satisfiability::Congruent {
            let mut visitor = Visitor {
                outlives: Ok(Some(Satisfied)),
                bound: &query.bound,
                environment,
            };

            assert!(query
                .operand
                .accept_one_level_async(&mut visitor)
                .await
                .is_ok());

            if visitor.outlives? == Some(Satisfied) {
                return Ok(Some(Arc::new(Satisfied)));
            }
        }

        'outer: for Outlives { operand: next_operand, bound: next_bound } in
            environment
                .premise()
                .predicates
                .iter()
                .filter_map(|x| x.as_type_outlives())
        {
            let Some(result) = environment
                .subtypes(
                    query.operand.clone(),
                    next_operand.clone(),
                    Variance::Covariant,
                )
                .await?
            else {
                continue;
            };

            if !result.result.forall_lifetime_errors.is_empty() {
                continue;
            }

            for constraint in &result.constraints {
                let Some(_) =
                    Box::pin(constraint.satisfies(environment)).await?
                else {
                    continue 'outer;
                };
            }

            let mut next_bound = *next_bound;
            result
                .result
                .forall_lifetime_instantiations
                .instantiate(&mut next_bound);

            if Box::pin(
                environment.query(&Outlives::new(next_bound, query.bound)),
            )
            .await?
            .is_some()
            {
                return Ok(Some(Arc::new(Satisfied)));
            }
        }

        // look for operand equivalences
        'out: for Succeeded {
            result: operand_eq,
            constraints: additional_outlives,
        } in
            environment.get_equivalences(&query.operand).await?.iter()
        {
            // additional constraints must be satisifed
            for constraint in additional_outlives {
                let Some(_) =
                    Box::pin(constraint.satisfies(environment)).await?
                else {
                    continue 'out;
                };
            }

            if Box::pin(
                environment
                    .query(&Outlives::new(operand_eq.clone(), query.bound)),
            )
            .await?
            .is_some()
            {
                return Ok(Some(Arc::new(Satisfied)));
            }
        }

        // look for bound equivalences
        'out: for Succeeded {
            result: bound_eq,
            constraints: additional_outlives,
        } in environment.get_equivalences(&query.bound).await?.iter()
        {
            // additional constraints must be satisified
            for constraint in additional_outlives {
                let Some(_) =
                    Box::pin(constraint.satisfies(environment)).await?
                else {
                    continue 'out;
                };
            }

            if Box::pin(
                environment
                    .query(&Outlives::new(query.operand.clone(), *bound_eq)),
            )
            .await?
            .is_some()
            {
                return Ok(Some(Arc::new(Satisfied)));
            }
        }

        Ok(None)
    }
}

impl Impl for Constant {
    async fn outlives(
        _: &Outlives<Self>,
        _: &Environment<'_, impl Normalizer>,
    ) -> Result<Option<Arc<Satisfied>>, Error> {
        Ok(Some(Arc::new(Satisfied)))
    }
}

#[cfg(test)]
mod test;
