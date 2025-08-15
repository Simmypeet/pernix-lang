use std::sync::Arc;

use pernixc_term::{
    lifetime::Lifetime, predicate::Outlives, variance::Variance, visitor,
};

use crate::{
    environment::{Environment, Query},
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
        match self
            .environment
            .query(&Outlives::new(term.clone(), *self.bound))
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
    ///
    /// # Errors
    ///
    /// See [`AbruptError`] for more information.
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
    async fn query(
        &self,
        environment: &Environment<'_, impl Normalizer>,
        (): Self::Parameter,
        (): Self::InProgress,
    ) -> Result<Option<std::sync::Arc<Self::Result>>, Self::Error> {
        let satisfiability = self.operand.outlives_satisfiability(&self.bound);

        if satisfiability == Satisfiability::Satisfied {
            return Ok(Some(Arc::new(Satisfied)));
        }

        // check if all sub-terms are satisfiable
        if satisfiability == Satisfiability::Congruent {
            let mut visitor = Visitor {
                outlives: Ok(Some(Satisfied)),
                bound: &self.bound,
                environment,
            };

            assert!(self
                .operand
                .accept_one_level_async(&mut visitor)
                .await
                .is_ok());

            if visitor.outlives? == Some(Satisfied) {
                return Ok(Some(Arc::new(Satisfied)));
            }
        }

        'outer: for Self { operand: next_operand, bound: next_bound } in
            environment
                .premise()
                .predicates
                .iter()
                .filter_map(|x| T::as_outlives_predicate(x))
        {
            let Some(result) = environment
                .subtypes(
                    self.operand.clone(),
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
                environment.query(&Outlives::new(next_bound, self.bound)),
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
        } in environment.get_equivalences(&self.operand).await?.iter()
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
                environment.query(&Self::new(operand_eq.clone(), self.bound)),
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
        } in environment.get_equivalences(&self.bound).await?.iter()
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
                environment.query(&Self::new(self.operand.clone(), *bound_eq)),
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

// #[cfg(test)]
// mod test;
