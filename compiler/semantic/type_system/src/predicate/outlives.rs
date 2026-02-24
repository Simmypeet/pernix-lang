//! Implements the [`Query`] for the [`Outlives`]

use std::future::Future;

use pernixc_semantic_element::variance::Variance;
use pernixc_term::{
    constant::Constant,
    instance::Instance,
    lifetime::Lifetime,
    predicate::Outlives,
    r#type::Type,
    visitor::{self, Element},
};

use crate::{
    OverflowError, Satisfiability, Succeeded,
    environment::{BoxedFuture, Environment, Query},
    lifetime_constraint::LifetimeConstraint,
    normalizer::Normalizer,
    term::Term,
};

struct Visitor<'a, 'e, N: Normalizer> {
    outlives: Result<bool, OverflowError>,
    bound: &'a Lifetime,
    environment: &'e Environment<'e, N>,
}

impl<U: Term, N: Normalizer> visitor::AsyncVisitor<U> for Visitor<'_, '_, N> {
    async fn visit(&mut self, term: &U, _: U::Location) -> bool {
        match self
            .environment
            .query(&Outlives::new(term.clone(), self.bound.clone()))
            .await
        {
            Err(err) => {
                self.outlives = Err(err);
                false
            }

            Ok(false) => {
                self.outlives = Ok(false);
                false
            }

            Ok(true) => true,
        }
    }
}

impl LifetimeConstraint {
    /// Checks if this lifetime constraints is satisfiable.
    pub async fn satisfies(
        &self,
        environment: &Environment<'_, impl Normalizer>,
    ) -> Result<bool, OverflowError> {
        match self {
            Self::LifetimeOutlives(outlives) => {
                environment.query(outlives).await
            }
            Self::TypeOutlives(outlives) => environment.query(outlives).await,
        }
    }
}

// TODO: optimize this query to use transitive closure
impl<T: Term> Query for Outlives<T> {
    type InProgress = ();
    type Result = bool;

    #[allow(clippy::too_many_lines)]
    fn query<'x, N: Normalizer>(
        &'x self,
        environment: &'x Environment<'x, N>,
        (): Self::InProgress,
    ) -> BoxedFuture<'x, Self::Result> {
        Box::pin(Impl::outlives(self, environment))
    }

    fn on_cyclic(
        &self,
        _: Self::InProgress,
        _: Self::InProgress,
        _: &[crate::environment::Call<
            crate::environment::DynArc,
            crate::environment::DynArc,
        >],
    ) -> Self::Result {
        false
    }
}

#[doc(hidden)]
pub trait Impl: Sized {
    fn outlives(
        query: &Outlives<Self>,
        environment: &Environment<impl Normalizer>,
    ) -> impl Future<Output = Result<bool, OverflowError>> + Send;
}

impl Impl for Lifetime {
    async fn outlives(
        query: &Outlives<Self>,
        environment: &Environment<'_, impl Normalizer>,
    ) -> Result<bool, OverflowError> {
        let satisfiability =
            query.operand.outlives_satisfiability(&query.bound);

        if satisfiability == Satisfiability::Satisfied {
            return Ok(true);
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

            if environment
                .query(&Outlives::new(next_bound.clone(), query.bound.clone()))
                .await?
            {
                return Ok(true);
            }
        }

        Ok(false)
    }
}

impl Impl for Type {
    #[allow(clippy::too_many_lines)]
    async fn outlives(
        query: &Outlives<Self>,
        environment: &Environment<'_, impl Normalizer>,
    ) -> Result<bool, OverflowError> {
        let satisfiability =
            query.operand.outlives_satisfiability(&query.bound);

        if satisfiability == Satisfiability::Satisfied {
            return Ok(true);
        }

        // check if all sub-terms are satisfiable
        if satisfiability == Satisfiability::Congruent {
            let mut visitor = Visitor {
                outlives: Ok(true),
                bound: &query.bound,
                environment,
            };

            assert!(
                query
                    .operand
                    .accept_one_level_async(&mut visitor)
                    .await
                    .is_ok()
            );

            if visitor.outlives? {
                return Ok(true);
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
                if !constraint.satisfies(environment).await? {
                    continue 'outer;
                }
            }

            let mut next_bound = next_bound.clone();
            result
                .result
                .forall_lifetime_instantiations
                .instantiate(&mut next_bound);

            if environment
                .query(&Outlives::new(next_bound, query.bound.clone()))
                .await?
            {
                return Ok(true);
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
                if !constraint.satisfies(environment).await? {
                    continue 'out;
                }
            }

            if environment
                .query(&Outlives::new(operand_eq.clone(), query.bound.clone()))
                .await?
            {
                return Ok(true);
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
                if !constraint.satisfies(environment).await? {
                    continue 'out;
                }
            }

            if environment
                .query(&Outlives::new(query.operand.clone(), bound_eq.clone()))
                .await?
            {
                return Ok(true);
            }
        }

        Ok(false)
    }
}

impl Impl for Constant {
    async fn outlives(
        _: &Outlives<Self>,
        _: &Environment<'_, impl Normalizer>,
    ) -> Result<bool, OverflowError> {
        Ok(true)
    }
}

impl Impl for Instance {
    async fn outlives(
        _: &Outlives<Self>,
        _: &Environment<'_, impl Normalizer>,
    ) -> Result<bool, OverflowError> {
        Ok(true)
    }
}

#[cfg(test)]
mod test;
