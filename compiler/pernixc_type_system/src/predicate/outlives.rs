use std::sync::Arc;

use pernixc_term::{
    lifetime::Lifetime, predicate::Outlives, variance::Variance, visitor, Model,
};

use crate::{
    compatible::Compatibility,
    environment::{Environment, Query},
    normalizer::Normalizer,
    term::Term,
    AbruptError, LifetimeConstraint, Satisfiability, Satisfied, Succeeded,
};

struct Visitor<'a, 'e, N: Normalizer<M>, M: Model> {
    outlives: Result<Option<Satisfied>, AbruptError>,
    bound: &'a Lifetime<M>,
    environment: &'e Environment<'e, M, N>,
}

impl<'a, 'e, 'v, U: Term, N: Normalizer<U::Model>> visitor::Visitor<'v, U>
    for Visitor<'a, 'e, N, U::Model>
{
    fn visit(&mut self, term: &'v U, _: U::Location) -> bool {
        match self
            .environment
            .query(&Outlives::new(term.clone(), self.bound.clone()))
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

impl<M: Model> LifetimeConstraint<M> {
    /// Checks if this lifetime constraints is satisfiable.
    ///
    /// # Errors
    ///
    /// See [`AbruptError`] for more information.
    pub fn satisfies(
        &self,
        environment: &Environment<M, impl Normalizer<M>>,
    ) -> Result<Option<Arc<Satisfied>>, AbruptError> {
        match self {
            Self::LifetimeOutlives(outlives) => environment.query(outlives),
        }
    }
}

impl<T: Term> Query for Outlives<T> {
    type Model = T::Model;
    type Parameter = ();
    type InProgress = ();
    type Result = Satisfied;
    type Error = AbruptError;

    fn query(
        &self,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
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

            assert!(self.operand.accept_one_level(&mut visitor).is_ok());

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
            let Some(Succeeded {
                result:
                    Compatibility {
                        forall_lifetime_instantiations,
                        forall_lifetime_errors,
                    },
                constraints,
            }) = environment.compatible(
                &self.operand,
                next_operand,
                Variance::Covariant,
            )?
            else {
                continue;
            };

            if !forall_lifetime_errors.is_empty() {
                continue;
            }

            for constraint in constraints {
                let Some(_) = constraint.satisfies(environment)? else {
                    continue 'outer;
                };
            }

            let mut next_bound = next_bound.clone();
            forall_lifetime_instantiations.instantiate(&mut next_bound);

            if environment
                .query(&Outlives::new(next_bound, self.bound.clone()))?
                .is_some()
            {
                return Ok(Some(Arc::new(Satisfied)));
            }
        }

        // look for operand equivalences
        'out: for Succeeded {
            result: operand_eq,
            constraints: additional_outlives,
        } in environment.get_equivalences(&self.operand)?
        {
            // additional constraints must be satisifed
            for constraint in additional_outlives {
                let Some(_) = constraint.satisfies(environment)? else {
                    continue 'out;
                };
            }

            if environment
                .query(&Self::new(operand_eq, self.bound.clone()))?
                .is_some()
            {
                return Ok(Some(Arc::new(Satisfied)));
            }
        }

        // look for bound equivalences
        'out: for Succeeded {
            result: bound_eq,
            constraints: additional_outlives,
        } in environment.get_equivalences(&self.bound)?
        {
            // additional constraints must be satisified
            for constraint in additional_outlives {
                let Some(_) = constraint.satisfies(environment)? else {
                    continue 'out;
                };
            }

            if environment
                .query(&Self::new(self.operand.clone(), bound_eq))?
                .is_some()
            {
                return Ok(Some(Arc::new(Satisfied)));
            }
        }

        Ok(None)
    }
}

#[cfg(test)]
mod test;
