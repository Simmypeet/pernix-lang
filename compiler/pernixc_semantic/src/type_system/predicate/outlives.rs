use std::sync::Arc;

use pernixc_table::{DisplayObject, Table};
use serde::{Deserialize, Serialize};

use super::{contains_error, Satisfiability};
use crate::type_system::{
    compatible::Compatibility,
    equivalence::get_equivalences,
    instantiation::{self, Instantiation},
    model::Model,
    normalizer::Normalizer,
    query::Query,
    term::{lifetime::Lifetime, ModelOf, Term},
    variance::Variance,
    visitor, AbruptError, Environment, LifetimeConstraint, Satisfied,
    Succeeded,
};

/// A predicate that a term outlives a lifetime.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Outlives<T: ModelOf> {
    /// The term that must outlive the bound.
    pub operand: T,

    /// The lifetime that the term must outlive.
    pub bound: Lifetime<T::Model>,
}

impl<T: ModelOf> Outlives<T> {
    /// Creates a new outlives predicate.
    #[must_use]
    pub const fn new(operand: T, bound: Lifetime<T::Model>) -> Self {
        Self { operand, bound }
    }
}

impl<T: pernixc_table::Display + Term> pernixc_table::Display for Outlives<T>
where
    Lifetime<T::Model>: pernixc_table::Display,
{
    fn fmt(
        &self,
        table: &Table,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{}: {}",
            DisplayObject { display: &self.operand, table },
            DisplayObject { display: &self.bound, table },
        )
    }
}

impl<T: Term> Outlives<T> {
    /// Checks if the term contains a `forall` lifetime.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        contains_error(&self.operand) || self.bound.is_forall()
    }

    /// Applies a instantiation to the [`Outlives::operand`] and
    /// [`Outlives::bound`].
    pub fn instantiate(&mut self, instantiation: &Instantiation<T::Model>) {
        instantiation::instantiate(&mut self.operand, instantiation);
        instantiation::instantiate(&mut self.bound, instantiation);
    }
}

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
                .premise
                .predicates
                .iter()
                .filter_map(|x| T::as_outlive_predicate(x))
        {
            let Some(Succeeded {
                result:
                    Compatibility {
                        forall_lifetime_instantiations,
                        forall_lifetime_errors,
                    },
                constraints,
            }) = self.operand.compatible(
                next_operand,
                Variance::Covariant,
                environment,
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
        } in get_equivalences(&self.operand, environment)?
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
        } in get_equivalences(&self.bound, environment)?
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

// TODO: bring test back
// #[cfg(test)]
// mod tests;
