use super::{contains_error, Satisfiability};
use crate::{
    symbol::table::{self, DisplayObject, State, Table},
    type_system::{
        compatible::Compatibility,
        equivalence::get_equivalences_with_context,
        instantiation::{self, Instantiation},
        model::Model,
        normalizer::Normalizer,
        observer::Observer,
        query::Context,
        term::{lifetime::Lifetime, ModelOf, Term},
        variance::Variance,
        visitor, Compute, Environment, LifetimeConstraint, OverflowError,
        Satisfied, Succeeded,
    },
};

/// A predicate that a term outlives a lifetime.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

impl<S: State, T: table::Display<S> + Term> table::Display<S> for Outlives<T>
where
    Lifetime<T::Model>: table::Display<S>,
{
    fn fmt(
        &self,
        table: &Table<S>,
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

struct Visitor<
    'a,
    'c,
    T: State,
    N: Normalizer<M, T>,
    O: Observer<M, T>,
    M: Model,
> {
    outlives: Result<Option<Satisfied>, OverflowError>,
    bound: &'a Lifetime<M>,
    environment: &'a Environment<'a, M, T, N, O>,
    context: &'c mut Context<M>,
}

impl<
        'a,
        'l,
        'v,
        U: Term,
        T: State,
        N: Normalizer<U::Model, T>,
        O: Observer<U::Model, T>,
    > visitor::Visitor<'v, U> for Visitor<'a, 'l, T, N, O, U::Model>
{
    fn visit(&mut self, term: &'v U, _: U::Location) -> bool {
        match Outlives::new(term.clone(), self.bound.clone())
            .query_with_context(self.environment, self.context)
        {
            result @ (Err(_) | Ok(None)) => {
                self.outlives = result;
                false
            }
            Ok(Some(Satisfied)) => true,
        }
    }
}

impl<M: Model> LifetimeConstraint<M> {
    pub(super) fn satisifies_with_context<S: State>(
        &self,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
        context: &mut Context<M>,
    ) -> Result<Option<Satisfied>, OverflowError> {
        match self {
            Self::LifetimeOutlives(outlives) => {
                outlives.query_with_context(environment, context)
            }
        }
    }
}

impl<T: Term> Compute for Outlives<T> {
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
        let satisfiability = self.operand.outlives_satisfiability(&self.bound);

        if satisfiability == Satisfiability::Satisfied {
            return Ok(Some(Satisfied));
        }

        // check if all sub-terms are satisfiable
        if satisfiability == Satisfiability::Congruent {
            let mut visitor = Visitor {
                outlives: Ok(Some(Satisfied)),
                bound: &self.bound,
                environment,
                context,
            };

            assert!(self.operand.accept_one_level(&mut visitor).is_ok());

            if visitor.outlives? == Some(Satisfied) {
                return Ok(Some(Satisfied));
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
            }) = self.operand.compatible_with_context(
                next_operand,
                Variance::Covariant,
                environment,
                context,
            )?
            else {
                continue;
            };

            if !forall_lifetime_errors.is_empty() {
                continue;
            }

            for constraint in constraints {
                let Some(Satisfied) =
                    constraint.satisifies_with_context(environment, context)?
                else {
                    continue 'outer;
                };
            }

            let mut next_bound = next_bound.clone();
            forall_lifetime_instantiations.instantiate(&mut next_bound);

            if Outlives::new(next_bound, self.bound.clone())
                .query_with_context(environment, context)?
                == Some(Satisfied)
            {
                return Ok(Some(Satisfied));
            }
        }

        // look for operand equivalences
        for Succeeded {
            result: operand_eq,
            constraints: additional_outlives,
        } in
            get_equivalences_with_context(&self.operand, environment, context)?
        {
            // additional constraints must be satisifed
            for constraint in additional_outlives {
                let Some(Satisfied) =
                    constraint.satisifies_with_context(environment, context)?
                else {
                    return Ok(Some(Satisfied));
                };
            }

            if Self::new(operand_eq, self.bound.clone())
                .query_with_context(environment, context)?
                == Some(Satisfied)
            {
                return Ok(Some(Satisfied));
            }
        }

        // look for bound equivalences
        for Succeeded { result: bound_eq, constraints: additional_outlives } in
            get_equivalences_with_context(&self.bound, environment, context)?
        {
            // additional constraints must be satisified
            for constraint in additional_outlives {
                let Some(Satisfied) =
                    constraint.satisifies_with_context(environment, context)?
                else {
                    return Ok(Some(Satisfied));
                };
            }

            if Self::new(self.operand.clone(), bound_eq)
                .query_with_context(environment, context)?
                == Some(Satisfied)
            {
                return Ok(Some(Satisfied));
            }
        }

        Ok(None)
    }
}

#[cfg(test)]
mod tests;
