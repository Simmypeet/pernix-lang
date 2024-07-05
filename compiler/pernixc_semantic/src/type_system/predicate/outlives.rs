use std::sync::Arc;

use super::{contains_forall_lifetime, Satisfiability};
use crate::{
    type_system::{
        get_equivalences_with_context,
        instantiation::{self, Instantiation},
        mapping::Mapping,
        model::Model,
        normalizer::Normalizer,
        query::Context,
        term::{
            constant::Constant, lifetime::Lifetime, r#type::Type, ModelOf, Term,
        },
        unification::{self, Log, Unification, Unifier},
        visitor, Compute, Environment, LifetimeConstraint, Output,
        OverflowError, Satisfied, Succeeded,
    },
    symbol::table::{self, DisplayObject, State, Table},
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
    pub fn new(operand: T, bound: Lifetime<T::Model>) -> Self {
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
    pub fn contains_forall_lifetime(&self) -> bool {
        contains_forall_lifetime(&self.operand) || self.bound.is_forall()
    }

    /// Applies a instantiation to the [`Outlives::operand`] and
    /// [`Outlives::bound`].
    pub fn instantiate(&mut self, instantiation: &Instantiation<T::Model>) {
        instantiation::instantiate(&mut self.operand, instantiation);
        instantiation::instantiate(&mut self.bound, instantiation);
    }
}

struct Visitor<'a, 'c, T: State, N: Normalizer<M>, M: Model> {
    outlives: Result<Option<Satisfied>, OverflowError>,
    bound: &'a Lifetime<M>,
    environment: &'a Environment<'a, M, T, N>,
    context: &'c mut Context<M>,
}

impl<'a, 'l, 'v, U: Term, T: State, N: Normalizer<U::Model>>
    visitor::Visitor<'v, U> for Visitor<'a, 'l, T, N, U::Model>
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct OutlivesUnifyingConfig;

impl<M: Model> unification::Predicate<Lifetime<M>> for OutlivesUnifyingConfig {
    fn unifiable(
        &self,
        _: &Lifetime<M>,
        _: &Lifetime<M>,
        _: &Vec<Log<M>>,
        _: &Vec<Log<M>>,
    ) -> Result<Output<Satisfied, M>, OverflowError> {
        Ok(Some(Succeeded::satisfied()))
    }
}

impl<M: Model> unification::Predicate<Type<M>> for OutlivesUnifyingConfig {
    fn unifiable(
        &self,
        _: &Type<M>,
        _: &Type<M>,
        _: &Vec<Log<M>>,
        _: &Vec<Log<M>>,
    ) -> Result<Output<Satisfied, M>, OverflowError> {
        Ok(None)
    }
}

impl<M: Model> unification::Predicate<Constant<M>> for OutlivesUnifyingConfig {
    fn unifiable(
        &self,
        _: &Constant<M>,
        _: &Constant<M>,
        _: &Vec<Log<M>>,
        _: &Vec<Log<M>>,
    ) -> Result<Output<Satisfied, M>, OverflowError> {
        Ok(None)
    }
}

impl<M: Model> LifetimeConstraint<M> {
    pub fn satisifies_with_context(
        &self,
        environment: &Environment<M, impl State, impl Normalizer<M>>,
        context: &mut Context<M>,
    ) -> Result<Option<Satisfied>, OverflowError> {
        match self {
            LifetimeConstraint::LifetimeOutlives(outlives) => {
                outlives.query_with_context(environment, context)
            }
            LifetimeConstraint::TypeOutlives(outlives) => {
                outlives.query_with_context(environment, context)
            }
            LifetimeConstraint::LifetimeMatching(lhs, rhs) => {
                if lhs == rhs {
                    return Ok(Some(Satisfied));
                }

                let lhs_outlives_rhs = Outlives::new(lhs.clone(), rhs.clone())
                    .query_with_context(environment, context)?;
                let rhs_outlives_lhs = Outlives::new(rhs.clone(), lhs.clone())
                    .query_with_context(environment, context)?;

                Ok(match (lhs_outlives_rhs, rhs_outlives_lhs) {
                    (Some(Satisfied), Some(Satisfied)) => Some(Satisfied),
                    _ => None,
                })
            }
        }
    }
}

impl<T: Term> Compute for Outlives<T> {
    type Error = OverflowError;
    type Parameter = ();

    #[allow(private_bounds, private_interfaces)]
    fn implementation(
        &self,
        environment: &Environment<
            Self::Model,
            impl State,
            impl Normalizer<Self::Model>,
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

            let _ = self.operand.accept_one_level(&mut visitor);

            if let Some(Satisfied) = visitor.outlives? {
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

            if let Some(Satisfied) =
                Outlives::new(operand_eq, self.bound.clone())
                    .query_with_context(environment, context)?
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

            if let Some(Satisfied) =
                Outlives::new(self.operand.clone(), bound_eq)
                    .query_with_context(environment, context)?
            {
                return Ok(Some(Satisfied));
            }
        }

        for Self { operand: next_operand, bound: next_bound } in environment
            .premise
            .predicates
            .iter()
            .filter_map(|x| T::as_outlive_predicate(x))
        {
            let Some(Succeeded { result, constraints: additional_outlives }) =
                Unification::new(
                    self.operand.clone(),
                    next_operand.clone(),
                    Arc::new(OutlivesUnifyingConfig),
                )
                .query_with_context(environment, context)?
            else {
                continue;
            };

            for constraint in additional_outlives {
                let Some(Satisfied) =
                    constraint.satisifies_with_context(environment, context)?
                else {
                    return Ok(Some(Satisfied));
                };
            }

            if Self::satisfies_by_lifetime_matching(
                result,
                environment,
                context,
            )?
            .is_none()
            {
                continue;
            }

            if let Some(Satisified) =
                Outlives::new(next_bound.clone(), self.bound.clone())
                    .query_with_context(environment, context)?
            {
                return Ok(Some(Satisified));
            }
        }

        Ok(None)
    }
}

impl<T: Term> Outlives<T> {
    fn satisfies_by_lifetime_matching(
        unifier: Unifier<T>,
        environment: &Environment<
            T::Model,
            impl State,
            impl Normalizer<T::Model>,
        >,
        context: &mut Context<T::Model>,
    ) -> Result<Option<Satisfied>, OverflowError> {
        let mapping = Mapping::from_unification(unifier);

        assert!(mapping.types.is_empty());
        assert!(mapping.constants.is_empty());

        for (bound, operands) in mapping.lifetimes {
            for operand in operands {
                let Some(Satisfied) = Outlives::new(bound.clone(), operand)
                    .query_with_context(environment, context)?
                else {
                    return Ok(None);
                };
            }
        }

        Ok(Some(Satisfied))
    }
}

// TODO
// #[cfg(test)]
// mod tests;
