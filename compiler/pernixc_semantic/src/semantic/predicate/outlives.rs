use super::contains_forall_lifetime;
use crate::{
    semantic::{
        get_equivalences_impl,
        instantiation::{self, Instantiation},
        mapping::Mapping,
        model::Model,
        normalizer::Normalizer,
        predicate::Satisfiability,
        session::{self, Cached, Limit, Session},
        term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
        unification::{self, Unification},
        visitor, Environment, ExceedLimitError, Satisfied,
    },
    symbol::table::{self, DisplayObject, State, Table},
};

/// A query for checking [`Outlives`] predicate satisfiability.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Query<'a, T: Term> {
    /// The term that must outlive the bound.
    pub operand: &'a T,

    /// The lifetime that the term must outlive.
    pub bound: &'a Lifetime<T::Model>,
}

/// A predicate that a term outlives a lifetime.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Outlives<T: Term> {
    /// The term that must outlive the bound.
    pub operand: T,

    /// The lifetime that the term must outlive.
    pub bound: Lifetime<T::Model>,
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

struct Visitor<
    'a,
    'l,
    T: State,
    N: Normalizer<M>,
    R: Session<Lifetime<M>> + Session<Type<M>> + Session<Constant<M>>,
    M: Model,
> {
    outlives: Result<bool, ExceedLimitError>,
    bound: &'a Lifetime<M>,
    environment: &'a Environment<'a, M, T, N>,
    limit: &'l mut Limit<R>,
}

impl<
        'a,
        'l,
        'v,
        U: Term,
        T: State,
        N: Normalizer<U::Model>,
        R: Session<U>
            + Session<Lifetime<U::Model>>
            + Session<Type<U::Model>>
            + Session<Constant<U::Model>>,
    > visitor::Visitor<'v, U> for Visitor<'a, 'l, T, N, R, U::Model>
{
    fn visit(&mut self, term: &'v U, _: U::Location) -> bool {
        match Outlives::satisfies_impl(
            term,
            self.bound,
            self.environment,
            self.limit,
        ) {
            result @ (Err(_) | Ok(false)) => {
                self.outlives = result;
                false
            }
            Ok(true) => true,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct OutlivesUnifyingConfig;

impl<M: Model> unification::Config<Lifetime<M>> for OutlivesUnifyingConfig {
    fn unifiable(
        &mut self,
        _: &Lifetime<M>,
        _: &Lifetime<M>,
    ) -> Result<bool, ExceedLimitError> {
        Ok(true)
    }
}

impl<M: Model> unification::Config<Type<M>> for OutlivesUnifyingConfig {
    fn unifiable(
        &mut self,
        _: &Type<M>,
        _: &Type<M>,
    ) -> Result<bool, ExceedLimitError> {
        Ok(false)
    }
}

impl<M: Model> unification::Config<Constant<M>> for OutlivesUnifyingConfig {
    fn unifiable(
        &mut self,
        _: &Constant<M>,
        _: &Constant<M>,
    ) -> Result<bool, ExceedLimitError> {
        Ok(false)
    }
}

impl<T: Term> Outlives<T> {
    fn satisfies_by_lifetime_matching(
        unification: Unification<T>,
        environment: &Environment<
            T::Model,
            impl State,
            impl Normalizer<T::Model>,
        >,
        limit: &mut Limit<
            impl Session<Lifetime<T::Model>>
                + Session<Type<T::Model>>
                + Session<Constant<T::Model>>,
        >,
    ) -> Result<bool, ExceedLimitError> {
        let mapping = Mapping::from_unification(unification);

        assert!(mapping.types.is_empty());
        assert!(mapping.constants.is_empty());

        for (bound, operands) in mapping.lifetimes {
            for operand in operands {
                if !Outlives::satisfies_impl(
                    &bound,
                    &operand,
                    environment,
                    limit,
                )? {
                    return Ok(false);
                }
            }
        }

        Ok(true)
    }

    /// Determines whether a predicate of the term and the bound is satisfiable.
    ///
    /// # Errors
    ///
    /// See [`ExceedLimitError`] for more information.
    pub fn satisfies(
        operand: &T,
        bound: &Lifetime<T::Model>,
        environment: &Environment<
            T::Model,
            impl State,
            impl Normalizer<T::Model>,
        >,
    ) -> Result<bool, ExceedLimitError> {
        let mut limit = Limit::<session::Default<_>>::default();

        Self::satisfies_impl(operand, bound, environment, &mut limit)
    }

    pub(in crate::semantic) fn satisfies_impl(
        operand: &T,
        bound: &Lifetime<T::Model>,
        environment: &Environment<
            T::Model,
            impl State,
            impl Normalizer<T::Model>,
        >,
        limit: &mut Limit<
            impl Session<Lifetime<T::Model>>
                + Session<Type<T::Model>>
                + Session<Constant<T::Model>>,
        >,
    ) -> Result<bool, ExceedLimitError> {
        let satisfiability =
            operand.outlives_satisfiability(bound, environment, limit)?;

        if satisfiability == Satisfiability::Satisfied {
            return Ok(true);
        }

        match limit.mark_as_in_progress::<T, _>(Query { operand, bound }, ())? {
            Some(Cached::Done(Satisfied)) => return Ok(true),
            Some(Cached::InProgress(())) => return Ok(false),
            None => {}
        }

        // check if all sub-terms are satisfiable
        if satisfiability == Satisfiability::Congruent {
            let mut visitor =
                Visitor { outlives: Ok(true), bound, environment, limit };

            let _ = operand.accept_one_level(&mut visitor);

            if visitor.outlives? {
                limit.mark_as_done::<T, _>(Query { operand, bound }, Satisfied);
                return Ok(true);
            }
        }

        // look for operand equivalences
        for operand_eq in get_equivalences_impl(operand, environment, limit)? {
            if Self::satisfies_impl(&operand_eq, bound, environment, limit)? {
                limit.mark_as_done::<T, _>(Query { operand, bound }, Satisfied);
                return Ok(true);
            }
        }

        // look for bound equivalences
        for bound_eq in get_equivalences_impl(bound, environment, limit)? {
            if Self::satisfies_impl(operand, &bound_eq, environment, limit)? {
                limit.mark_as_done::<T, _>(Query { operand, bound }, Satisfied);
                return Ok(true);
            }
        }

        for Self { operand: next_operand, bound: next_bound } in environment
            .premise
            .predicates
            .iter()
            .filter_map(|x| T::as_outlive_predicate(x))
        {
            let Some(unification) = unification::unify_impl(
                operand,
                next_operand,
                &mut OutlivesUnifyingConfig,
                environment,
                limit,
            )?
            else {
                continue;
            };

            if !Self::satisfies_by_lifetime_matching(
                unification,
                environment,
                limit,
            )? {
                continue;
            }

            if Outlives::satisfies_impl(next_bound, bound, environment, limit)?
            {
                limit.mark_as_done::<T, _>(Query { operand, bound }, Satisfied);
                return Ok(true);
            }
        }

        limit.clear_query::<T, _>(Query { operand, bound });
        Ok(false)
    }
}

#[cfg(test)]
mod tests;
