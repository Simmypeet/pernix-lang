//! Contains the semantic logic of the compiler (i.e. type checking/system).

use std::collections::BTreeSet;

use enum_as_inner::EnumAsInner;
use environment::Environment;
use observer::Observer;
use predicate::Outlives;
use query::{Cached, Context, Record, Sealed};
use term::constant::Constant;
use unification::Log;

use self::{
    model::Model,
    normalizer::Normalizer,
    predicate::Predicate,
    term::{lifetime::Lifetime, r#type::Type},
};
use crate::{
    arena::ID,
    symbol::{table::State, PositiveTraitImplementation, Trait},
};

pub mod compatible;
pub mod deduction;
pub mod definite;
pub mod environment;
pub mod equality;
pub mod equivalence;
pub mod fresh;
pub mod instantiation;
pub mod mapping;
pub mod matching;
pub mod model;
pub mod normalizer;
pub mod observer;
pub mod order;
pub mod predicate;
pub mod query;
pub mod simplify;
pub mod sub_term;
pub mod term;
pub mod type_check;
pub mod unification;
pub mod variance;
pub mod visitor;

/// A struct implementing the [`unification::Predicate`] that allows the
/// lifetime to be unified.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct LifetimeUnifyingPredicate;

impl<M: Model> unification::Predicate<Lifetime<M>>
    for LifetimeUnifyingPredicate
{
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

impl<M: Model> unification::Predicate<Type<M>> for LifetimeUnifyingPredicate {
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

impl<M: Model> unification::Predicate<Constant<M>>
    for LifetimeUnifyingPredicate
{
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

/// An error that occurs when the number of queries exceeds the limit.
///
/// Due to the fact that the semantic system is partially-decidable, it is
/// possible that the number of queries can be infinite. To prevent this, a
/// limit is set to the number of queries that can be made. However, in
/// most cases, the number of queries should not exceed the limit.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error("exceeded the limit of the number of queries")]
#[allow(missing_docs)]
pub struct OverflowError;

/// A tag type signaling that the predicate/query is satisfied.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Satisfied;

/// Contains constraints related to lifetimes.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum LifetimeConstraint<M: Model> {
    LifetimeOutlives(Outlives<Lifetime<M>>),
}

/// The result of the semantic logic in the success case.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Succeeded<Result, M: Model> {
    /// The result of the query in the success case.
    pub result: Result,

    /// The additional constraints related to lifetimes that must be satisfied
    /// for the query to be considered successful.
    pub constraints: BTreeSet<LifetimeConstraint<M>>,
}

impl<Result: Default, M: Model> Default for Succeeded<Result, M> {
    fn default() -> Self {
        Self { result: Default::default(), constraints: BTreeSet::new() }
    }
}

impl<Result, M: Model> Succeeded<Result, M> {
    /// Creates a new [`Succeeded`] with the given result and no constraints.
    #[must_use]
    pub fn new(result: Result) -> Self {
        Self { result, constraints: BTreeSet::new() }
    }

    /// Creates a new [`Succeeded`] with the given result and constraints.
    #[must_use]
    pub fn with_constraints(
        result: Result,
        constraints: BTreeSet<LifetimeConstraint<M>>,
    ) -> Self {
        Self { result, constraints }
    }

    /// Maps the [`Result`] of the [`Succeeded`] to another type.
    pub fn map<N>(self, f: impl FnOnce(Result) -> N) -> Succeeded<N, M> {
        Succeeded { result: f(self.result), constraints: self.constraints }
    }

    /// Maps the [`Result`] of the [`Succeeded`] to another type with the
    /// possibility of failure.
    pub fn try_map<N, E>(
        self,
        f: impl FnOnce(Result) -> std::result::Result<N, E>,
    ) -> std::result::Result<Succeeded<N, M>, E> {
        Ok(Succeeded { result: f(self.result)?, constraints: self.constraints })
    }
}

impl<M: Model> Succeeded<Satisfied, M> {
    /// Creates a new [`Succeeded`] with the [`Satisfied`] result.
    #[must_use]
    pub fn satisfied() -> Self { Self::new(Satisfied) }

    /// Creates a new [`Succeeded`] with the [`Satisfied`] result and
    /// constraints.
    #[must_use]
    pub fn satisfied_with(
        constraints: BTreeSet<LifetimeConstraint<M>>,
    ) -> Self {
        Self::with_constraints(Satisfied, constraints)
    }

    /// Combines the constraints of two satisified results.
    #[must_use]
    pub fn combine(mut self, other: Self) -> Self {
        self.constraints.extend(other.constraints);
        self
    }
}

/// The type alias for the [`Option`] of [`Succeeded`].
///
/// The [`None`] is returned if failed to prove the query. Otherwise, the
/// [`Succeeded`] is returned with the result and additional constraints.
pub type Output<Result, M> = Option<Succeeded<Result, M>>;

/// Contains the premise of the semantic logic.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Premise<M: Model> {
    /// List of predicates that will be considered as facts.
    pub predicates: BTreeSet<Predicate<M>>,

    /// The extra trait context that has a particular effect on the semantic.
    pub trait_context: TraitContext,
}

/// Extra environment content that has a particular effect on the semantic
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    EnumAsInner,
)]
pub enum TraitContext {
    /// The semantic logic is currently taking place in a trait implementation.
    InTraitImplementation(ID<PositiveTraitImplementation>),

    /// The semantic logic is currently taking place in a trait.
    InTrait(ID<Trait>),

    /// The semantic logic is currently taking place in other than the above.
    #[default]
    Normal,
}

/// A trait used for computing the result of the query.
#[allow(private_bounds)]
pub trait Compute: Sealed {
    /// The error type of the computation
    type Error: From<OverflowError>;

    #[doc(hidden)]
    type Parameter: Default;

    /// The raw implementation of the query.
    ///
    /// The implementation shouldn't include the context's state mutation
    /// (mark_as_done, clear_query).
    #[doc(hidden)]
    #[allow(private_interfaces, private_bounds)]
    fn implementation<S: State>(
        &self,
        environment: &Environment<
            Self::Model,
            S,
            impl Normalizer<Self::Model, S>,
            impl Observer<Self::Model, S>,
        >,
        context: &mut Context<Self::Model>,
        parameter: Self::Parameter,
        in_progress: Self::InProgress,
    ) -> Result<Option<Self::Result>, Self::Error>;

    /// The result to return of the query when the query is cyclic.
    #[doc(hidden)]
    #[allow(private_interfaces, private_bounds)]
    fn on_cyclic(
        &self,
        _: Self::Parameter,
        _: Self::InProgress,
        _: Self::InProgress,
        _: &[Record<Self::Model>],
    ) -> Result<Option<Self::Result>, Self::Error> {
        Ok(None) /* the default implementation is to make the query fails */
    }

    /// Queries the result.
    #[allow(private_interfaces, private_bounds)]
    fn query<S: State>(
        &self,
        environment: &Environment<
            Self::Model,
            S,
            impl Normalizer<Self::Model, S>,
            impl Observer<Self::Model, S>,
        >,
    ) -> Result<Option<Self::Result>, Self::Error> {
        let mut context = Context::default();
        self.query_with_context(environment, &mut context)
    }

    /// Queries the result with the explicitly specified context.
    #[doc(hidden)]
    #[allow(private_interfaces, private_bounds)]
    fn query_with_context<S: State>(
        &self,
        environment: &Environment<
            Self::Model,
            S,
            impl Normalizer<Self::Model, S>,
            impl Observer<Self::Model, S>,
        >,
        context: &mut Context<Self::Model>,
    ) -> Result<Option<Self::Result>, Self::Error> {
        self.query_with_context_full(
            environment,
            context,
            Self::Parameter::default(),
            Self::InProgress::default(),
        )
    }

    /// Queries the result with the explicitly specified context, in-progress
    /// state, and additional parameters.
    #[doc(hidden)]
    #[allow(private_interfaces, private_bounds)]
    fn query_with_context_full<S: State>(
        &self,
        environment: &Environment<
            Self::Model,
            S,
            impl Normalizer<Self::Model, S>,
            impl Observer<Self::Model, S>,
        >,
        context: &mut Context<Self::Model>,
        parameter: Self::Parameter,
        in_progress: Self::InProgress,
    ) -> Result<Option<Self::Result>, Self::Error> {
        match context.mark_as_in_progress(
            self.clone(),
            in_progress.clone(),
            environment,
        )? {
            Some(Cached::Done(result)) => return Ok(Some(result)),
            Some(Cached::InProgress(new_in_progress)) => {
                let position = context
                    .call_stack()
                    .iter()
                    .position(|x| {
                        Self::from_call(x).map_or(false, |x| &x.query == self)
                    })
                    .expect("should exist");

                // circular dependency
                let result = self.on_cyclic(
                    parameter,
                    in_progress,
                    new_in_progress,
                    &context.call_stack()[position..],
                )?;

                return Ok(result);
            }
            None => { /*no circular dependency, continue...*/ }
        }

        let result =
            self.implementation(environment, context, parameter, in_progress);

        match result {
            Ok(Some(result)) => {
                // remembers the result
                assert!(context.mark_as_done(self, result.clone()));
                Ok(Some(result))
            }
            result @ (Ok(None) | Err(_)) => {
                // reset the query state
                assert!(context.clear_query(self).is_some());
                result
            }
        }
    }
}
