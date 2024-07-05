//! Contains the semantic logic of the compiler (i.e. type checking/system).

use std::collections::HashSet;

use enum_as_inner::EnumAsInner;
use getset::Getters;
use predicate::Outlives;
use query::{Cached, Context, Query, QueryCall};

use self::{
    model::Model,
    normalizer::Normalizer,
    predicate::Predicate,
    term::{lifetime::Lifetime, r#type::Type, Term},
};
use crate::{
    arena::ID,
    symbol::{
        table::{State, Table},
        PositiveTraitImplementation, Trait,
    },
};

pub mod compatible;
pub mod deduction;
pub mod definite;
pub mod equality;
pub mod fresh;
pub mod instantiation;
pub mod mapping;
pub mod matching;
pub mod model;
pub mod normalizer;
pub mod order;
pub mod predicate;
pub mod query;
// pub mod requirement;
// pub mod simplify;
pub mod sub_term;
pub mod term;
pub mod type_check;
pub mod unification;
pub mod visitor;

/// An error that occurs when the number of queries exceeds the limit.
///
/// Due to the fact that the semantic system is partially-decidable, it is
/// possible that the number of queries can be infinite. To prevent this, a
/// limit is set to the number of queries that can be made. However, in most
/// cases, the number of queries should not exceed the limit.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error("exceeded the limit of the number of queries")]
#[allow(missing_docs)]
pub struct OverflowError;

/// A tag type signaling that the predicate/query is satisfied.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Satisfied;

/// Contains constraints related to lifetimes.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum LifetimeConstraint<M: Model> {
    LifetimeOutlives(Outlives<Lifetime<M>>),
    TypeOutlives(Outlives<Type<M>>),
    LifetimeMatching(Lifetime<M>, Lifetime<M>),
}

/// The result of the semantic logic in the success case.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Succeeded<Result, M: Model> {
    /// The result of the query in the success case.
    pub result: Result,

    /// The additional outlives constraints that must be satisfied for the
    /// query to be considered successful.
    pub constraints: HashSet<LifetimeConstraint<M>>,
}

impl<Result, M: Model> Succeeded<Result, M> {
    /// Creates a new [`Succeeded`] with the given result and no constraints.
    #[must_use]
    pub fn new(result: Result) -> Self {
        Self { result, constraints: HashSet::new() }
    }

    /// Creates a new [`Succeeded`] with the given result and constraints.
    #[must_use]
    pub fn with_constraints(
        result: Result,
        constraints: HashSet<LifetimeConstraint<M>>,
    ) -> Self {
        Self { result, constraints }
    }
}

impl<M: Model> Succeeded<Satisfied, M> {
    /// Creates a new [`Succeeded`] with the [`Satisfied`] result.
    #[must_use]
    pub fn satisfied() -> Self { Self::new(Satisfied) }

    /// Creates a new [`Succeeded`] with the [`Satisfied`] result and
    /// constraints.
    #[must_use]
    pub fn satisfied_with(constraints: HashSet<LifetimeConstraint<M>>) -> Self {
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
pub type Output<Result, M: Model> = Option<Succeeded<Result, M>>;

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

/// The foundation truth used to derive further arguments.
#[derive(Debug, Clone, Default, Getters)]
pub struct Premise<M: Model> {
    /// The list of predicates
    #[get = "pub"]
    predicates: HashSet<Predicate<M>>,

    /// The environment of the premise.
    pub trait_context: TraitContext,
}

impl<M: Model> Premise<M> {
    /// Appends the given predicates to the premise.
    pub fn append_from_predicates(
        &mut self,
        predicates: impl Iterator<Item = Predicate<M>>,
    ) {
        self.predicates.extend(predicates);
    }

    /// Creates a new [`Premise`] with the given predicates.
    pub fn from_predicates(
        predicates: impl Iterator<Item = Predicate<M>>,
    ) -> Self {
        let mut premise = Self::default();
        premise.append_from_predicates(predicates);
        premise
    }
}

/// A structure that contains the environment of the semantic logic.
#[derive(Debug, Clone, Copy)]
pub struct Environment<'a, M: Model, T: State, N: Normalizer<M>> {
    /// The premise of the semantic logic.
    pub premise: &'a Premise<M>,

    /// The table that contains the information of symbols.
    pub table: &'a Table<T>,

    /// The normalizer used to normalize the inference variables.
    pub normalizer: &'a N,
}

/// Gets the list of equivalent terms for the given term.
///
/// This including normalized term and equivalent classes.
///
/// # Errors
///
/// See [`ExceedLimitError`] for more information.
pub fn get_equivalences<T: Term>(
    term: &T,
    environment: &Environment<T::Model, impl State, impl Normalizer<T::Model>>,
) -> Result<Vec<Succeeded<T, T::Model>>, OverflowError> {
    let mut context = Context::default();
    get_equivalences_with_context(term, environment, &mut context)
}

fn get_equivalences_with_context<T: Term>(
    term: &T,
    environment: &Environment<T::Model, impl State, impl Normalizer<T::Model>>,
    context: &mut Context<T::Model>,
) -> Result<Vec<Succeeded<T, T::Model>>, OverflowError> {
    let mut equivalences = (term.normalize(environment, context)?)
        .map_or_else(Vec::new, |result| vec![result]);

    todo!();

    Ok(equivalences)
}

#[allow(private_bounds)]
pub trait Compute: Query {
    type Error: From<OverflowError>;
    type Parameter: Default;

    /// The raw implementation of the query.
    ///
    /// The implementation shouldn't include the context's state mutation
    /// (mark_as_done, clear_query).
    #[doc(hidden)]
    #[allow(private_interfaces, private_bounds)]
    fn implementation(
        &self,
        environment: &Environment<
            Self::Model,
            impl State,
            impl Normalizer<Self::Model>,
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
        _: &Environment<Self::Model, impl State, impl Normalizer<Self::Model>>,
        _: &mut Context<Self::Model>,
        _: Self::Parameter,
        _: Self::InProgress,
        _: Self::InProgress,
        _: &[QueryCall<Self::Model>],
    ) -> Result<Option<Self::Result>, Self::Error> {
        Ok(None) /* the default implementation is to make the query fails */
    }

    /// Queries the result.
    #[allow(private_interfaces, private_bounds)]
    fn query(
        &self,
        environment: &Environment<
            Self::Model,
            impl State,
            impl Normalizer<Self::Model>,
        >,
    ) -> Result<Option<Self::Result>, Self::Error> {
        let mut context = Context::default();
        self.query_with_context(environment, &mut context)
    }

    /// Queries the result with the explicitly specified context.
    #[doc(hidden)]
    #[allow(private_interfaces, private_bounds)]
    fn query_with_context(
        &self,
        environment: &Environment<
            Self::Model,
            impl State,
            impl Normalizer<Self::Model>,
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
    fn query_with_context_full(
        &self,
        environment: &Environment<
            Self::Model,
            impl State,
            impl Normalizer<Self::Model>,
        >,
        context: &mut Context<Self::Model>,
        parameter: Self::Parameter,
        in_progress: Self::InProgress,
    ) -> Result<Option<Self::Result>, Self::Error> {
        match context.mark_as_in_progress(self.clone(), in_progress.clone())? {
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
                let call_stack = context.call_stack()[position..].to_vec();
                let result = self.on_cyclic(
                    environment,
                    context,
                    parameter,
                    in_progress,
                    new_in_progress,
                    &call_stack,
                )?;

                context.clear_query(self);

                return Ok(result);
            }
            None => { /*no circular dependency, continue...*/ }
        }

        let result =
            self.implementation(environment, context, parameter, in_progress);

        match result {
            Ok(Some(result)) => {
                assert!(context.mark_as_done(self, result.clone()));
                Ok(Some(result))
            }
            result @ (Ok(None) | Err(_)) => {
                assert!(context.clear_query(self).is_some());
                result
            }
        }
    }
}
