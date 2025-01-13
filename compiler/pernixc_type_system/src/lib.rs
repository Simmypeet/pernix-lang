//! Contains the type-system logic of the complier.

use std::collections::BTreeSet;

use enum_as_inner::EnumAsInner;
use pernixc_table::query::CyclicDependency;
use pernixc_term::{lifetime::Lifetime, predicate::Outlives, Model};

pub mod environment;
pub mod equality;
pub mod normalizer;
pub mod term;

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

/// A common abrupt error that aborts the query and returns the error.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    thiserror::Error,
    EnumAsInner,
)]
#[allow(missing_docs)]
pub enum AbruptError {
    #[error(transparent)]
    Overflow(#[from] OverflowError),

    #[error(transparent)]
    CyclicDependency(#[from] CyclicDependency),
}

/// A tag type signaling that the predicate/query is satisfied.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Satisfied;

/// Contains constraints related to lifetimes.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
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
    pub const fn new(result: Result) -> Self {
        Self { result, constraints: BTreeSet::new() }
    }

    /// Creates a new [`Succeeded`] with the given result and constraints.
    #[must_use]
    pub const fn with_constraints(
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
    ///
    /// # Errors
    ///
    /// Returns error whatever the closure returns an error.
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
    pub const fn satisfied() -> Self { Self::new(Satisfied) }

    /// Creates a new [`Succeeded`] with the [`Satisfied`] result and
    /// constraints.
    #[must_use]
    pub const fn satisfied_with(
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
