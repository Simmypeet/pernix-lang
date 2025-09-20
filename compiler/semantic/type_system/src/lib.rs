//! Contains the type-system logic of the complier.

use std::{collections::BTreeSet, sync::Arc};

use enum_as_inner::EnumAsInner;
use pernixc_query::runtime::executor::CyclicError;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::lifetime_constraint::LifetimeConstraint;

pub mod adt_fields;
pub mod deduction;
pub mod definite;
pub mod diagnostic;
pub mod environment;
pub mod equality;
pub mod equivalence;
pub mod lifetime_constraint;
pub mod mapping;
pub mod normalizer;
pub mod order;
pub mod predicate;
pub mod resolution;
pub mod simplify;
pub mod subtype;
pub mod term;
pub mod unification;
pub mod variance;
pub mod wf_check;

/// An error that occurs when the number of queries exceeds the limit.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    thiserror::Error,
)]
#[error(
    "exceeded the limit of the number of queries; the error hasn't been \
     reported to the user yet as it requires more context"
)]
pub struct OverflowError;

/// A common abrupt error that aborts the query and returns the error.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    thiserror::Error,
    EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Error {
    #[error(transparent)]
    Overflow(#[from] OverflowError),

    #[error(transparent)]
    CyclicDependency(#[from] CyclicError),
}

/// A tag type signaling that the predicate/query is satisfied.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Satisfied;

/// The result of the semantic logic in the success case.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Succeeded<Result> {
    /// The result of the query in the success case.
    pub result: Result,
    /// The additional constraints related to lifetimes that must be satisfied
    /// for the query to be considered successful.
    pub constraints: BTreeSet<LifetimeConstraint>,
}

impl<Result: Default> Default for Succeeded<Result> {
    fn default() -> Self {
        Self { result: Default::default(), constraints: BTreeSet::new() }
    }
}

impl<Result> Succeeded<Result> {
    /// Creates a new [`Succeeded`] with the given result and no constraints.
    #[must_use]
    pub const fn new(result: Result) -> Self {
        Self { result, constraints: BTreeSet::new() }
    }

    /// Creates a new [`Succeeded`] with the given result and constraints.
    #[must_use]
    pub const fn with_constraints(
        result: Result,
        constraints: BTreeSet<LifetimeConstraint>,
    ) -> Self {
        Self { result, constraints }
    }

    /// Maps the [`Result`] of the [`Succeeded`] to another type.
    pub fn map<N>(self, f: impl FnOnce(Result) -> N) -> Succeeded<N> {
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
    ) -> std::result::Result<Succeeded<N>, E> {
        Ok(Succeeded { result: f(self.result)?, constraints: self.constraints })
    }
}

impl Succeeded<Satisfied> {
    /// Creates a new [`Succeeded`] with the [`Satisfied`] result.
    #[must_use]
    pub const fn satisfied() -> Self { Self::new(Satisfied) }

    /// Creates a new [`Succeeded`] with the [`Satisfied`] result and
    /// constraints.
    #[must_use]
    pub const fn satisfied_with(
        constraints: BTreeSet<LifetimeConstraint>,
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

/// An alias for the result where the Ok variant can be `Option::Some(Succeeded
/// {..})` or `None`.
pub type Result<T, E = Error> = std::result::Result<Option<Succeeded<T>>, E>;

/// An alias for the result where the Ok variant can be
/// `Option::Some(Arc<Succeeded {..})` or `None`.
pub type ResultArc<T, E = Error> =
    std::result::Result<Option<Arc<Succeeded<T>>>, E>;

/// Describes a satisfiability of a certain predicate.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Satisfiability {
    /// The predicate is satisfiable.
    Satisfied,

    /// The predicate is unsatisfiable.
    Unsatisfied,

    /// If all the sub-term of the predicate are satisfiable, then the
    /// predicate is satisfiable.
    Congruent,
}

#[cfg(test)]
mod test;
