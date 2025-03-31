//! Contains the type-system logic of the complier.

use std::{collections::BTreeSet, sync::Arc};

use enum_as_inner::EnumAsInner;
use pernixc_abort::Abort;
use pernixc_handler::Handler;
use pernixc_source_file::Span;
use pernixc_semantic::diagnostic::Diagnostic;
use pernixc_term::{
    constant::Constant,
    lifetime::Lifetime,
    predicate::{Outlives, Predicate},
    r#type::Type,
    Model,
};
use unification::Log;

pub mod compatible;
pub mod deduction;
pub mod definite;
pub mod diagnostic;
pub mod environment;
pub mod equality;
pub mod equivalences;
pub mod mapping;
pub mod normalizer;
pub mod order;
pub mod predicate;
pub mod resolution;
pub mod simplify;
pub mod term;
pub mod type_check;
pub mod unification;
pub mod variance;
pub mod well_formedness;

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
        _: &[Log<M>],
        _: &[Log<M>],
    ) -> Result<Satisfied, M> {
        Ok(Some(Succeeded::satisfied()))
    }
}

impl<M: Model> unification::Predicate<Type<M>> for LifetimeUnifyingPredicate {
    fn unifiable(
        &self,
        _: &Type<M>,
        _: &Type<M>,
        _: &[Log<M>],
        _: &[Log<M>],
    ) -> Result<Satisfied, M> {
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
        _: &[Log<M>],
        _: &[Log<M>],
    ) -> Result<Satisfied, M> {
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
#[error(
    "exceeded the limit of the number of queries; the error hasn't been \
     reported to the user yet as it requires more context"
)]
pub struct OverflowError;

impl OverflowError {
    /// Reports the [`OverflowError`] as a
    /// [`diagnostic::TypeCalculatingOverflow`] to be reported to the user.
    pub fn report_as_type_calculating_overflow(
        self,
        overflow_span: Span,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Abort {
        handler.receive(Box::new(diagnostic::TypeCalculatingOverflow::new(
            overflow_span,
            self,
        )));

        Abort
    }

    /// Reports the [`OverflowError`] as a [`diagnostic::TypeCheckOverflow`] to
    /// be reported to the user.
    pub fn report_as_type_check_overflow(
        self,
        overflow_span: Span,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Abort {
        handler.receive(Box::new(diagnostic::TypeCheckOverflow::new(
            overflow_span,
            self,
        )));

        Abort
    }

    /// Reports the [`OverflowError`] as a [`diagnostic::UndecidablePredicate`]
    /// to be reported to the user.
    pub fn report_as_undecidable_predicate<M: Model>(
        self,
        predicate: Predicate<M>,
        predicate_declaration_span: Option<Span>,
        instantiation_span: Span,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Abort
    where
        Predicate<M>: pernixc_semantic::Display,
    {
        handler.receive(Box::new(diagnostic::UndecidablePredicate::new(
            predicate,
            predicate_declaration_span,
            instantiation_span,
            self,
        )));

        Abort
    }
}

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
    Abort(#[from] Abort),
}

impl Error {
    /// Invokes the closure with the [`OverflowError`] in case the error is an
    /// [`OverflowError`].
    pub fn report_overflow(
        self,
        f: impl FnOnce(OverflowError) -> Abort,
    ) -> Abort {
        match self {
            Self::Overflow(overflow_error) => f(overflow_error),
            Self::Abort(abort) => abort,
        }
    }
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

/// An alias for the result where the Ok variant can be `Option::Some(Succeeded
/// {..})` or `None`.
pub type Result<T, M, E = Error> =
    std::result::Result<Option<Succeeded<T, M>>, E>;

/// An alias for the result where the Ok variant can be
/// `Option::Some(Arc<Succeeded {..})` or `None`.
pub type ResultArc<T, M, E = Error> =
    std::result::Result<Option<Arc<Succeeded<T, M>>>, E>;

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
