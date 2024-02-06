//! Contains the definition of [`Location`].

use std::{fmt::Debug, hash::Hash};

use crate::{
    symbol::Variance,
    table::{State, Table},
};

/// An error that occurs when assigning a sub-term to a term.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum AssignSubTermError {
    #[error("the given location is invalid for this term")]
    InvalidLocation,
    #[error("the given term to assign was expected to be a tuple")]
    TupleExpected,
    #[error("the given tuple term to assign had a mismatched length")]
    InvalidTupleRange,
}

/// An error that occurs when getting the variance of a sub-term.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum GetVarianceError {
    #[error("the given location is invalid for this term")]
    InvalidLocation,

    #[error("the term contains an ID that is invalid to the given table")]
    InvalidID,
}

/// Represents a type used to retrieve a sub-term of a particular term.
pub trait Location<Term, SubTerm>:
    Debug
    + Clone
    + Copy
    + PartialEq
    + Eq
    + PartialOrd
    + Ord
    + Hash
    + Send
    + Sync
    + 'static
{
    /// Assigns the `sub_term` to the given `term` at this location.
    ///
    /// # Errors
    ///
    /// See [`AssignSubTermError`] for more information.
    fn assign_sub_term(
        self,
        term: &mut Term,
        sub_term: SubTerm,
    ) -> Result<(), AssignSubTermError>;

    /// Returns the sub-term at this location.
    #[must_use]
    fn get_sub_term(self, term: &Term) -> Option<SubTerm>;

    /// Returns the variance of the sub-term at this location.
    ///
    /// # Errors
    ///
    /// See [`GetVarianceError`] for more information.
    fn get_sub_variance(
        self,
        term: &Term,
        table: &Table<impl State>,
    ) -> Result<Variance, GetVarianceError>;
}
