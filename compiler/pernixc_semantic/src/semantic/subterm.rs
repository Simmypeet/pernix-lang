//! Contains the definition of [`Location`].

use std::{fmt::Debug, hash::Hash};

use super::term::{MemberSymbol, Symbol, Term, Tuple, TupleElement};

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
}

/// Represents a sub-term location where the sub-term is stored as a generic
/// arguments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SubMemberSymbolLocation {
    /// The index of the sub-term in the generic arguments.
    pub index: usize,

    /// True if the sub-term is in the parent's generic arguments part,
    /// otherwise false.
    pub from_parent: bool,
}

impl<ID> MemberSymbol<ID> {
    /// Returns a mutable reference to a particular sub-term of this generic
    /// arguments.
    ///
    /// Returns `None` if the location is invalid.
    #[must_use]
    pub fn get_term_mut<T: Term>(
        &mut self,
        location: SubMemberSymbolLocation,
    ) -> Option<&mut T> {
        let generic_arguments = if location.from_parent {
            T::get_generic_arguments_mut(&mut self.parent_generic_arguments)
        } else {
            T::get_generic_arguments_mut(&mut self.member_generic_arguments)
        };

        generic_arguments.get_mut(location.index)
    }

    /// Returns a reference to a particular sub-term of this generic arguments.
    ///
    /// Returns `None` if the location is invalid.
    #[must_use]
    pub fn get_term<T: Term>(
        &self,
        location: SubMemberSymbolLocation,
    ) -> Option<&T> {
        let generic_arguments = if location.from_parent {
            T::get_generic_arguments(&self.parent_generic_arguments)
        } else {
            T::get_generic_arguments(&self.member_generic_arguments)
        };

        generic_arguments.get(location.index)
    }
}

/// Represents a sub-term location where the sub-term is stored as an element of
/// a tuple.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SubTupleLocation {
    /// The sub-term is stored as a regular element of the tuple.
    Single(usize),

    /// The sub-term ranges into multiple elements of the tuple.
    Range {
        /// The index of the first element of the tuple.
        begin: usize,

        /// The index of the after the last element of the tuple.
        end: usize,
    },
}

impl<T: Term> Tuple<T>
where
    Self: TryFrom<T, Error = T> + Into<T>,
{
    /// Assigns the `sub_term` to the given `term` at this location.
    ///
    /// # Errors
    ///
    /// See [`AssignSubTermError`] for more information.
    pub fn assign_sub_term(
        &mut self,
        location: SubTupleLocation,
        sub_term: T,
    ) -> Result<(), AssignSubTermError> {
        match location {
            SubTupleLocation::Single(idx) => {
                let element = self
                    .elements
                    .get_mut(idx)
                    .map(TupleElement::as_term_mut)
                    .ok_or(AssignSubTermError::InvalidLocation)?;

                *element = sub_term;

                Ok(())
            }
            SubTupleLocation::Range { begin, end } => {
                let Ok(sub_constant_tuple) = Self::try_from(sub_term) else {
                    return Err(AssignSubTermError::TupleExpected);
                };

                let tuple_elements = self
                    .elements
                    .get_mut(begin..end)
                    .ok_or(AssignSubTermError::InvalidLocation)?;

                if sub_constant_tuple.elements.len() != tuple_elements.len() {
                    return Err(AssignSubTermError::InvalidLocation);
                }

                for (lhs, rhs) in
                    tuple_elements.iter_mut().zip(sub_constant_tuple.elements)
                {
                    *lhs = rhs;
                }

                Ok(())
            }
        }
    }
}

/// Represents a sub-term location where the sub-term is stored as a generic
/// arguments.
///
/// The `usize` represents the index of the sub-term in the generic arguments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SubSymbolLocation(pub usize);

impl<ID> Symbol<ID> {
    /// Returns a mutable reference to a particular sub-term of this generic
    /// arguments.
    ///
    /// Returns `None` if the location is invalid.
    #[must_use]
    pub fn get_term_mut<T: Term>(
        &mut self,
        location: SubSymbolLocation,
    ) -> Option<&mut T> {
        let generic_arguments =
            T::get_generic_arguments_mut(&mut self.generic_arguments);

        generic_arguments.get_mut(location.0)
    }

    /// Returns a reference to a particular sub-term of this generic arguments.
    ///
    /// Returns `None` if the location is invalid.
    #[must_use]
    pub fn get_term<T: Term>(&self, location: SubSymbolLocation) -> Option<&T> {
        let generic_arguments =
            T::get_generic_arguments(&self.generic_arguments);

        generic_arguments.get(location.0)
    }
}

/// A new type wrapper for [`SubMemberSymbolLocation`] to represent a sub-term
/// in trait member symbols.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SubTraitMemberLocation(pub SubMemberSymbolLocation);
