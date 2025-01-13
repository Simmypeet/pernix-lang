//! Contains the definition of [`SubTerm`] trait.
//!
//! Contains logic related to manipulating sub-terms of a term.

use std::{fmt::Debug, hash::Hash};

use enum_as_inner::EnumAsInner;

use crate::{
    constant::{self, Constant},
    generic_arguments,
    lifetime::Lifetime,
    r#type::{self, Type},
    MemberSymbol, Model, ModelOf, Symbol, Tuple,
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

/// Contains the information about the sub-term of a term.
pub trait SubTerm: Sized + ModelOf {
    /// The type that represents the location of a sub-type in the term.
    type SubTypeLocation: Location<Self, Type<Self::Model>>;

    /// The type that represents the location of a sub-constant in the term.
    type SubConstantLocation: Location<Self, Constant<Self::Model>>;

    /// The type that represents the location of a sub-lifetime in the term.
    type SubLifetimeLocation: Location<Self, Lifetime<Self::Model>>;

    /// The type that represents the location of a sub-term of this kind of
    /// term.
    type ThisSubTermLocation: Location<Self, Self>;
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
    + Into<TermLocation>
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

    /// Returns the reference to the sub-term at this location.
    ///
    /// Returns [`None`] if the given location is invalid and the location
    /// refers to the range of tuple elements.
    #[must_use]
    fn get_sub_term_ref(self, term: &Term) -> Option<&SubTerm>;

    /// Returns the mutable reference to the sub-term at this location.
    ///
    /// Returns [`None`] if the given location is invalid and the location
    /// refers to the range of tuple elements.
    #[must_use]
    fn get_sub_term_mut(self, term: &mut Term) -> Option<&mut SubTerm>;
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

/// Represents a sub-term location where the sub-term is stored as an element of
/// a tuple.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
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

impl<T> Tuple<T>
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
                    .map(|x| &mut x.term)
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

/// A new type wrapper for [`SubMemberSymbolLocation`] to represent a sub-term
/// in trait member symbols.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SubTraitMemberLocation(pub SubMemberSymbolLocation);

/// An enumeration of locations where a lifetime can be located.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
pub enum SubLifetimeLocation {
    /// The location points to a lifetime that is a part of a type.
    FromType(r#type::SubLifetimeLocation),
}

/// An enumeration of locations where a type can be located.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
pub enum SubTypeLocation {
    /// The location points to a type that is a part of a type.
    FromType(r#type::SubTypeLocation),
}

/// An enumeration of locations where a constant can be located.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
pub enum SubConstantLocation {
    /// The location points to a constant that is a part of a type.
    FromType(r#type::SubConstantLocation),

    /// The location points to a constant that is a part of a constant.
    FromConstant(constant::SubConstantLocation),
}

/// Enumeration of all sub-location of all kinds of terms.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
pub enum TermLocation {
    /// The location points to a lifetime.
    Lifetime(SubLifetimeLocation),

    /// The location points to a type.
    Type(SubTypeLocation),

    /// The location points to a constant.
    Constant(SubConstantLocation),
}

impl<M: Model> MemberSymbol<M> {
    /// Returns a mutable reference to a particular sub-term of this generic
    /// arguments.
    ///
    /// Returns `None` if the location is invalid.
    #[must_use]
    pub fn get_term_mut<T: generic_arguments::Element<M>>(
        &mut self,
        location: SubMemberSymbolLocation,
    ) -> Option<&mut T> {
        let generic_arguments = if location.from_parent {
            T::get_mut(&mut self.parent_generic_arguments)
        } else {
            T::get_mut(&mut self.member_generic_arguments)
        };

        generic_arguments.get_mut(location.index)
    }

    /// Returns a reference to a particular sub-term of this generic arguments.
    ///
    /// Returns `None` if the location is invalid.
    #[must_use]
    pub fn get_term<T: generic_arguments::Element<M>>(
        &self,
        location: SubMemberSymbolLocation,
    ) -> Option<&T> {
        let generic_arguments = if location.from_parent {
            T::get(&self.parent_generic_arguments)
        } else {
            T::get(&self.member_generic_arguments)
        };

        generic_arguments.get(location.index)
    }
}

impl<M: Model> Symbol<M> {
    /// Returns a mutable reference to a particular sub-term of this generic
    /// arguments.
    ///
    /// Returns `None` if the location is invalid.
    #[must_use]
    pub fn get_term_mut<T: generic_arguments::Element<M>>(
        &mut self,
        location: SubSymbolLocation,
    ) -> Option<&mut T> {
        let generic_arguments = T::get_mut(&mut self.generic_arguments);

        generic_arguments.get_mut(location.0)
    }

    /// Returns a reference to a particular sub-term of this generic arguments.
    ///
    /// Returns `None` if the location is invalid.
    #[must_use]
    pub fn get_term<T: generic_arguments::Element<M>>(
        &self,
        location: SubSymbolLocation,
    ) -> Option<&T> {
        let generic_arguments = T::get(&self.generic_arguments);

        generic_arguments.get(location.0)
    }
}
