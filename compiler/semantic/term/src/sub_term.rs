//! Contains the definition of [`SubTerm`] trait.
//!
//! Contains logic related to manipulating sub-terms of a term.

use std::{fmt::Debug, hash::Hash};

use enum_as_inner::EnumAsInner;

use crate::{
    constant::{self, Constant},
    lifetime::Lifetime,
    r#type::{self, Type},
};

/// Contains the information about the sub-term of a term.
pub trait SubTerm: Sized {
    /// The type that represents the location of a sub-type in the term.
    type SubTypeLocation: Location<Self, Type>;

    /// The type that represents the location of a sub-constant in the term.
    type SubConstantLocation: Location<Self, Constant>;

    /// The type that represents the location of a sub-lifetime in the term.
    type SubLifetimeLocation: Location<Self, Lifetime>;

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
    fn assign_sub_term(self, term: &mut Term, sub_term: SubTerm);

    /// Returns the sub-term at this location.
    #[must_use]
    fn get_sub_term(self, term: &Term) -> Option<SubTerm>;

    /// Returns the reference to the sub-term at this location.
    /// /// Returns [`None`] if the given location is invalid and the location
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
