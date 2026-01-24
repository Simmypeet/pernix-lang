//! Defines the various terms to be used with the type system.

use enum_as_inner::EnumAsInner;
use qbice::{Decode, Encode, StableHash};

pub mod constant;
pub mod display;
pub mod effect;
pub mod error;
pub mod generic_arguments;
pub mod generic_parameters;
pub mod inference;
pub mod instantiation;
pub mod lifetime;
pub mod matching;
pub mod predicate;
pub mod sub_term;
pub mod tuple;
pub mod r#type;
pub mod visitor;

/// Represents a type that can never be instantiated.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Never {}

/// An enumeration of all kinds of terms in the type system.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
#[allow(missing_docs)]
pub enum Term {
    Constant(constant::Constant),
    Lifetime(lifetime::Lifetime),
    Type(r#type::Type),
}

/// A reference to a term, which can be either a constant, lifetime, or type.
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
#[allow(missing_docs)]
pub enum TermRef<'a> {
    Constant(&'a constant::Constant),
    Lifetime(&'a lifetime::Lifetime),
    Type(&'a r#type::Type),
}

/// A mutable reference to a term, which can be either a constant, lifetime, or
/// type.
#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, derive_more::From,
)]
#[allow(missing_docs)]
pub enum TermMut<'a> {
    Constant(&'a mut constant::Constant),
    Lifetime(&'a mut lifetime::Lifetime),
    Type(&'a mut r#type::Type),
}
