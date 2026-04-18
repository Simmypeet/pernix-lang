#![feature(coroutines)]

//! Defines the various terms to be used with the type system.

use enum_as_inner::EnumAsInner;
use qbice::{Decode, Encode, StableHash, storage::intern::Interned};

pub mod constant;
pub mod error;
pub mod generic_arguments;
pub mod generic_parameters;
pub mod inference;
pub mod instance;
pub mod lifetime;
pub mod sub_term;
pub mod tuple;
pub mod r#type;

#[cfg(test)]
mod test;

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
    Constant(Interned<constant::Constant>),
    Lifetime(Interned<lifetime::Lifetime>),
    Type(Interned<r#type::Type>),
    Instance(Interned<instance::Instance>),
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
    Constant(&'a Interned<constant::Constant>),
    Lifetime(&'a Interned<lifetime::Lifetime>),
    Type(&'a Interned<r#type::Type>),
    Instance(&'a Interned<instance::Instance>),
}

/// A mutable reference to a term, which can be either a constant, lifetime, or
/// type.
#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, derive_more::From,
)]
#[allow(missing_docs)]
pub enum TermMut<'a> {
    Constant(&'a mut Interned<constant::Constant>),
    Lifetime(&'a mut Interned<lifetime::Lifetime>),
    Type(&'a mut Interned<r#type::Type>),
    Instance(&'a mut Interned<instance::Instance>),
}

impl TermMut<'_> {
    #[must_use]
    pub fn to_term(&self) -> Term {
        match self {
            Self::Constant(constant) => Term::Constant((*constant).clone()),
            Self::Lifetime(lifetime) => Term::Lifetime((*lifetime).clone()),
            Self::Type(ty) => Term::Type((*ty).clone()),
            Self::Instance(instance) => Term::Instance((*instance).clone()),
        }
    }
}
