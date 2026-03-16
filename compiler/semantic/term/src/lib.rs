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
pub mod instance;
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
    Instance(instance::Instance),
}

impl display::Display for Term {
    async fn fmt(
        &self,
        engine: &pernixc_qbice::TrackedEngine,
        formatter: &mut display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        match self {
            Self::Constant(constant) => constant.fmt(engine, formatter).await,
            Self::Lifetime(lifetime) => lifetime.fmt(engine, formatter).await,
            Self::Type(ty) => ty.fmt(engine, formatter).await,
            Self::Instance(instance) => instance.fmt(engine, formatter).await,
        }
    }
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
    Instance(&'a instance::Instance),
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
    Instance(&'a mut instance::Instance),
}

impl TermMut<'_> {
    #[must_use]
    pub fn to_owned_term(&self) -> Term {
        match self {
            Self::Constant(constant) => Term::Constant((*constant).clone()),
            Self::Lifetime(lifetime) => Term::Lifetime((*lifetime).clone()),
            Self::Type(ty) => Term::Type((*ty).clone()),
            Self::Instance(instance) => Term::Instance((*instance).clone()),
        }
    }
}
