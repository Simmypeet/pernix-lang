use enum_as_inner::EnumAsInner;
use qbice::{Decode, Encode, StableHash, storage::intern::Interned};

use crate::{
    symbol::Symbol,
    r#type::{Type, bound::Binder},
    variance::Variance,
};

/// A basic type equality predicate, can also be used for rewriting.
///
/// Both left and right has to be a kind of type.
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
pub struct Equality {
    binder: Binder,
    left: Type,
    right: Type,
}

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
pub struct Outlives {
    /// Can either has a kind of lifetime or a kind of type.
    operand: Interned<Type>,

    /// Must always has a kind of lifetime.
    bound: Interned<Type>,
}

impl Outlives {
    #[must_use]
    pub const fn new(operand: Interned<Type>, bound: Interned<Type>) -> Self {
        Self { operand, bound }
    }
}

/// Requires the operand is a tuple type. The oeprand can only be a kind of
/// Type.
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
pub struct Tuple {
    operand: Type,
}

/// Positive or Negative marker.
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
    Encode,
    Decode,
)]
pub enum MarkerPolar {
    Positive,
    Negative,
}

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
pub struct Marker {
    polar: MarkerPolar,
    binder: Binder,
    symbol: Symbol,
}

/// Like Rust, we don't have a full-blown subtyping relation, but only
/// subtyping relation between lifetimes.
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
pub struct Subtype {
    less: Type,
    greater: Type,
    variance: Variance,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    StableHash,
    Encode,
    Decode,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Predicate {
    Outlives(Outlives),
    Tuple(Tuple),
    Marker(Marker),
    Equality(Equality),
}
