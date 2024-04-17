//! Contains the definition of patterns

use std::collections::HashMap;

use crate::{
    arena::ID,
    ir::address::{self, Address},
    semantic::{
        session::ExceedLimitError,
        term::{lifetime::Lifetime, r#type::Qualifier},
    },
    symbol::{Field, Struct},
};

/// A pattern that matches as a value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueBinding {
    /// The address to the value that the pattern has matched.
    ///
    /// NOTE: This is the address where the value is stored. The value can be
    /// accessed by loading the value from this address.
    pub address: Address,
}

/// A pattern that matches as a reference.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReferenceBinding {
    /// The qualifier of the reference.
    pub qualifier: Qualifier,

    /// The lifetime of the reference.
    pub lifetime: Lifetime,

    /// The address to the value that the reference points to.
    ///
    /// NOTE: This is not the address where the reference is stored but the
    /// address of the value that the reference points to!
    pub address: Address,
}

/// A trait that is implemented by [`Refutable`] and [`Irrefutable`].
pub trait Pattern {}

/// A numeric value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum NumericValue {
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Uint8(u8),
    Uint16(u16),
    Uint32(u32),
    Uint64(u64),
}

/// A numeric literal pattern
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Numeric {
    /// The value of the numeric literal.
    pub value: NumericValue,
}

/// A boolean literal pattern
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Boolean {
    /// The value of the boolean literal.
    pub value: bool,
}

/// A pattern where the value is bound to a name
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Named {
    /// The name of the pattern.
    pub name: String,

    /// The address to the location where the value is stored with this name
    /// binding.
    pub address: address::Stack,
}

/// An element in a tuple pattern.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum TupleElement<T: Pattern> {
    Unpacked(T),
    Regular(T),
}

/// A tuple pattern.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple<T: Pattern> {
    /// The pattern that each element of the tuple must match.
    pub elements: Vec<TupleElement<T>>,
}

/// A pattern that matches on a struct with fields.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Structural<T: Pattern> {
    /// The ID of the struct that the pattern matches.
    pub struct_id: ID<Struct>,

    /// Mapping from each field to the pattern that the field must match.
    pub fields: HashMap<ID<Field>, T>,
}

/// A pattern that discards the value
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Wildcard;

/// A pattern that cannot be refuted (always matches)
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum Irrefutable {
    Named(Named),
    Tuple(Tuple<Irrefutable>),
    Structural(Structural<Irrefutable>),
    Wildcard(Wildcard),
}

/// A pattern that can be refuted (may not always match)
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Refutable {
    Boolean(Boolean),
    Numeric(Numeric),
    Named(Named),
    Tuple(Tuple<Refutable>),
    Structural(Structural<Refutable>),
    Wildcard(Wildcard),
}

impl Pattern for Irrefutable {}

impl Pattern for Refutable {}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum CreatePatternError {
    #[error(transparent)]
    ExceedLimitError(#[from] ExceedLimitError),

    #[error(
        "The given block is unreachable and cannot be used to create a pattern"
    )]
    UnreachableBlock,
}
