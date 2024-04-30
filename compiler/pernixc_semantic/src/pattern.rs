//! Contains the definition of patterns

use std::collections::HashMap;

use crate::{
    arena::ID,
    ir::address::Address,
    semantic::session::ExceedLimitError,
    symbol::{Field, Struct},
};

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
    pub load_address: Address,

    /// Determined if the underlying value is mutable or not.
    pub mutable: bool,
}

/// A tuple binding where alll of the patterns are regular.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RegularTupleBinding<T: Pattern> {
    /// The pattern binding for each element in the tuple.
    pub elements: Vec<T>,
}

/// A tuple binding with at least one packed element.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PackedTupleBinding<T: Pattern> {
    /// The pattern binding for each element in the tuple up to the packed
    /// element.
    pub before_packed_elements: Vec<T>,

    /// The pattern binding for each element in the tuple after the packed
    /// element to the end of the tuple.
    pub after_packed_elements: Vec<T>,

    /// The tuple pattern for the packed element.
    pub packed_element: Box<T>,
}

/// A pattern bound to a tuple type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Tuple<T: Pattern> {
    Regular(RegularTupleBinding<T>),
    Packed(PackedTupleBinding<T>),
}

/// A pattern that matches on a struct with fields.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Structural<T: Pattern> {
    /// The ID of the struct that the pattern matches.
    pub struct_id: ID<Struct>,

    /// Mapping from each field to the pattern that the field must match.
    pub patterns_by_field_id: HashMap<ID<Field>, T>,
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
