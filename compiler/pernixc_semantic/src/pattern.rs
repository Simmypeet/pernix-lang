//! Contains the definition of patterns

use std::collections::HashMap;

use enum_as_inner::EnumAsInner;

use crate::{
    arena::ID,
    ir::address::Address,
    semantic::{session::ExceedLimitError, term::r#type::Qualifier},
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

    /// The address to the value that the reference points to.
    ///
    /// NOTE: This is not the address where the reference is stored but the
    /// address of the value that the reference points to!
    pub address: Address,
}

/// Specifies how a pattern matches (as a value or as a reference).
///
/// # Examples
///
/// With the following code:
///
/// ``` pnx
/// public struct Vector2 {
///     public x: float32,
///     public y: float32,
/// }
///
/// public function returnVector(): Vector2 {
///     return Vector2 {
///         x: 0,
///         y: 0,
///     }
/// }
/// ```
///
/// This is how the value-bound pattern would look like:
///
/// ``` pnx
/// public function main() {
///     let binding  = returnVector();
///     let { x, y } = returnVector();
/// }
/// ```
///
/// The named pattern `binding` is bound as a value where the address is the
/// pointer to the stack where the value is stored. The named pattern `{ x, y }`
/// is bound as a value where the address is the pointer to the fields `x` and
/// `y`, which also are stored on the stack.
///
/// This is how the reference-bound pattern would look like:
///
/// ``` pnx
/// public function main['a](
///     ref first: Vector2,
///     second:    &'a Vector2
/// ) {}
/// ```
///
/// Both `first` and `second` are bound as references. The address of `first` is
/// the pointer pointing to the stack where the `first` parameter is stored. The
/// address of `second` is directly the address held by the `&'a Vector2` type.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Binding {
    Value(ValueBinding),
    Reference(ReferenceBinding),
}

/// A trait that is implemented by [`Refutable`] and [`Irrefutable`].
pub trait Pattern {
    /// Returns the binding of the pattern.
    fn binding(&self) -> &Binding;
}

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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Numeric {
    /// The value of the numeric literal.
    pub value: NumericValue,

    /// The binding of this pattern.
    pub binding: Binding,
}

/// A boolean literal pattern
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Boolean {
    /// The value of the boolean literal.
    pub value: bool,

    /// The binding of this pattern.
    pub binding: Binding,
}

/// A pattern where the value is bound to a name
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Named {
    /// The name of the pattern.
    pub name: String,

    /// The binding of this pattern.
    pub binding: Binding,

    /// For value-bound patterns, this specifies whether the value is mutable.
    ///
    /// This is only relevant for value-bound patterns.
    pub is_mutable: bool,
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

    /// The binding of this pattern.
    pub binding: Binding,
}

/// A pattern that matches on a struct with fields.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Structural<T: Pattern> {
    /// The ID of the struct that the pattern matches.
    pub struct_id: ID<Struct>,

    /// Mapping from each field to the pattern that the field must match.
    pub fields: HashMap<ID<Field>, T>,

    /// The binding of this pattern.
    pub binding: Binding,
}

/// A pattern that discards the value
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Discard {
    /// The binding of this pattern.
    pub binding: Binding,
}

/// A pattern that cannot be refuted (always matches)
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum Irrefutable {
    Named(Named),
    Tuple(Tuple<Irrefutable>),
    Structural(Structural<Irrefutable>),
    Discard(Discard),
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
    Discard(Discard),
}

impl Pattern for Irrefutable {
    fn binding(&self) -> &Binding {
        match self {
            Self::Named(p) => &p.binding,
            Self::Tuple(p) => &p.binding,
            Self::Structural(p) => &p.binding,
            Self::Discard(p) => &p.binding,
        }
    }
}

impl Pattern for Refutable {
    fn binding(&self) -> &Binding {
        match self {
            Self::Boolean(p) => &p.binding,
            Self::Numeric(p) => &p.binding,
            Self::Named(p) => &p.binding,
            Self::Tuple(p) => &p.binding,
            Self::Structural(p) => &p.binding,
            Self::Discard(p) => &p.binding,
        }
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum CreatePatternError {
    #[error(transparent)]
    ExceedLimitError(ExceedLimitError),

    #[error(
        "The given block is unreachable and cannot be used to create a pattern"
    )]
    UnreachableBlock,
}
