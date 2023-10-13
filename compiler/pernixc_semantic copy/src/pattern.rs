//! Contains all the definitions of pattern matching.

use enum_as_inner::EnumAsInner;

use crate::{
    symbol::{StructRef, TypeParameterRef, VariantRef},
    ty,
};

/// Represents a pattern that matches to any value of the given type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Irrefutable {
    Discard,
    Named(Named),
    Structural(Structural<Self>),
    Tuple(Tuple<Self>),
}

/// Represents a pattern that matches to a specific value of numeric type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum NumericLiteral {
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Uint8(u8),
    Uint16(u16),
    Uint32(u32),
    Uint64(u64),
}

/// Represents a pattern that matches to a specific variant of an enum, denoted by
/// `case Variant(...)` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Enum<Pattern> {
    /// The reference to the variant that will be matched.
    pub variant_ref: VariantRef,
    /// The pattern that will match the associated value of the variant.
    pub pattern: Option<Box<Pattern>>,
}

/// Represents a pattern that might not always match to a value of the given type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Refutable {
    BooleanLiteral(bool),
    NumericLiteral(NumericLiteral),
    Structural(Structural<Self>),
    Enum(Enum<Self>),
    Named(Named),
    Tuple(Tuple<Self>),
}

/// Represents a simple name binding pattern.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Named {
    /// Whether the binding is mutable.
    pub is_mutable: bool,

    /// The type of the binding.
    pub ty: ty::Type,

    /// The name of the binding.
    pub name: String,
}

/// A pattern matching for each field of a struct.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Structural<Pattern> {
    /// The reference of the struct in the [`crate::table::Table::structs`] field.
    pub struct_ref: StructRef,

    /// The pattern will match to each field in the struct.
    pub patterns: Vec<Pattern>,
}

/// Is a type that can present on the [`Packed`] tuple pattern.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum PackedType {
    Paraeter(TypeParameterRef),
    TraitAssociated(ty::TraitAssociated),
    Tuple(ty::Tuple),
}

/// Represents a pattern that matches multiple tuple elements and packs them into a single tuple
/// value, denoted by `...name` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Packed {
    /// The type of the packed tuple.
    pub ty: PackedType,

    /// The name of the packed tuple.
    pub name: String,

    /// Whether the packed tuple is mutable.
    pub mutable: bool,
}

/// A pattern that will match to each element/multule elements of a tuple.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum TupleElement<Pattern> {
    /// The pattern will match multiple tuple elements and pack them into a single tuple value.
    Packed(Packed),

    /// The pattern will match to a single tuple element.
    Regular(Pattern),
}

/// Represents a pattern that matches to a tuple value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple<Pattern> {
    /// Contains all the patterns that will match to all the elements of the tuple.
    ///
    /// Tuple pattern can contain at most **one** [`Packed`] tuple pattern.
    pub elements: Vec<TupleElement<Pattern>>,
}
