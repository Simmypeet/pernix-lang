//! Contains the type system for Pernix.

use derive_more::From;
use enum_as_inner::EnumAsInner;

use super::TypedID;

/// Is an enumeration of all the primitive types in Pernix.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum PrimitiveType {
    /// Void (unit type)
    Void,
    /// A boolean value (true or false)
    Bool,
    /// A 32-bit floating point number
    Float32,
    /// A 64-bit floating point number
    Float64,
    /// A 8-bit signed integer
    Int8,
    /// A 16-bit signed integer
    Int16,
    /// A 32-bit signed integer
    Int32,
    /// A 64-bit signed integer
    Int64,
    /// A 8-bit unsigned integer
    Uint8,
    /// A 16-bit unsigned integer
    Uint16,
    /// A 32-bit unsigned integer
    Uint32,
    /// A 64-bit unsigned integer
    Uint64,
}

/// Represents a type in Pernix.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, EnumAsInner, From)]
pub enum Type {
    /// The type is a primitive type.
    PrimitiveType(PrimitiveType),

    /// The type refers to a particular typed symbol in the symbol table.
    TypedID(TypedID),
}

impl Type {
    /// Returns whether the type is [`PrimitiveType::Void`].
    #[must_use]
    pub fn is_void(&self) -> bool { matches!(self, Self::PrimitiveType(PrimitiveType::Void)) }
}

/// Represents a type used to represent a type binding of an l-value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeBinding {
    /// The type of the binding.
    pub ty: Type,

    /// Whether the binding is mutable.
    pub is_mutable: bool,
}