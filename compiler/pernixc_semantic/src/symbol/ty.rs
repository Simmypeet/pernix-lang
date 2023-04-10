//! Contains the type system for Pernix.

use derive_more::From;
use enum_as_inner::EnumAsInner;

use super::TypedID;

/// Is an enumeration of all the primitive types in Pernix.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub enum PrimitiveType {
    /// Void (unit type)
    #[default]
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
    Primitive(PrimitiveType),

    /// The type refers to a particular typed symbol in the symbol table.
    TypedID(TypedID),

    /// The type that yields from an expression that jumps the flow out of the current procedure.
    Never,
}

impl Default for Type {
    fn default() -> Self { Self::Primitive(PrimitiveType::Void) }
}

/// Represents a type used to represent a type binding of an l-value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeBinding {
    /// The type of the binding.
    pub ty: Type,

    /// Whether the binding is mutable.
    pub is_mutable: bool,
}
