//! This module implements the type system of the compiler.

use derive_more::From;
use enum_as_inner::EnumAsInner;

use crate::{LifetimeArgument, StructID, Substitution, TraitTypeID, TypeParameterID};

/// Enumeration of all possible primitive types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum PrimitiveType {
    Float32,
    Float64,
    Void,
    Int8,
    Int16,
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Bool,
}

/// Is an additional qualifier for the reference type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ReferenceQualifier {
    /// The reference is mutable.
    Mutable,

    /// The reference is resitricted (requires unique access)
    Restrict,
}

/// Represents a reference type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReferenceType {
    /// The type of the reference.
    pub operand: Box<Type>,

    /// The optional qualifier of the reference.
    pub qualifier: Option<ReferenceQualifier>,

    /// The optional lifetime of the reference.
    pub lifetime: Option<LifetimeArgument>,
}

/// Represents a type from the struct.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Struct {
    /// The ID of the struct.
    pub struct_id: StructID,

    /// The generic parameter substitution.
    pub substitution: Substitution,
}

/// Represents a type from the trait associated type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitType {
    /// The ID of the associated type.
    pub trait_type_id: TraitTypeID,

    /// The generic parameter substitution.
    pub substitution: Substitution,
}

/// Represents a type symbol.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Type {
    Struct(Struct),
    PrimitiveType(PrimitiveType),
    ReferenceType(ReferenceType),
    TypeParameter(TypeParameterID),
    TraitType(TraitType),
}
