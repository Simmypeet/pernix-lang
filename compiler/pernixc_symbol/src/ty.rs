//! This module implements the type system of the compiler.

use std::{collections::HashMap, sync::Arc};

use crate::{GenericStructID, LifetimeParameterID, StructID, TypeParameterID};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ReferenceQualifier {
    Mutable,
    Restrict,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LifetimeArgument {
    Static,
    LifetimeParamter(LifetimeParameterID),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReferenceType {
    pub inner: Type,
    pub qualifier: Option<ReferenceQualifier>,
    pub lifetime: Option<LifetimeArgument>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Instantiation {
    pub generic_struct_id: GenericStructID,
    pub type_arguments: HashMap<TypeParameterID, Type>,
    pub lifetime_arguments: HashMap<LifetimeParameterID, LifetimeArgument>,
}

/// Represents a type symbol.
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum Type {
    Struct(StructID),
    PrimitiveType(PrimitiveType),
    ReferenceType(Arc<ReferenceType>),
    TypeParameter(TypeParameterID),
    Instantiation(Arc<Instantiation>),
}
