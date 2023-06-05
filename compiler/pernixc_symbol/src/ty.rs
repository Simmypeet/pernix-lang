//! This module implements the type system of the compiler.

use std::{collections::HashMap, sync::Arc};

use crate::{
    AssociatedTypeID, LifetimeArgument, LifetimeParameterID, StructID, TraitID, TypeParameterID,
};

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReferenceType {
    pub inner: Type,
    pub qualifier: Option<ReferenceQualifier>,
    pub lifetime: Option<LifetimeArgument>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Struct {
    pub struct_id: StructID,
    pub type_arguments_by_parameter: HashMap<TypeParameterID, Type>,
    pub lifetime_arguments_by_parameter: HashMap<LifetimeParameterID, LifetimeArgument>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssociatedType {
    pub trait_id: TraitID,
    pub trait_type_arguments_by_parameter: HashMap<TypeParameterID, Type>,
    pub trait_lifetime_arguments_by_parameter: HashMap<LifetimeParameterID, LifetimeArgument>,
    pub associated_type_id: AssociatedTypeID,
    pub associated_type_type_arguments_by_parameter: HashMap<TypeParameterID, Type>,
    pub associated_type_lifetime_arguments_by_parameter:
        HashMap<LifetimeParameterID, LifetimeArgument>,
}

/// Represents a type symbol.
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum Type {
    Struct(Struct),
    PrimitiveType(PrimitiveType),
    ReferenceType(Arc<ReferenceType>),
    TypeParameter(TypeParameterID),
    AssociatedType(AssociatedType),
}
