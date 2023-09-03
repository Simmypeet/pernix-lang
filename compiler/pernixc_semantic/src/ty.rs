use pernixc_system::arena;

use crate::symbol::{
    self, GenericItemRef, LifetimeParameterRef, LocalSubstitution, TypeParameterRef,
};

/// Represents an automatic generated lifetime used in the place where the lifetime is elided.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ElidedLifetime {
    /// The reference to the generic item that generates this elided lifetime.
    pub generic_item_ref: GenericItemRef,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum Lifetime {
    #[default]
    Static,
    Parameter(LifetimeParameterRef),
    ElidedLifetime(ElidedLifetime),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Primitive {
    Int8,
    Int16,
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Float32,
    Float64,
    Bool,
}

/// Is an enumeration of all possible unpackable constants.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Unpackable {
    Parameter(TypeParameterRef),
    TraitAssociated(TraitAssociated),
}

/// Represents a single element of a tuple.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TupleElement {
    /// A regular singular constant.
    Regular(Type),
    /// A constant that can be unpacked into multiple elements.
    Unpacked(Unpackable),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple(pub Vec<Type>);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Struct {
    pub struct_index: arena::ID<symbol::Struct>,
    pub substitution: LocalSubstitution,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Enum {
    pub eunm_id: arena::ID<symbol::Enum>,
    pub substitution: LocalSubstitution,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitAssociated {
    pub reference: TypeParameterRef,
    pub association_substitution: LocalSubstitution,
    pub trait_substitution: LocalSubstitution,
}

/// Represents a type parameter that has been bounded to be a tuple type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TupleBoundable {
    TypeParameter(TypeParameterRef),
    TraitAssociatedType(TraitAssociated),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Qualifier {
    Mutable,
    Restrict,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Reference {
    pub qualifier: Option<Qualifier>,
    pub lifetime: Lifetime,
    pub ty: Box<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pointer {
    pub qualifier: Option<Qualifier>,
    pub ty: Box<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Primitive(Primitive),
    Tuple(Tuple),
    Struct(Struct),
    Parameter(TypeParameterRef),
    Enum(Enum),
    TraitAssociated(TraitAssociated),
    Reference(Reference),
    Pointer(Pointer),
}

impl Default for Type {
    fn default() -> Self { Self::Tuple(Tuple(Vec::new())) }
}
