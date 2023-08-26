use pernixc_system::arena::ID;

use crate::symbol::{self, LocalSubstitution, TypeParameterRef};

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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple(pub Vec<Type>);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Struct {
    pub struct_id: ID<symbol::Struct>,
    pub substitution: LocalSubstitution,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Enum {
    pub enum_id: ID<symbol::Enum>,
    pub substitution: LocalSubstitution,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Primitive(Primitive),
    Tuple(Tuple),
    Struct(Struct),
    Type(TypeParameterRef),
}

impl Default for Type {
    fn default() -> Self { Self::Tuple(Tuple(Vec::new())) }
}
