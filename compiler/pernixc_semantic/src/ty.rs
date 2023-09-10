use crate::symbol::{GenericItemRef, GenericParameterRef, LocalSubstitution};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Enum {
    pub index: usize,
    pub substitution: LocalSubstitution,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Qualifier {
    Restrict,
    Mutable,
    Immutable,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Reference {
    pub qualifier: Qualifier,
    pub lifetime: Lifetime,
    pub operand: Box<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pointer {
    pub qualifier: Qualifier,
    pub operand: Box<Type>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lifetime {
    Static,
    Parameter(GenericParameterRef),
    Elided(GenericItemRef),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Struct {
    pub struct_index: usize,
    pub substitution: LocalSubstitution,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Unpacked {
    Parameter(GenericParameterRef),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TupleElement {
    Regular(Type),
    Unpacked(Unpacked),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple {
    pub elements: Vec<TupleElement>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Enum(Enum),
    Reference(Reference),
    Pointer(Pointer),
    Struct(Struct),
    Tuple(Tuple),
    Parameter(GenericParameterRef),
}
