//! Contains the definition of type model.

use enum_as_inner::EnumAsInner;

use super::{GenericArguments, System};
use crate::symbol::{EnumRef, StructRef, TraitTypeRef};

/// Represents an algebraic type, which is either a struct or an enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum AlgebraicKind {
    Struct(StructRef),
    Enum(EnumRef),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Algebraic<S: System> {
    pub kind: AlgebraicKind,
    pub arguments: GenericArguments<S>,
}

/// Contains all primitive types in the language.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
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
    Usize,
    Isize,
}

/// Represents a trait associated type projection (e.g. `T::Item`)
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitAssociated<S: System> {
    pub trait_associated_type_ref: TraitTypeRef,
    pub trait_arguments: GenericArguments<S>,
    pub type_arguments: GenericArguments<S>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Type<S: System> {
    Primitive(Primitive),
    InferenceVariable(S::TypeInference),
    Algebraic(Algebraic<S>),
}
