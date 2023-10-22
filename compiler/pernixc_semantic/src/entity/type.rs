//! Contains the definition of type model.

use enum_as_inner::EnumAsInner;

use super::{constant::Constant, region::Region, GenericArguments, Model};
use crate::{
    arena::ID,
    symbol::{Enum, Struct, TraitTypeID, TypeParameterID},
};

/// Either an ID of an enum or an ID of a struct.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum AlgebraicKind {
    Enum(ID<Enum>),
    Struct(ID<Struct>),
}

/// Represents an algebraic type, which is either an enum or a struct.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Algebraic<S: Model> {
    /// The kind of the algebraic type.
    pub kind: AlgebraicKind,

    /// The generic arguments supplied to the algebraic type.
    pub generic_arguments: GenericArguments<S>,
}

/// A qualifier that can be applied to references/pointers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Qualifier {
    Immutable,
    Mutable,
    Restrict,
}

/// Represents a pointer type, denoted by `*QUALIFIDER TYPE` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pointer<S: Model> {
    /// The qualifier applied to the pointer.
    pub qualifier: Qualifier,

    /// The type that the pointer points to.
    pub pointee: Box<Type<S>>,
}

/// Represents a reference type, denoted by `&'LIFETIME QUALIFIER TYPE` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Reference<S: Model> {
    /// The qualifier applied to the reference.
    pub qualifier: Qualifier,

    /// The region that the reference lives in.
    pub region: Region<S>,

    /// The type that the reference points to.
    pub pointee: Box<Type<S>>,
}

/// Represents an array type, denoted by `[ELEMENT: LENGTH]` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Array<S: Model> {
    /// Constant representing the length of the array.
    pub length: Constant<S>,

    /// The type of the elements in the array.
    pub element: Box<Type<S>>,
}

/// Represents a trait member type, denoted by `TRAIT<GENERIC_ARGUMENTS>::MEMBER<GENERIC_ARGUMENTS>`
/// syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitMember<S: Model> {
    /// The ID to the trait type.
    pub trait_type_id: TraitTypeID,

    /// The generic arguments supplied to the trait.
    pub trait_generic_arguments: GenericArguments<S>,

    /// The generic arugments supplied to the trait type member.
    pub member_generic_arguments: GenericArguments<S>,
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

/// Represents an **unpacked** element type in the tuple, denoted by `...type` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Unpacked<S: Model> {
    Parameter(TypeParameterID),
    TraitMember(TraitMember<S>),
}

/// Represents an element type in the tuple.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum TupleElement<S: Model> {
    /// Represents a singular type in the tuple.
    Regular(Type<S>),

    /// Represents a type that might be expanded into multiple types in the tuple later on.
    Unpacked(Unpacked<S>),
}

/// Represents a tuple type, denoted by `(type, type, ...type)` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple<S: Model> {
    /// The elements of the tuple.
    ///
    /// At most only one [`TupleElement::Unpacked`] can be present in the tuple.
    pub elements: Vec<TupleElement<S>>,
}

/// Represents a type in the language.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Type<S: Model> {
    Primitive(Primitive),
    Inference(S::TypeInference),
    Algebraic(Algebraic<S>),
    Pointer(Pointer<S>),
    Reference(Reference<S>),
    Array(Array<S>),
    TraitMember(TraitMember<S>),
    Parameter(TypeParameterID),
    Tuple(Tuple<S>),
}
