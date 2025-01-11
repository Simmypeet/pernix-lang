//! Contains the definition of the `Type` enum, which represents all types in
//! the language.

use enum_as_inner::EnumAsInner;
use serde::{Deserialize, Serialize};

use super::{
    constant::Constant, generic_parameters::TypeParameterID,
    lifetime::Lifetime, Error, MemberSymbol, Model, Symbol,
};

/// Contains all primitive types in the language.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    Serialize,
    Deserialize,
    derive_more::Display,
)]
#[allow(missing_docs)]
pub enum Primitive {
    #[display(fmt = "int8")]
    Int8,
    #[display(fmt = "int16")]
    Int16,
    #[display(fmt = "int32")]
    Int32,
    #[display(fmt = "int64")]
    Int64,
    #[display(fmt = "uint8")]
    Uint8,
    #[display(fmt = "uint16")]
    Uint16,
    #[display(fmt = "uint32")]
    Uint32,
    #[display(fmt = "uint64")]
    Uint64,
    #[display(fmt = "float32")]
    Float32,
    #[display(fmt = "float64")]
    Float64,
    #[display(fmt = "bool")]
    Bool,
    #[display(fmt = "usize")]
    Usize,
    #[display(fmt = "isize")]
    Isize,
}

/// Represents a pointer type, denoted by `*mutable? TYPE` syntax.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Pointer<M: Model> {
    /// Determines whether the pointer is mutable.
    pub mutable: bool,

    /// The type that the pointer points to.
    pub pointee: Box<Type<M>>,
}

/// A qualifier that can be applied to references/pointers.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    derive_more::Display,
)]
#[allow(missing_docs)]
pub enum Qualifier {
    #[display(fmt = "immutable")]
    Immutable,
    #[display(fmt = "mutable")]
    Mutable,
}

/// Represents a reference type, denoted by `&'LIFETIME QUALIFIER TYPE` syntax.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Reference<M: Model> {
    /// The qualifier applied to the reference.
    pub qualifier: Qualifier,

    /// The lifetime that the reference lives in.
    pub lifetime: Lifetime<M>,

    /// The type that the reference points to.
    pub pointee: Box<Type<M>>,
}

/// Represents an array type, denoted by `[ELEMENT: LENGTH]` syntax.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Array<M: Model> {
    /// Constant representing the length of the array.
    pub length: Constant<M>,

    /// The type of the elements in the array.
    pub r#type: Box<Type<M>>,
}

/// Represents a phantom type, denoted by `phantom TYPE` syntax.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Phantom<M: Model>(pub Box<Type<M>>);

/// Represents a tuple type, denoted by `(type, type, ...type)` syntax.
pub type Tuple<M> = super::Tuple<Type<M>>;

/// Represents a type term.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    Serialize,
    Deserialize,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Type<M: Model> {
    #[from]
    Primitive(Primitive),
    #[from]
    Parameter(TypeParameterID),
    Inference(M::TypeInference),
    #[from]
    Symbol(Symbol<M>),
    #[from]
    Pointer(Pointer<M>),
    #[from]
    Reference(Reference<M>),
    #[from]
    Array(Array<M>),
    #[from]
    Tuple(Tuple<M>),
    #[from]
    Phantom(Phantom<M>),
    #[from]
    MemberSymbol(MemberSymbol<M>),
    #[from]
    Error(Error),
}
