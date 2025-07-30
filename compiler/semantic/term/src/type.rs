//! Contains the definition of [`Type`] term.

use derive_more::{Deref, DerefMut};
use enum_as_inner::EnumAsInner;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::{
    constant::Constant,
    error::Error,
    generic_arguments::{MemberSymbol, Symbol},
    generic_parameters::TypeParameterID,
    inference::Inference,
    lifetime::Lifetime,
};

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
    StableHash,
    Serialize,
    Deserialize,
)]
#[allow(missing_docs)]
pub enum Qualifier {
    Immutable,
    Mutable,
}

/// Represents a pointer type, denoted by `*mutable? TYPE` syntax.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct Pointer {
    /// Determines whether the pointer is mutable.
    pub mutable: bool,

    /// The type that the pointer points to.
    pub pointee: Box<Type>,
}

/// Represents a reference type, denoted by `&'LIFETIME QUALIFIER TYPE` syntax.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct Reference {
    /// The qualifier applied to the reference.
    pub qualifier: Qualifier,

    /// The lifetime that the reference lives in.
    pub lifetime: Lifetime,

    /// The type that the reference points to.
    pub pointee: Box<Type>,
}

/// Represents an array type, denoted by `[ELEMENT: LENGTH]` syntax.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct Array {
    /// Constant representing the length of the array.
    pub length: Constant,

    /// The type of the elements in the array.
    pub r#type: Box<Type>,
}

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
    StableHash,
    Serialize,
    Deserialize,
)]
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

/// Represents a tuple type, denoted by `(type, type, ...type)` syntax.
pub type Tuple = crate::tuple::Tuple<Type>;

/// Represents a phantom type, denoted by `phantom TYPE` syntax.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct Phantom(pub Box<Type>);

/// A new type wrapper representing a trait associated type.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    Deref,
    DerefMut,
)]
pub struct TraitMember(pub MemberSymbol);

/// Represents a function signature object. Can be used to represent a function
/// pointer.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct FunctionSignature {
    /// The list of function parameters
    pub parameters: Vec<Type>,

    /// The return type of the function
    pub return_type: Box<Type>,
}

/// Represents a type term
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Type {
    Inference(Inference<Self>),
    #[from]
    Primitive(Primitive),
    #[from]
    Parameter(TypeParameterID),
    #[from]
    Symbol(Symbol),
    #[from]
    Pointer(Pointer),
    #[from]
    Reference(Reference),
    #[from]
    Array(Array),
    #[from]
    Tuple(Tuple),
    #[from]
    Phantom(Phantom),
    #[from]
    MemberSymbol(MemberSymbol),
    #[from]
    TraitMember(TraitMember),
    #[from]
    FunctionSignature(FunctionSignature),
    #[from]
    Error(Error),
}
