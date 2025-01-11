//! Contains the definition of compile-time constant terms.

use enum_as_inner::EnumAsInner;
use pernixc_table::GlobalID;
use serde::{Deserialize, Serialize};

use super::{generic_parameters::ConstantParameterID, Error, Model};

/// Represents a primitive constant.
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
pub enum Primitive {
    #[display(fmt = "{_0}")]
    Integer(i128),
    #[display(fmt = "{_0}")]
    Bool(bool),
}

/// Represents a struct constant value.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Struct<M: Model> {
    /// The ID to the struct.
    pub id: GlobalID,

    /// The fields of the struct constant value.
    pub fields: Vec<Constant<M>>,
}

/// Represents an enum constant value.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Enum<M: Model> {
    /// The variant that the enum constant value is.
    pub variant_id: GlobalID,

    /// The associated value of the enum constant value (if any).
    pub associated_value: Option<Box<Constant<M>>>,
}

/// Represents an array constant value.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Array<M: Model> {
    /// The value of each element in the array constant value.
    pub elements: Vec<Constant<M>>,
}

/// Represents a tuple constant value, denoted by `(value, value, ...value)`
/// syntax.
pub type Tuple<M> = super::Tuple<Constant<M>>;

/// Represents a compile-time constant term.
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
#[non_exhaustive]
pub enum Constant<M: Model> {
    #[from]
    Primitive(Primitive),
    Inference(M::ConstantInference),
    #[from]
    Struct(Struct<M>),
    #[from]
    Enum(Enum<M>),
    #[from]
    Array(Array<M>),
    #[from]
    Parameter(ConstantParameterID),
    #[from]
    Tuple(Tuple<M>),

    Phantom,
    #[from]
    Error(Error),
}
