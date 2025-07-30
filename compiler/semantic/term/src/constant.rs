//! Contains the definition of [`Constant`] term.

use enum_as_inner::EnumAsInner;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

use crate::{
    error::Error, generic_parameters::ConstantParameterID, inference::Inference,
};

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
    StableHash,
    Serialize,
    Deserialize,
    EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Primitive {
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Isize(i64),
    Uint8(u8),
    Uint16(u16),
    Uint32(u32),
    Uint64(u64),
    Usize(u64),
    Bool(bool),
}

/// Represents a struct constant value.
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
pub struct Struct {
    /// The ID to the struct.
    pub id: Global<pernixc_symbol::ID>,

    /// The fields of the struct constant value.
    pub fields: Vec<Constant>,
}

/// Represents an enum constant value.
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
pub struct Enum {
    /// The variant that the enum constant value is.
    pub variant_id: Global<pernixc_symbol::ID>,

    /// The associated value of the enum constant value (if any).
    pub associated_value: Option<Box<Constant>>,
}

/// Represents an array constant value.
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
    /// The value of each element in the array constant value.
    pub elements: Vec<Constant>,
}

/// Represents a tuple constant value, denoted by `(value, value, ...value)`
/// syntax.
pub type Tuple = crate::tuple::Tuple<Constant>;

/// Represents a compile-time constant term.
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
pub enum Constant {
    #[from]
    Primitive(Primitive),
    #[from]
    Inference(pernixc_arena::ID<Inference<Self>>),
    #[from]
    Parameter(ConstantParameterID),
    #[from]
    Struct(Struct),
    #[from]
    Enum(Enum),
    #[from]
    Array(Array),
    #[from]
    Tuple(Tuple),
    Phantom,
    #[from]
    Error(Error),
}
