//! Contains the definition of [`Address`] and its variants.

use enum_as_inner::EnumAsInner;

use super::{alloca::Alloca, value::Value};
use crate::{
    arena::ID,
    semantic::model::Model,
    symbol::{self, Parameter},
};

/// The address points to a field in a struct.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Field<M: Model> {
    /// The address to the struct.
    pub struct_address: Box<Address<M>>,

    /// The field that the address points to.
    pub id: ID<symbol::Field>,
}

/// The offset from the start or end of a tuple.
///
/// Primarily used for indexing into a tuple element with an offset.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Offset {
    /// The offset is from the start of the tuple (0-indexed).
    FromStart(usize),

    /// The offset is from the end of the tuple (0-indexed).
    FromEnd(usize),
}

/// The address points to an element in a tuple.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple<M: Model> {
    /// The address to the tuple.
    pub tuple_address: Box<Address<M>>,

    /// The offset of the element to access.
    pub offset: Offset,
}

/// Represents an address to a particular location in memory.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Address<M: Model> {
    Parameter(ID<Parameter>),
    Alloca(ID<Alloca<M>>),
    Field(Field<M>),
    Tuple(Tuple<M>),

    /// The address is stored in a value.
    Value(Value<M>),
}
