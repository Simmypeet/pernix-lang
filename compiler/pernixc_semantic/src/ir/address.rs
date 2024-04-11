//! Contains the definition of [`Address`] and its variants.

use enum_as_inner::EnumAsInner;

use super::{alloca::Alloca, value::Value};
use crate::{arena::ID, symbol::Parameter};

/// The address points to a field in a struct.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Field {
    /// The address to the struct.
    pub struct_address: Box<Address>,

    /// The field that the address points to.
    pub field: ID<Field>,
}

/// Represents an address to a stack allocated value.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Stack {
    Parameter(ID<Parameter>),
    Alloca(ID<Alloca>),
}

/// Represents an address to a particular location in memory.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Address {
    Stack(Stack),
    Field(Field),

    /// The address is stored in a value.
    Value(Value),
}
