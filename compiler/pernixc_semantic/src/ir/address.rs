//! Contains the definition of [`Address`] and its variants.
//!
//! ## `Address<Memory<_>>` vs `Address<ID<Register<_>>`
//!
//! The `Address<Memory<_>>` variant is used to represent a **real** memory in
//! the program. This can be used in various instructions such as `Load` and
//! `Store`.
//!
//! The `Address<ID<Register<_>>>` variant is used to represent a **virtual**
//! memory in the registers. This is used to address a temporary value that is
//! stored in a register.

use enum_as_inner::EnumAsInner;

use super::{alloca::Alloca, register::Register};
use crate::{
    arena::ID,
    type_system::model::Model,
    symbol::{self, Parameter},
};

/// The address points to a field in a struct.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Field<B> {
    /// The address to the struct.
    pub struct_address: Box<Address<B>>,

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
pub struct Tuple<B> {
    /// The address to the tuple.
    pub tuple_address: Box<Address<B>>,

    /// The offset of the element to access.
    pub offset: Offset,
}

/// Represents a stack location.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Stack<M: Model> {
    Alloca(ID<Alloca<M>>),
    Parameter(ID<Parameter>),
}

/// Represents a real memory location.
///
/// This is used to represent the base address of a memory location for the
/// [`Address`] type.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Memory<M: Model> {
    Parameter(ID<Parameter>),
    Alloca(ID<Alloca<M>>),

    /// The memory pointer is stored in a register.
    ReferenceValue(ID<Register<M>>),
}

/// Represents an address to a particular location in memory.
///
/// The type parameter `B` represents the base or starting address where the
/// rest of projections are based on.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Address<B> {
    Base(B),
    Field(Field<B>),
    Tuple(Tuple<B>),
}
