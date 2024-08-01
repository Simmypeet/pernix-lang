//! Contains the definition of [`Register`] and [`Assignment`].
//!
//! The register is a place where SSA values are stored. The assignment is the
//! value that is stored in the register.

use std::collections::HashMap;

use enum_as_inner::EnumAsInner;
use pernixc_base::source_file::Span;

use super::address::{Address, Memory};
use crate::{
    arena::ID,
    symbol::{self, CallableID, Field},
    type_system::{
        instantiation::Instantiation,
        model::Model,
        term::r#type::{Qualifier, Type},
    },
};

/// Represents an element of a [`Tuple`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TupleElement<M: Model> {
    /// The value of the tuple element.
    pub value: ID<Register<M>>,

    /// Whether the tuple element is unpacked.
    pub is_unpacked: bool,
}

/// Represents a tuple of values.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple<M: Model> {
    /// The elements of kthe tuple.
    pub elements: Vec<TupleElement<M>>,
}

/// An enumeration of either moving or copying loads.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LoadKind {
    /// The value is memcpy'd from the address and the value in the address is
    /// invalidated.
    Move,

    /// The value is copied from the address via `Copy` trait.
    Copy,
}

/// Represents a load/read from an address in memory.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Load<M: Model> {
    /// The address where the value is stored and will be read from.
    pub address: Address<Memory<M>>,

    /// The kind of load.
    pub kind: LoadKind,
}

/// Obtains a reference at the given address.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReferenceOf<M: Model> {
    /// The address to the value.
    pub address: Address<Memory<M>>,

    /// The qualfier of the reference.
    pub qualifier: Qualifier,

    /// Checks if the reference of operation is local (with `@` operator).
    pub is_local: bool,
}

/// An enumeration of the different kinds of prefix operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PrefixOperator {
    /// The value must be the signed numbers type.
    Negate,

    /// The value must be the boolean type.
    LogicalNot,

    /// The value must be integers.
    BitwiseNot,

    /// The value can be any type.
    Local,

    /// The value must be a type of `local`.
    Unlocal,
}

/// A value applied with a prefix operator.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Prefix<M: Model> {
    /// The operand of the prefix operator.
    pub operand: ID<Register<M>>,

    /// The operator applied to the operand.
    pub operator: PrefixOperator,
}

/// Represents a numeric literal value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Numeric {
    /// The numeric value for the integer part as a string.
    pub integer_string: String,

    /// The numeric value for the decimal part as a string.
    pub decimal_stirng: Option<String>,
}

/// Represents a boolean value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Boolean {
    /// The value of the boolean.
    pub value: bool,
}

/// Represents a struct value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Struct<M: Model> {
    /// The struct ID of the struct.
    pub struct_id: ID<symbol::Struct>,

    /// The field initializers of the struct.
    pub initializers_by_field_id: HashMap<ID<Field>, ID<Register<M>>>,
}

/// Represents a variant value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant<M: Model> {
    /// The variant ID of the variant.
    pub variant_id: ID<symbol::Variant>,

    /// The field initializers of the variant.
    pub associated_value: Option<ID<Register<M>>>,
}

/// Represents a function call.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionCall<M: Model> {
    /// The ID of the function that is called.
    pub callable_id: CallableID,

    /// The arguments supplied to the function.
    pub arguments: Vec<ID<Register<M>>>,

    /// The generic instantiations of the function.
    pub generic_instantiations: Instantiation<M>,
}

/// An enumeration of the different kinds of values that can be assigned in the
/// register.
#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
#[allow(missing_docs)]
pub enum Assignment<M: Model> {
    Tuple(Tuple<M>),
    Load(Load<M>),
    ReferenceOf(ReferenceOf<M>),
    Prefix(Prefix<M>),
    Numeric(Numeric),
    Boolean(Boolean),
    Struct(Struct<M>),
    Variant(Variant<M>),
    FunctionCall(FunctionCall<M>),

    /// The value is an error.
    Errored,
}

/// Represents a register in the SSA from.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Register<M: Model> {
    /// The value stored in the register.
    pub assignment: Assignment<M>,

    /// The type of the value stored in the register.
    pub r#type: Type<M>,

    /// The span where the value was defined.
    pub span: Option<Span>,
}
