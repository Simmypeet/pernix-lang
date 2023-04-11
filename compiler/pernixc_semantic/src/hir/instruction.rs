//! Contains a list of instructions that are used in the high-level intermediate representation.

use derive_more::From;
use enum_as_inner::EnumAsInner;

use super::value::{Address, Value, ValueType};

/// The instruction of the high-level intermediate representation.
#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Instruction<T: ValueType> {
    EvaluateValue(Value<T>),
    Store(Store<T>),
}

/// Represents a storage's value store instruction.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Store<T: ValueType> {
    /// The address of the store
    pub store_address: Address,

    /// The value that will be assigned to the variable.
    pub value: Value<T>,
}
