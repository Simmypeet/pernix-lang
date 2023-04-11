//! Contains a list of instructions that are used in the high-level intermediate representation.

use derive_more::From;
use enum_as_inner::EnumAsInner;

use super::value::{Value, ValueType, VariableID};
use crate::symbol::FieldID;

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
    pub variable_id: VariableID,

    /// Specifies how the initialization of the store should be performed.
    pub initialization: Initialization<T>,
}

/// Is an initialization instruction that is used in struct initialization.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructInitialization<T: ValueType> {
    /// List of fields to initialize and their values.
    pub field_initializations: Vec<(FieldID, Value<T>)>,
}

/// Is an enum that specifies how the initialization of the store should be performed.
#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner, From)]
pub enum Initialization<T: ValueType> {
    /// No value to initialize the store with.
    ///
    /// Usually, this is used when the variable is a unit type (void).
    None,

    /// Specifies that the store should be initialized with a value.
    Value(Value<T>),

    /// Specifies that the store should be initialized with a struct.
    StructInitialization(StructInitialization<T>),
}
