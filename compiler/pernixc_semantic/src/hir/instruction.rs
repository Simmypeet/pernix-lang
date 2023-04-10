//! Contains a list of instructions that are used in the high-level intermediate representation.

use derive_more::From;
use enum_as_inner::EnumAsInner;

use super::value::{Value, ValueType};

/// The instruction of the high-level intermediate representation.
#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Instruction<T: ValueType> {
    EvaluateValue(Value<T>),
    VariableStore(VariableStore<T>),
}

/// Represents a variable value initialization instruction.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariableStore<T: ValueType> {
    /// The variable index in the [`super::HIR::local_variables()`] list that will be initialized.
    pub variable_index: usize,

    /// The value that will be assigned to the variable.
    pub value: Value<T>,
}
