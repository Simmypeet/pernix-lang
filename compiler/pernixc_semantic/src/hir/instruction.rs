//! Contains a list of instructions that are used in the high-level intermediate representation.

use derive_more::From;
use enum_as_inner::EnumAsInner;

use super::value::HirValue;

/// The instruction of the high-level intermediate representation.
#[derive(Debug, Clone, PartialEq, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Instruction {
    EvaluateValue(HirValue),
    VariableStore(VariableStore),
}

/// Represents a variable value initialization instruction.
#[derive(Debug, Clone, PartialEq)]
pub struct VariableStore {
    /// The variable index in the [`super::HIR::local_variables()`] list that will be initialized.
    pub variable_index: usize,

    /// The value that will be assigned to the variable.
    pub value: HirValue,
}
