//! Contains a list of instructions that are used in the high-level intermediate representation.

use std::fmt::Debug;

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;

use super::value::{Value, ValueType, VariableID};
use crate::{
    cfg::{
        BasicBlockID, ConditionalJumpInstruction, IRinstruction, InstructionBackend,
        JumpInstruction, ReturnInstruction,
    },
    symbol::item::FieldID,
};

/// The instruction of the high-level intermediate representation.
#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Instruction<T: ValueType> {
    EvaluateValue(Value<T>),
    Store(Store<T>),
}

impl<T: ValueType> IRinstruction for Instruction<T> {}

/// Represents a storage's value store instruction.
#[derive(Debug, Clone, PartialEq, Eq, Getters)]
pub struct Store<T: ValueType> {
    /// The address of the store
    #[get = "pub"]
    pub(super) variable_id: VariableID,

    /// Specifies how the initialization of the store should be performed.
    #[get = "pub"]
    pub(super) initialization: Initialization<T>,
}

/// Is an initialization instruction that is used in struct initialization.
#[derive(Debug, Clone, PartialEq, Eq, Getters)]
pub struct StructInitialization<T: ValueType> {
    /// List of fields to initialize and their values.
    #[get = "pub"]
    pub(super) field_initializations: Vec<(FieldID, Value<T>)>,
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

/// Is a return terminator instruction.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct Return<T: ValueType> {
    /// The value that is returned by the function.
    #[get = "pub"]
    pub(super) value: Value<T>,
}

impl<T: ValueType> ReturnInstruction for Return<T> {
    type Value = Value<T>;

    fn return_value(&self) -> Option<&Self::Value> { todo!() }
}

/// Is a jump terminator instruction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Hash, Getters)]
pub struct Jump {
    /// The unique identifier of the basic block that is the target of the jump.
    #[get = "pub"]
    pub(super) target: BasicBlockID,
}

impl JumpInstruction for Jump {
    fn jump_target(&self) -> BasicBlockID { self.target }
}

/// Is a conditional jump terminator instruction.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct ConditionalJump<T: ValueType> {
    /// The unique identifier of the basic block that is the target of the jump if the condition is
    /// true.
    #[get = "pub"]
    pub(super) true_jump_target: BasicBlockID,

    /// The unique identifier of the basic block that is the target of the jump if the condition is
    /// false.
    #[get = "pub"]
    pub(super) false_jump_target: BasicBlockID,

    /// The value that is used to determine the jump target.
    #[get = "pub"]
    pub(super) value: Value<T>,
}

impl<T: ValueType> ConditionalJumpInstruction for ConditionalJump<T> {
    type Value = Value<T>;

    fn condition_value(&self) -> &Self::Value { &self.value }

    fn true_jump_target(&self) -> BasicBlockID { self.true_jump_target }

    fn false_jump_target(&self) -> BasicBlockID { self.false_jump_target }
}

/// Is a structure passed to the [`crate::control_flow_graph::ControlFlowGraph`] to express all the
/// types that are used in the high-level intermediate representation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Backend<T: ValueType + Clone + Debug>(std::marker::PhantomData<T>);

impl<T: ValueType + Clone + Debug> InstructionBackend for Backend<T> {
    type ConditionalJump = ConditionalJump<T>;
    type IR = Instruction<T>;
    type Jump = Jump;
    type Return = Return<T>;
    type Value = Value<T>;
}
