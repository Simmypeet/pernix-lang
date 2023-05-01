//! Contains all the definitions of instructions used in the HIR.

use std::marker::PhantomData;

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};

use super::{
    value::{Address, Value},
    AllocaID, RegisterID, TypeSystem,
};
use crate::cfg::{
    BasicBlockID, BasicInstruction, ConditionalJumpInstruction, InstructionBackend,
    JumpInstruction, ReturnInstruction,
};

/// Represents a jump instruciton in the HIR.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Jump {
    pub(super) jump_target: BasicBlockID,
}

impl JumpInstruction for Jump {
    fn jump_target(&self) -> BasicBlockID { self.jump_target }
}

/// Represents a conditional jump instruction in the HIR.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConditionalJump<T: TypeSystem> {
    pub(super) condition: Value<T>,
    pub(super) true_jump_target: BasicBlockID,
    pub(super) false_jump_target: BasicBlockID,
}

impl<T: TypeSystem> ConditionalJumpInstruction for ConditionalJump<T> {
    type Value = Value<T>;

    fn condition_value(&self) -> &Self::Value { &self.condition }

    fn true_jump_target(&self) -> BasicBlockID { self.true_jump_target }

    fn false_jump_target(&self) -> BasicBlockID { self.false_jump_target }
}

/// Represents a return instruction in the HIR.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Return<T: TypeSystem> {
    pub(super) return_value: Option<Value<T>>,
}

impl<T: TypeSystem> ReturnInstruction for Return<T> {
    type Value = Value<T>;

    fn return_value(&self) -> Option<&Self::Value> { self.return_value.as_ref() }
}

/// Represents a basic instruction in the HIR.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Basic<T: TypeSystem> {
    RegisterAssignment(RegisterAssignment),
    VariableDeclaration(VariableDeclaration),
    Store(Store<T>),
}

/// Represents a register assignment instruction in the HIR.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, CopyGetters)]
pub struct RegisterAssignment {
    /// Gets the [`RegisterID`] of the register that is being assigned to.
    #[get_copy = "pub"]
    pub(super) register_id: RegisterID,
}

/// Represents a variable declaration instruction in the HIR.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Getters, CopyGetters)]
pub struct VariableDeclaration {
    /// Gets the [`AllocaID`] that was declared at the point of this instruction.
    #[get_copy = "pub"]
    pub(super) alloca_id: AllocaID,
}

/// Represents a store a value to an address instruction in the HIR.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct Store<T: TypeSystem> {
    /// Gets the [`Address`] that is being stored to.
    #[get = "pub"]
    pub(super) address: Address,

    /// Gets the [`Value`] that is being stored to the [`Address`].
    #[get = "pub"]
    pub(super) value: Value<T>,
}

impl<T: TypeSystem> BasicInstruction for Basic<T> {}

/// Is a struct that implements [`InstructionBackend`] for the HIR.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Backend<T: TypeSystem>(PhantomData<T>);

impl<T: TypeSystem> InstructionBackend for Backend<T> {
    type Basic = Basic<T>;
    type ConditionalJump = ConditionalJump<T>;
    type Jump = Jump;
    type Return = Return<T>;
    type Value = Value<T>;
}
