//! Contains all the definitions of instructions used in the HIR.

use std::marker::PhantomData;

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use pernixc_source::Span;

use super::{
    value::{Address, Value},
    AllocaID, RegisterID, ScopeID, TypeSystem,
};
use crate::cfg::{
    BasicBlockID, BasicInstruction, ConditionalJumpInstruction, InstructionBackend,
    JumpInstruction, ReturnInstruction,
};

/// Specifies which expression generated this instruction.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
pub enum JumpSource {
    /// The jump instruction was generated from a `break` expression.
    Express(Span),

    /// The jump instruction was generated from a `continue` expression.
    Continue(Span),

    /// The jump instruction was generated from a `break` expression.
    Break(Span),
}

/// Represents a jump instruciton in the HIR.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct Jump {
    pub(super) jump_target: BasicBlockID,

    /// Gets the source of this jump instruction.
    ///
    /// If `None`, this jump instruction was generated implicitly by the compiler.
    #[get = "pub"]
    pub(super) jump_source: Option<JumpSource>,
}

impl JumpInstruction for Jump {
    fn jump_target(&self) -> BasicBlockID {
        self.jump_target
    }
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

    fn condition_value(&self) -> &Self::Value {
        &self.condition
    }

    fn true_jump_target(&self) -> BasicBlockID {
        self.true_jump_target
    }

    fn false_jump_target(&self) -> BasicBlockID {
        self.false_jump_target
    }
}

/// Represents a return instruction in the HIR.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Return<T: TypeSystem> {
    pub(super) return_value: Option<Value<T>>,
}

impl<T: TypeSystem> ReturnInstruction for Return<T> {
    type Value = Value<T>;

    fn return_value(&self) -> Option<&Self::Value> {
        self.return_value.as_ref()
    }
}

/// Represents a basic instruction in the HIR.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Basic<T: TypeSystem> {
    RegisterAssignment(RegisterAssignment),
    VariableDeclaration(VariableDeclaration),
    Store(Store<T>),
    ScopePush(ScopePush),
    ScopePop(ScopePop),
}

/// Is an instruction that is inserted every time a new scope (*block*) is entered.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, CopyGetters)]
pub struct ScopePush {
    /// The ID of the scope that is entered.
    #[get_copy = "pub"]
    pub(super) scope_id: ScopeID,
}

/// Is an instruction executed every time a scope (*block*) is exited.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, CopyGetters)]
pub struct ScopePop {
    /// The ID of the scope that is exited.
    #[get_copy = "pub"]
    pub(super) scope_id: ScopeID, 
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

    /// Specifies the location in the source code that generates this instruction.
    #[get = "pub"]
    pub(super) span: Span,
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
