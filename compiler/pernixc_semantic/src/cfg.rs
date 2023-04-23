//! This module contains the definition of the [`ControlFlowGraph`] and all the traits that are
//! required to implement it.

use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    ops::{Index, IndexMut},
};

use getset::{CopyGetters, Getters};

use crate::symbol::{Uid, UniqueIdentifier};

/// Is an unique identifier used to identify a basic block in the [`ControlFlowGraph`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BasicBlockID(Uid);

impl UniqueIdentifier for BasicBlockID {
    fn fresh() -> Self {
        Self(Uid::fresh())
    }
}

/// A trait for the jump instruction of a [`ControlFlowGraph`].
pub trait JumpInstruction {
    /// Returns the [`BasicBlockID`] of the jump target.
    fn jump_target(&self) -> BasicBlockID;
}

/// A trait for the return instruction of a [`ControlFlowGraph`].
pub trait ReturnInstruction {
    /// The type of the return value of the function.
    type Value;

    /// Returns the return value of the function.
    fn return_value(&self) -> Option<&Self::Value>;
}

/// A trait for the conditional jump instruction of a [`ControlFlowGraph`].
pub trait ConditionalJumpInstruction {
    /// The type of the condition value.
    type Value;

    /// Returns the condition value.
    fn condition_value(&self) -> &Self::Value;

    /// Returns the [`BasicBlockID`] of the jump target if the condition is true.
    fn true_jump_target(&self) -> BasicBlockID;

    /// Returns the [`BasicBlockID`] of the jump target if the condition is false.
    fn false_jump_target(&self) -> BasicBlockID;
}

/// Is a trait for the basic instruction of a [`ControlFlowGraph`].
pub trait IRinstruction {}

/// Is a trait for the backend of a [`ControlFlowGraph`].
///
/// This trait is used to specify all the types of the instructions of the [`ControlFlowGraph`].
pub trait InstructionBackend {
    /// The type of the value of the [`ControlFlowGraph`].
    type Value: Debug + Clone;

    /// The type of the instruction of the [`ControlFlowGraph`].
    type IR: IRinstruction + Debug + Clone;

    /// The type of the jump instruction of the [`ControlFlowGraph`].
    type Jump: JumpInstruction + Debug + Clone;

    /// The type of the return instruction of the [`ControlFlowGraph`].
    type Return: ReturnInstruction<Value = Self::Value> + Debug + Clone;

    /// The type of the conditional jump instruction of the [`ControlFlowGraph`].
    type ConditionalJump: ConditionalJumpInstruction<Value = Self::Value> + Debug + Clone;
}

/// Represents a unit of instructions that are stored in the [`BasicBlock`] for a sequential
/// execution.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Instruction<T: InstructionBackend> {
    Jump(T::Jump),
    Return(T::Return),
    ConditionalJump(T::ConditionalJump),
    IR(T::IR),
}

/// Represents a unit of sequential instructions that are executed in order.
///
/// These blocks can be connected to each other by various kinds of jump instructions in order to
/// represent the full control flow of a function.
#[derive(Debug, Clone, Getters, CopyGetters)]
pub struct BasicBlock<T: InstructionBackend> {
    /// The list of instructions that are stored in the basic block.
    #[get = "pub"]
    instructions: Vec<Instruction<T>>,

    /// The list of [`BasicBlockID`] of the basic blocks that are successors of this basic block.
    #[get = "pub"]
    successors: HashSet<BasicBlockID>,

    /// The list of [`BasicBlockID`] of the basic blocks that are predecessors of this basic block.
    #[get = "pub"]
    predecessors: HashSet<BasicBlockID>,

    /// The unique identifier of the basic block.
    #[get_copy = "pub"]
    basic_block_id: BasicBlockID,
}

/// Represents a control flow graph of a function.
///
/// The graph is represented by [`BasicBlock`]s that are connected to each other by jump
/// instructions. Each [`BasicBlock`] is assigned with a unique [`BasicBlockID`], which enables
/// the graph to be represented by storing the [`BasicBlockID`]s as edges.
///
/// The [`ControlFlowGraph`] is primarily used in all kinds of intermediate representation passes to
/// represent the control flow of a function. It accepts a [`InstructionBackend`] generic type
/// parameter to accommodate different kinds of intermediate representations throughout the semantic
/// analysis.
#[derive(Debug, Clone, CopyGetters)]
pub struct ControlFlowGraph<T: InstructionBackend> {
    basic_blocks_by_id: HashMap<BasicBlockID, BasicBlock<T>>,

    /// The [`BasicBlockID`] of the entry block of the control flow graph.
    #[get_copy = "pub"]
    entry_block: BasicBlockID,
}

impl<T: InstructionBackend> ControlFlowGraph<T> {
    /// Creates a new [`ControlFlowGraph`] with a single basic block.
    #[must_use]
    pub fn new() -> Self {
        let new_id = BasicBlockID::fresh();
        let mut basic_blocks_by_id = HashMap::new();
        basic_blocks_by_id.insert(
            new_id,
            BasicBlock {
                instructions: Vec::new(),
                successors: HashSet::new(),
                predecessors: HashSet::new(),
                basic_block_id: new_id,
            },
        );
        Self {
            basic_blocks_by_id,
            entry_block: new_id,
        }
    }
}

impl<T: InstructionBackend> Default for ControlFlowGraph<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: InstructionBackend> Index<BasicBlockID> for ControlFlowGraph<T> {
    type Output = BasicBlock<T>;

    fn index(&self, index: BasicBlockID) -> &Self::Output {
        self.basic_blocks_by_id
            .get(&index)
            .expect("Basic block not found")
    }
}

impl<T: InstructionBackend> IndexMut<BasicBlockID> for ControlFlowGraph<T> {
    fn index_mut(&mut self, index: BasicBlockID) -> &mut Self::Output {
        self.basic_blocks_by_id
            .get_mut(&index)
            .expect("Basic block not found")
    }
}
