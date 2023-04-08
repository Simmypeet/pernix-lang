//! Contains all the code for the control flow graph.

use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
};

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};

/// Is an identifier used to identify a basic block in the control flow graph.
///
/// The identifier is only valid for the control flow graph that it was created from.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BasicBlockID(usize);

impl BasicBlockID {
    /// Is the identifier of the entry basic block.
    ///
    /// Every control flow graph has at least one entry basic block that is the first basic block to
    /// be executed.
    pub const ENTRY_BLOCK_ID: Self = Self(0);
}

/// Represents a control-flow graph (CFG) for a function.
///
/// The control-flow graph is used as an intermediate representation for both the high-level and
/// mid-level IRs.
///
/// # Generic Parameters
///
/// Based on the intermediate representation format, the control-flow graph takes different forms of
/// instruction types via the generic parameters `Inst` and `Value`.
///
/// - `Inst` is the instruction type that the control-flow graph uses.
/// - `Value` is the type that represents a value in the control-flow graph. It's used in such cases
///   as the return value of a function.
#[derive(Debug, Clone, Getters)]
pub struct ControlFlowGraph<Inst, Value> {
    /// Is the list of basic blocks in the control flow graph.
    #[get = "pub"]
    ids_by_basic_block: HashMap<BasicBlockID, BasicBlock<Inst, Value>>,

    /// Is the list of terminal blocks in the control flow graph.
    ///
    /// Terminal blocks are blocks that do not have any outgoing edges.
    #[get = "pub"]
    terminal_block_ids: Vec<BasicBlockID>,

    // internally used to generate unique basic block ids.
    id_counter: usize,
}

impl<Inst, Value> ControlFlowGraph<Inst, Value> {
    /// Creates a new [`ControlFlowGraph`] with a single entry basic block.
    #[must_use]
    pub fn new() -> Self {
        let mut cfg = Self {
            ids_by_basic_block: HashMap::new(),
            terminal_block_ids: Vec::new(),
            id_counter: 0,
        };

        cfg.create_new_block();

        cfg
    }

    /// Creates a new basic block in the control flow graph.
    ///
    /// The created basic block is not connected to any other basic blocks and contains no
    /// instructions.
    ///
    /// # Returns
    /// The [`BasicBlockID`] of the newly created basic block.
    pub fn create_new_block(&mut self) -> BasicBlockID {
        let id = BasicBlockID(self.id_counter);
        self.id_counter += 1;

        self.ids_by_basic_block.insert(id, BasicBlock {
            basic_block_id: id,
            successor_block_ids: Vec::new(),
            predecessor_block_ids: Vec::new(),
            instructions: Vec::new(),
        });

        id
    }
}

impl<Inst, Value> Default for ControlFlowGraph<Inst, Value> {
    fn default() -> Self { Self::new() }
}

impl<Inst, Value> Index<BasicBlockID> for ControlFlowGraph<Inst, Value> {
    type Output = BasicBlock<Inst, Value>;

    fn index(&self, index: BasicBlockID) -> &Self::Output { &self.ids_by_basic_block[&index] }
}

impl<Inst, Value> IndexMut<BasicBlockID> for ControlFlowGraph<Inst, Value> {
    fn index_mut(&mut self, index: BasicBlockID) -> &mut Self::Output {
        self.ids_by_basic_block.get_mut(&index).unwrap()
    }
}

/// Represents a basic block in the control flow graph.
///
/// The basic block contains a sequence of instructions that are executed in order.
#[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
pub struct BasicBlock<Inst, Value> {
    /// Gets the [`BasicBlockID`] that is unique to the control flow graph that it was created
    /// from.
    #[get_copy = "pub"]
    basic_block_id: BasicBlockID,

    /// The list of block ids that are predecessors of this block.
    ///
    /// In other words, the list of blocks that can jump to this block.
    pub predecessor_block_ids: Vec<BasicBlockID>,

    /// The list of block ids that are successors of this block.
    ///
    /// In other words, the list of blocks that can be jumped to from this block.
    pub successor_block_ids: Vec<BasicBlockID>,

    /// The list of instructions that will be executed in the basic block.
    instructions: Vec<Instruction<Inst, Value>>,
}

impl<Inst, Value> BasicBlock<Inst, Value> {
    /// Checks whether the basic block is the entry block of the control flow graph.
    #[must_use]
    pub fn is_entry_block(&self) -> bool { self.basic_block_id.0 == 0 }

    /// Adds an instruction to the basic block.
    pub fn add_instruction(&mut self, instruction: Instruction<Inst, Value>) {
        self.instructions.push(instruction);
    }
}

impl<Inst, Value> Index<usize> for BasicBlock<Inst, Value> {
    type Output = Instruction<Inst, Value>;

    fn index(&self, index: usize) -> &Self::Output { &self.instructions[index] }
}

impl<Inst, Value> IndexMut<usize> for BasicBlock<Inst, Value> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output { &mut self.instructions[index] }
}

/// Is a return instruction that will be executed in the control flow graph.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReturnInstruction<Value> {
    /// The value that will be returned in the return instruction.
    ///
    /// If the value is `None`, then the return instruction will return void.
    pub value: Option<Value>,
}

/// Is a jump instruction that will be executed in the control flow graph.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct JumpInstruction {
    /// The index of the basic block that will be jumped to.
    pub target_block_index: BasicBlockID,
}

/// Is a conditional jump instruction that will be executed in the control flow graph.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConditionalJumpInstruction<Value> {
    /// The value that will be evaluated to determine whether to jump to the target block.
    pub condition: Value,

    /// The id of the basic block that will be jumped to if the condition is true.
    pub true_target_block_id: BasicBlockID,

    /// The id of the basic block that will be jumped to if the condition is false.
    pub false_target_block_id: BasicBlockID,
}

/// Represents an instruction that will be executed in the control flow graph.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Instruction<Inst, Value> {
    IRInstruction(Inst),
    TerminalInstruction(TerminalInstruction<Value>),
}

/// Represents an instruction that cause the control flow graph to terminate.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum TerminalInstruction<Value> {
    ReturnInstruction(ReturnInstruction<Value>),
    JumpInstruction(JumpInstruction),
    ConditionalJumpInstruction(ConditionalJumpInstruction<Value>),
}
