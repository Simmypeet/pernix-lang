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
#[derive(Debug, Clone, Getters, CopyGetters)]
pub struct ControlFlowGraph<Inst, Value> {
    /// Is the list of basic blocks in the control flow graph.
    #[get = "pub"]
    ids_by_basic_block: HashMap<BasicBlockID, BasicBlock<Inst, Value>>,

    /// Gets the [`BasicBlockID`] that points to the entry basic block.
    #[get_copy = "pub"]
    entry_block_id: BasicBlockID,
}

impl<Inst, Value> ControlFlowGraph<Inst, Value> {
    /// Creates a new [`ControlFlowGraph`] with a single entry basic block.
    #[must_use]
    pub fn new() -> Self {
        let mut cfg = Self {
            ids_by_basic_block: HashMap::new(),
            entry_block_id: BasicBlockID(0),
        };

        let id = cfg.create_new_block();
        cfg.entry_block_id = id;

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
        // use atomic as a counter
        static COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(1);

        let id = BasicBlockID(COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed));

        self.ids_by_basic_block.insert(id, BasicBlock {
            basic_block_id: id,
            successor_block_ids: Vec::new(),
            predecessor_block_ids: Vec::new(),
            instructions: Vec::new(),
        });

        id
    }

    /// Adds a jump instruction to the end of `from` that jumps to `to`.
    ///
    /// # Panics
    /// Panics if `from` or `to` are not valid basic block ids.
    pub fn add_jump(&mut self, from: BasicBlockID, to: BasicBlockID) {
        self[from].successor_block_ids.push(to);
        self[to].predecessor_block_ids.push(from);
        self[from]
            .instructions
            .push(Instruction::TerminalInstruction(
                JumpInstruction { basic_block_id: to }.into(),
            ));
    }

    /// Adds a conditional jump instruction to the end of `from` that jumps to `true_basic_block_id`
    /// or `false_basic_block_id` depending on the value of `condition`.
    ///
    /// # Panics
    /// Panics if any of the basic block ids are not valid.
    pub fn add_conditional_jump(
        &mut self,
        from: BasicBlockID,
        true_basic_block_id: BasicBlockID,
        false_basic_block_id: BasicBlockID,
        condition: Value,
    ) {
        // add successor block list
        self[from].successor_block_ids.push(true_basic_block_id);
        self[from].successor_block_ids.push(false_basic_block_id);

        // add predecessor block list
        self[true_basic_block_id].predecessor_block_ids.push(from);
        self[false_basic_block_id].predecessor_block_ids.push(from);

        // add jump instruction
        self[from]
            .instructions
            .push(Instruction::TerminalInstruction(
                ConditionalJumpInstruction {
                    condition,
                    true_basic_block_id,
                    false_basic_block_id,
                }
                .into(),
            ));
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
    #[get = "pub"]
    instructions: Vec<Instruction<Inst, Value>>,
}

impl<Inst, Value> BasicBlock<Inst, Value> {
    /// Adds an instruction to the basic block.
    pub fn add_instruction(&mut self, instruction: Inst) {
        self.instructions
            .push(Instruction::IRInstruction(instruction));
    }

    /// Adds a return instruction to the basic block.
    pub fn return_value(&mut self, value: Option<Value>) {
        self.instructions.push(Instruction::TerminalInstruction(
            ReturnInstruction { value }.into(),
        ));
    }

    /// Gets the number of instructions in the basic block.
    #[must_use]
    pub fn len(&self) -> usize { self.instructions.len() }

    /// Checks if the basic block has no instructions.
    #[must_use]
    pub fn is_empty(&self) -> bool { self.instructions.is_empty() }
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
    pub basic_block_id: BasicBlockID,
}

/// Is a conditional jump instruction that will be executed in the control flow graph.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConditionalJumpInstruction<Value> {
    /// The value that will be evaluated to determine whether to jump to the target block.
    pub condition: Value,

    /// The id of the basic block that will be jumped to if the condition is true.
    pub true_basic_block_id: BasicBlockID,

    /// The id of the basic block that will be jumped to if the condition is false.
    pub false_basic_block_id: BasicBlockID,
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
