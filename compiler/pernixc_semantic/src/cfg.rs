//! This module contains the definition of the [`ControlFlowGraph`] and all the traits that are
//! required to implement it.

use std::{collections::HashSet, fmt::Debug};

use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use pernixc_system::{
    arena::{Arena, InvalidIDError},
    create_symbol,
};

create_symbol! {
    /// Represents a unit of sequential instructions that are executed in order.
    ///
    /// These blocks can be connected to each other by various kinds of jump instructions in order to
    /// represent the full control flow of a function.
    #[derive(Debug, Clone, Getters, CopyGetters)]
    pub struct BasicBlock<T: InstructionBackend> {
        /// Specifies whether if the basic block is an entry block or not.
        #[get_copy = "pub"]
        is_entry: bool,

        /// The list of instructions that are stored in the basic block.
        #[get = "pub"]
        instructions: Vec<Instruction<T>>,

        /// The list of instructions that were inserted in the basic block after any *terminator*
        /// instructions.
        #[get = "pub"]
        unreachable_instructions: Vec<Instruction<T>>,

        /// The list of [`BasicBlockID`] of the basic blocks that are successors of this basic block.
        #[get = "pub"]
        successors: HashSet<BasicBlockID>,

        /// The list of [`BasicBlockID`] of the basic blocks that are predecessors of this basic block.
        #[get = "pub"]
        predecessors: HashSet<BasicBlockID>,
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
pub trait BasicInstruction {}

/// Is a trait for the backend of a [`ControlFlowGraph`].
///
/// This trait is used to specify all the types of the instructions of the [`ControlFlowGraph`].
pub trait InstructionBackend {
    /// The type of the value of the [`ControlFlowGraph`].
    type Value: Debug + Clone;

    /// The basic instruction type of the [`ControlFlowGraph`].
    type Basic: BasicInstruction + Debug + Clone;

    /// The jump instruction type of the [`ControlFlowGraph`].
    type Jump: JumpInstruction + Debug + Clone;

    /// The return instruction type of the [`ControlFlowGraph`].
    type Return: ReturnInstruction<Value = Self::Value> + Debug + Clone;

    /// The conditional jump instruction type of the [`ControlFlowGraph`].
    type ConditionalJump: ConditionalJumpInstruction<Value = Self::Value> + Debug + Clone;
}

/// Represents a unit of instructions that are stored in the [`BasicBlock`] for a sequential
/// execution.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Instruction<T: InstructionBackend> {
    Jump(T::Jump),
    Return(T::Return),
    ConditionalJump(T::ConditionalJump),
    Basic(T::Basic),
}

impl<T: InstructionBackend> BasicBlock<T> {
    fn new(is_entry: bool) -> Self {
        Self {
            instructions: Vec::new(),
            unreachable_instructions: Vec::new(),
            successors: HashSet::new(),
            predecessors: HashSet::new(),
            is_entry,
        }
    }

    /// Checks if the [`BasicBlock`] is already terminated with one of the terminator instructions.
    #[must_use]
    pub fn is_terminated(&self) -> bool {
        self.instructions.last().map_or(false, |instruction| {
            matches!(
                instruction,
                Instruction::Jump(_) | Instruction::Return(_) | Instruction::ConditionalJump(_)
            )
        }) || (!self.is_entry && self.predecessors().is_empty())
    }

    /// Adds a new basic instruction to the [`BasicBlock`].
    pub fn add_basic_instruction(&mut self, instruction: T::Basic) {
        self.add_instruction(Instruction::Basic(instruction));
    }

    /// Adds a new return instruction to the [`BasicBlock`].
    pub fn add_return_instruction(&mut self, instruction: T::Return) {
        self.add_instruction(Instruction::Return(instruction));
    }

    /// Gets an iterator over both the reachable and unreachable instructions of the [`BasicBlock`].
    #[must_use]
    pub fn all_instructions(&self) -> impl Iterator<Item = &Instruction<T>> {
        self.instructions
            .iter()
            .chain(self.unreachable_instructions.iter())
    }

    fn add_instruction(&mut self, instruction: Instruction<T>) {
        if self.is_terminated() {
            self.unreachable_instructions.push(instruction);
        } else {
            self.instructions.push(instruction);
        }
    }
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
#[derive(Debug, CopyGetters)]
pub struct ControlFlowGraph<T: InstructionBackend> {
    basic_blocks: Arena<BasicBlock<T>>,

    /// The [`BasicBlockID`] of the entry block of the control flow graph.
    #[get_copy = "pub"]
    entry_block: BasicBlockID,
}

impl<T: InstructionBackend> ControlFlowGraph<T> {
    /// Creates a new [`ControlFlowGraph`] with a single basic block.
    #[must_use]
    pub fn new() -> Self {
        let mut basic_blocks = Arena::new();
        let entry_block = basic_blocks.insert(BasicBlock::new(true));

        Self {
            basic_blocks,
            entry_block,
        }
    }

    /// Adds a new basic block to the [`ControlFlowGraph`].
    ///
    /// # Returns
    /// The [`BasicBlockID`] of the newly added basic block.
    pub fn new_basic_block(&mut self) -> BasicBlockID {
        self.basic_blocks.insert(BasicBlock::new(false))
    }

    /// Gets a reference to the [`BasicBlock`] with the given [`BasicBlockID`].
    ///
    /// # Errors
    /// If the given [`BasicBlockID`] is invalid.
    pub fn get(&self, id: BasicBlockID) -> Result<&BasicBlockSymbol<T>, InvalidIDError> {
        self.basic_blocks.get(id)
    }

    /// Gets a mutable reference to the [`BasicBlock`] with the given [`BasicBlockID`].
    ///
    /// # Errors
    /// If the given [`BasicBlockID`] is invalid.
    pub fn get_mut(
        &mut self,
        id: BasicBlockID,
    ) -> Result<&mut BasicBlockSymbol<T>, InvalidIDError> {
        self.basic_blocks.get_mut(id)
    }

    /// Adds a new [`ConditionalJumpInstruction`] into the [`BasicBlock`] of the given ID.
    ///
    /// # Parameters
    /// - `id`: The [`BasicBlockID`] of the [`BasicBlock`] to add the [`ConditionalJumpInstruction`]
    ///   into.
    /// - `jump`: The [`ConditionalJumpInstruction`] to add.
    ///
    /// # Errors
    /// - [`InvalidIDError`]: if the given [`BasicBlockID`], or the jump target IDs are invalid.
    pub fn add_conditional_jump_instruction(
        &mut self,
        id: BasicBlockID,
        jump: T::ConditionalJump,
    ) -> Result<(), InvalidIDError> {
        // check for id validity
        if self.get(jump.true_jump_target()).is_err() || self.get(jump.false_jump_target()).is_err()
        {
            return Err(InvalidIDError);
        }
        let true_jump_target = jump.true_jump_target();
        let false_jump_target = jump.false_jump_target();

        // add the jump instruction
        let is_terminated = {
            let target = self.get_mut(id)?;
            let is_terminated = target.is_terminated();
            target.add_instruction(Instruction::ConditionalJump(jump));

            if !is_terminated {
                target.successors.insert(true_jump_target);
                target.successors.insert(false_jump_target);
            }

            is_terminated
        };

        // add predecessor for both jump target
        if !is_terminated {
            {
                let true_jump_target = self.get_mut(true_jump_target)?;
                true_jump_target.predecessors.insert(id);
            }
            {
                let false_jump_target = self.get_mut(false_jump_target)?;
                false_jump_target.predecessors.insert(id);
            }
        }

        Ok(())
    }

    /// Adds a new [`JumpInstruction`] into the [`BasicBlock`] of the given ID.
    ///
    /// # Parameters
    /// - `id`: The [`BasicBlockID`] of the [`BasicBlock`] to add the [`JumpInstruction`] into.
    /// - `jump`: The [`JumpInstruction`] to add.
    ///
    /// # Errors
    /// - [`InvalidIDError`]: if the given [`BasicBlockID`], or the jump target ID is invalid.
    pub fn add_jump_instruction(
        &mut self,
        id: BasicBlockID,
        jump: T::Jump,
    ) -> Result<(), InvalidIDError> {
        // check for id validity
        if self.get(jump.jump_target()).is_err() {
            return Err(InvalidIDError);
        }
        let jump_target = jump.jump_target();

        // add the jump instruction
        let is_terminated = {
            let target = self.get_mut(id)?;
            let is_terminated = target.is_terminated();
            target.add_instruction(Instruction::Jump(jump));

            if !is_terminated {
                target.successors.insert(jump_target);
            }

            is_terminated
        };

        // add predecessor for both jump target
        if !is_terminated {
            let jump_target = self.get_mut(jump_target)?;
            jump_target.predecessors.insert(id);
        }

        Ok(())
    }
}

impl<T: InstructionBackend> Default for ControlFlowGraph<T> {
    fn default() -> Self {
        Self::new()
    }
}
