//! Contains the definition of [`Block`] and [`ControlFlowGraph`].

use std::collections::HashSet;

use getset::{CopyGetters, Getters};

use super::instruction::Instruction;
use crate::arena::{Arena, ID};

/// Represents a list of instructions executed in sequence.
#[derive(Debug, Clone, PartialEq, Eq, Getters)]
pub struct Block {
    /// List of instructions that are executed in sequence.
    #[get = "pub"]
    instructions: Vec<Instruction>,
    /// List of instructions that will never be executed.
    #[get = "pub"]
    unreachables: Vec<Instruction>,
    /// List of blocks that are successors of this block.
    #[get = "pub"]
    successors: HashSet<ID<Block>>,
    /// List of blocks that are predecessors of this block.
    #[get = "pub"]
    predecessors: HashSet<ID<Block>>,
}

impl Block {
    /// Returns `true` if any of the instructions that will be added in the
    /// future will be unreachable (never executed)
    #[must_use]
    pub fn is_unreachable(&self) -> bool {
        self.instructions.last().map_or(false, |x| match x {
            Instruction::Jump(_) | Instruction::Return(_) => true,
            Instruction::Basic(_) => false,
        })
    }
}

/// Represents a complete flow of a program
///
/// This is the intermediate representation of the program that is used to
/// generate the final code.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Getters,
    CopyGetters,
    derive_more::Index,
    derive_more::IndexMut,
)]
pub struct ControlFlowGraph {
    /// Contains all the blocks in the control flow graph.
    #[get = "pub"]
    #[index]
    #[index_mut]
    blocks: Arena<Block>,

    /// The id of the entry block.
    #[get_copy = "pub"]
    entry_block_id: ID<Block>,
}

impl ControlFlowGraph {
    /// Gets the [`Block`] with the given ID.
    #[must_use]
    pub fn get_block(&self, id: ID<Block>) -> Option<&Block> {
        self.blocks.get(id)
    }

    /// Gets the mutable reference to the [`Block`] with the given ID.
    #[must_use]
    pub fn get_block_mut(&mut self, id: ID<Block>) -> Option<&mut Block> {
        self.blocks.get_mut(id)
    }
}
