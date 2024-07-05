//! Contains the definition of [`Block`] and [`ControlFlowGraph`].

use std::collections::HashSet;

use getset::{CopyGetters, Getters};

use super::instruction::{Instruction, Terminator};
use crate::{
    arena::{Arena, ID},
    type_system::model::Model,
};

/// Represents a list of instructions executed in sequence.
#[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
pub struct Block<M: Model> {
    /// List of instructions that are executed in sequence.
    #[get = "pub"]
    instructions: Vec<Instruction<M>>,
    /// List of instructions that will never be executed.
    #[get = "pub"]
    unreachables: Vec<Instruction<M>>,
    /// The terminator instruction that will be executed last.
    #[get = "pub"]
    terminator: Option<Terminator<M>>,
    /// List of blocks that are successors of this block.
    #[get = "pub"]
    successors: HashSet<ID<Block<M>>>,
    /// List of blocks that are predecessors of this block.
    #[get = "pub"]
    predecessors: HashSet<ID<Block<M>>>,
}

impl<M: Model> Block<M> {
    /// Returns `true` if any of the instructions that will be added in the
    /// future will be unreachable (never executed)
    #[must_use]
    pub const fn is_unreachable(&self) -> bool { self.terminator.is_some() }

    /// Adds a basic instruction to the block.
    pub fn insert_basic(&mut self, instruction: Instruction<M>) {
        if self.is_unreachable() {
            self.unreachables.push(instruction);
        } else {
            self.instructions.push(instruction);
        }
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
pub struct ControlFlowGraph<M: Model> {
    /// Contains all the blocks in the control flow graph.
    #[get = "pub"]
    #[index]
    #[index_mut]
    blocks: Arena<Block<M>>,

    /// The id of the entry block.
    #[get_copy = "pub"]
    entry_block_id: ID<Block<M>>,
}

impl<M: Model> ControlFlowGraph<M> {
    /// Gets the [`Block`] with the given ID.
    #[must_use]
    pub fn get_block(&self, id: ID<Block<M>>) -> Option<&Block<M>> {
        self.blocks.get(id)
    }

    /// Gets the mutable reference to the [`Block`] with the given ID.
    #[must_use]
    pub fn get_block_mut(&mut self, id: ID<Block<M>>) -> Option<&mut Block<M>> {
        self.blocks.get_mut(id)
    }
}

impl<M: Model> Default for ControlFlowGraph<M> {
    fn default() -> Self {
        let mut blocks = Arena::new();

        // create an entry block
        let entry_block_id = blocks.insert(Block {
            instructions: Vec::new(),
            unreachables: Vec::new(),
            successors: HashSet::new(),
            predecessors: HashSet::new(),
            terminator: None,
        });

        Self { blocks, entry_block_id }
    }
}
