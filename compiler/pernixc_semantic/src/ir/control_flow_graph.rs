//! Contains the definition of [`Block`] and [`ControlFlowGraph`].

use std::collections::HashSet;

use getset::{CopyGetters, Getters};

use super::instruction::{self, Instruction};
use crate::{
    arena::{Arena, ID},
    semantic::model::Model,
};

/// Represents a scope hierarchy that is used to determine the
/// visibility/lifetime of each variable.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
    /// The ID to the parent scope. If this is `None`, then this is the
    /// top-most scope.
    pub parent: Option<ID<Scope>>,

    /// The depth of the scope. The top-most scope has a depth of 0.
    pub scope_depth: usize,

    /// List of child scopes that are under this scope.
    pub child_scopes: HashSet<ID<Scope>>,
}

/// Represents a list of instructions executed in sequence.
#[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
pub struct Block<M: Model> {
    /// List of instructions that are executed in sequence.
    #[get = "pub"]
    instructions: Vec<Instruction<M>>,
    /// List of instructions that will never be executed.
    #[get = "pub"]
    unreachables: Vec<Instruction<M>>,
    /// List of blocks that are successors of this block.
    #[get = "pub"]
    successors: HashSet<ID<Block<M>>>,
    /// List of blocks that are predecessors of this block.
    #[get = "pub"]
    predecessors: HashSet<ID<Block<M>>>,
    /// The scope in which this block is in.
    #[get_copy = "pub"]
    in_scope_id: ID<Scope>,
}

impl<M: Model> Block<M> {
    /// Returns `true` if any of the instructions that will be added in the
    /// future will be unreachable (never executed)
    #[must_use]
    pub fn is_unreachable(&self) -> bool {
        self.instructions.last().map_or(false, |x| match x {
            Instruction::Jump(_) | Instruction::Return(_) => true,
            Instruction::Basic(_) => false,
        })
    }

    /// Adds a basic instruction to the block.
    pub fn insert_basic(&mut self, instruction: instruction::Basic<M>) {
        if self.is_unreachable() {
            self.unreachables.push(Instruction::Basic(instruction));
        } else {
            self.instructions.push(Instruction::Basic(instruction));
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

    /// List of all the scopes in the control flow graph.
    #[get = "pub"]
    scopes: Arena<Scope>,

    /// The id of the top-most scope.
    #[get_copy = "pub"]
    starting_scope_id: ID<Scope>,
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
        let mut scopes = Arena::new();
        let starting_scope_id = scopes.insert(Scope {
            parent: None,
            scope_depth: 0,
            child_scopes: HashSet::new(),
        });

        let mut blocks = Arena::new();

        // create an entry block
        let entry_block_id = blocks.insert(Block {
            instructions: Vec::new(),
            unreachables: Vec::new(),
            successors: HashSet::new(),
            predecessors: HashSet::new(),
            in_scope_id: starting_scope_id,
        });

        Self { blocks, entry_block_id, scopes, starting_scope_id }
    }
}
