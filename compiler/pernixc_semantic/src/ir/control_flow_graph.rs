//! Contains the definition of [`Block`] and [`ControlFlowGraph`].

use std::{collections::HashSet, ops::Not};

use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};

use super::instruction::{Instruction, Jump, Terminator};
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
    /// The terminator instruction that will be executed last.
    #[get = "pub"]
    terminator: Option<Terminator<M>>,
    /// List of blocks that are predecessors of this block.
    #[get = "pub"]
    predecessors: HashSet<ID<Block<M>>>,
    /// Determines if the block is the entry block.
    #[get_copy = "pub"]
    is_entry: bool,
}

impl<M: Model> Block<M> {
    /// Returns `true` if any of the instructions that will be added in the
    /// future will be unreachable (never executed)
    #[must_use]
    pub fn is_unreachable_or_terminated(&self) -> bool {
        self.terminator.is_some()
            || (!self.is_entry && self.predecessors.is_empty())
    }

    /// Returns `true` if the block is reachable from the entry block.
    pub fn is_reachable(&self) -> bool {
        !self.predecessors.is_empty() || self.is_entry
    }

    /// Adds a basic instruction to the block.
    #[must_use]
    pub fn insert_instruction(&mut self, instruction: Instruction<M>) -> bool {
        if self.is_unreachable_or_terminated() {
            false
        } else {
            self.instructions.push(instruction);
            true
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

/// An error from calling [`ControlFlowGraph::insert_terminator`].
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    thiserror::Error,
    EnumAsInner,
)]
#[allow(missing_docs)]
pub enum InsertTerminatorError<M: Model> {
    #[error("the block is unreachable or already terminated")]
    UnreachableOrTerminated,

    #[error(
        "found an invalid block ID either from the terminator instruction or \
         the target block ID parameter"
    )]
    InvalidBlockID(ID<Block<M>>),
}

/// An iterator for traversing through the control flow graph.
///
/// Every block is guaranteed to be visited exactly once and reachable from
/// the entry block.
///
/// The iterator is called in a depth-first and pre-order manner.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ControlFlowGraphTraverser<'a, M: Model> {
    graph: &'a ControlFlowGraph<M>,
    visited: HashSet<ID<Block<M>>>,
    stack: Vec<ID<Block<M>>>,
}

impl<'a, M: Model> Iterator for ControlFlowGraphTraverser<'a, M> {
    type Item = (ID<Block<M>>, &'a Block<M>);

    fn next(&mut self) -> Option<Self::Item> {
        let block_id = loop {
            let block_id = self.stack.pop()?;
            if self.visited.insert(block_id) {
                break block_id;
            }
        };

        let block = self.graph.get_block(block_id).unwrap();

        self.stack.extend(
            block
                .terminator()
                .iter()
                .flat_map(|x| x.as_jump())
                .flat_map(|x| x.jump_targets()),
        );

        Some((block_id, block))
    }
}

/// An iterator used for retrieving the removed unreachable blocks from the
/// control flow graph.
///
/// This iterator behaves similarly like `drain` in Rust's standard library.
#[derive(Debug, PartialEq, Eq)]
pub struct RemoveUnreachableBlocks<'a, M: Model> {
    graph: &'a mut ControlFlowGraph<M>,
    unreachable_blocks: Vec<ID<Block<M>>>,
}

impl<M: Model> Iterator for RemoveUnreachableBlocks<'_, M> {
    type Item = (ID<Block<M>>, Block<M>);

    fn next(&mut self) -> Option<Self::Item> {
        let block_id = self.unreachable_blocks.pop()?;
        let block = self.graph.blocks.remove(block_id);
        Some((block_id, block.unwrap()))
    }
}

impl<M: Model> Drop for RemoveUnreachableBlocks<'_, M> {
    fn drop(&mut self) {
        for block in self.unreachable_blocks.iter().copied() {
            self.graph.blocks.remove(block).unwrap();
        }
    }
}

impl<M: Model> ControlFlowGraph<M> {
    /// Creates an iterator used for traversing through the control flow graph.
    ///
    /// See [`ControlFlowGraphTraverser`] for more information.
    pub fn traverse(&self) -> ControlFlowGraphTraverser<M> {
        ControlFlowGraphTraverser {
            graph: self,
            visited: HashSet::new(),
            stack: vec![self.entry_block_id],
        }
    }

    /// Removes all the blocks in the control flow graphs that are unreachable
    /// from the entry block.
    pub fn remove_unerachable_blocks(&mut self) -> RemoveUnreachableBlocks<M> {
        RemoveUnreachableBlocks {
            unreachable_blocks: self
                .blocks()
                .iter()
                .filter_map(|(id, x)| x.is_reachable().not().then_some(id))
                .collect(),
            graph: self,
        }
    }

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

    /// Creates a new block and returns its ID.
    ///
    /// When the block is created, it is not connected to any other block.
    /// Therefore, the block is unreachable and any instructions added to it
    /// will not be executed.
    #[must_use]
    pub fn new_block(&mut self) -> ID<Block<M>> {
        self.blocks.insert(Block {
            instructions: Vec::new(),
            predecessors: HashSet::new(),
            terminator: None,
            is_entry: false,
        })
    }

    /// Inserts a new terminator instruction to the given block ID.
    ///
    /// # Errors
    ///
    /// See [`InsertTerminatorError`] for more information.
    pub fn insert_terminator(
        &mut self,
        block_id: ID<Block<M>>,
        terminator: Terminator<M>,
    ) -> Result<(), InsertTerminatorError<M>> {
        if self
            .get_block(block_id)
            .ok_or(InsertTerminatorError::InvalidBlockID(block_id))?
            .is_unreachable_or_terminated()
        {
            return Err(InsertTerminatorError::UnreachableOrTerminated);
        }

        match terminator {
            Terminator::Jump(Jump::Unconditional(jump)) => {
                let _ = self.get_block(jump.target).ok_or(
                    InsertTerminatorError::InvalidBlockID(jump.target),
                )?;

                let this_block = self
                    .get_block_mut(block_id)
                    .ok_or(InsertTerminatorError::InvalidBlockID(block_id))?;

                this_block.terminator =
                    Some(Terminator::Jump(Jump::Unconditional(jump)));

                // add the predecessor for the jump target
                self.get_block_mut(jump.target)
                    .unwrap()
                    .predecessors
                    .insert(block_id);
            }

            Terminator::Jump(Jump::Conditional(jump)) => {
                let _ = self.get_block(jump.true_target).ok_or(
                    InsertTerminatorError::InvalidBlockID(jump.true_target),
                )?;
                let _ = self.get_block(jump.false_target).ok_or(
                    InsertTerminatorError::InvalidBlockID(jump.false_target),
                )?;

                // add the predecessors for the jump targets
                self.get_block_mut(jump.true_target)
                    .unwrap()
                    .predecessors
                    .insert(block_id);
                self.get_block_mut(jump.false_target)
                    .unwrap()
                    .predecessors
                    .insert(block_id);

                self.get_block_mut(block_id).unwrap().terminator =
                    Some(Terminator::Jump(Jump::Conditional(jump)));
            }

            Terminator::Jump(Jump::Select(select)) => {
                for target in
                    select.branches.values().copied().chain(select.otherwise)
                {
                    let _ = self
                        .get_block(target)
                        .ok_or(InsertTerminatorError::InvalidBlockID(target))?;
                }

                for target in
                    select.branches.values().copied().chain(select.otherwise)
                {
                    self.get_block_mut(target)
                        .unwrap()
                        .predecessors
                        .insert(block_id);
                }

                self.get_block_mut(block_id).unwrap().terminator =
                    Some(Terminator::Jump(Jump::Select(select)));
            }

            Terminator::Return(ret) => {
                self.get_block_mut(block_id).unwrap().terminator =
                    Some(Terminator::Return(ret));
            }

            Terminator::Panic => {
                self.get_block_mut(block_id).unwrap().terminator =
                    Some(Terminator::Panic);
            }
        }

        Ok(())
    }
}

impl<M: Model> Default for ControlFlowGraph<M> {
    fn default() -> Self {
        let mut blocks = Arena::new();

        // create an entry block
        let entry_block_id = blocks.insert(Block {
            instructions: Vec::new(),
            predecessors: HashSet::new(),
            terminator: None,
            is_entry: true,
        });

        Self { blocks, entry_block_id }
    }
}
