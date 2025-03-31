//! Contains the definition of [`Block`] and [`ControlFlowGraph`].

use std::{
    collections::{HashMap, HashSet},
    ops::{Not, RangeBounds},
};

use getset::{CopyGetters, Getters};
use pernixc_arena::{Arena, Key, ID};
use pernixc_semantic::term::r#type::Type;
use pernixc_transitive_closure::TransitiveClosure;
use serde::{Deserialize, Serialize};

use super::instruction::{Instruction, Jump, Terminator};
use crate::model::Transform;

/// A data structure used for computing whether a particular block in the
/// control flow graph is reachable to another.
///
/// This data structure used an efficient algorithm to determine the
/// reachability in constant time.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Reachability<M: pernixc_term::Model> {
    blocks_to_index: HashMap<ID<Block<M>>, usize>,
    index_to_blocks: Vec<ID<Block<M>>>,

    transitive_closure: TransitiveClosure,
}

impl<M: pernixc_term::Model> Reachability<M> {
    /// Checks if there is a path from the `from` block to the `to` block.
    #[must_use]
    pub fn block_reachable(
        &self,
        from: ID<Block<M>>,
        to: ID<Block<M>>,
    ) -> Option<bool> {
        let from = self.blocks_to_index.get(&from)?;
        let to = self.blocks_to_index.get(&to)?;

        Some(self.transitive_closure.has_path(*from, *to).unwrap())
    }

    /// Checks if there is a path from the `from` point to the `to` point.
    ///
    /// The interpretation of reachable is that the exit of `from` point is
    /// reachable to the entry of `to` point; therefore, the same point is
    /// not reachable to each other unless the block is in the loop.
    #[must_use]
    pub fn point_reachable(
        &self,
        from: Point<M>,
        to: Point<M>,
    ) -> Option<bool> {
        if self.block_reachable(from.block_id, to.block_id)? {
            Some(true)
        } else {
            Some(
                from.block_id == to.block_id
                    && from.instruction_index < to.instruction_index,
            )
        }
    }
}

/// Represents a list of instructions executed in sequence.
#[derive(
    Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Getters, CopyGetters,
)]
pub struct Block<M: pernixc_term::Model> {
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

impl<M: pernixc_term::Model> Block<M> {
    /// Transforms the [`Block`] to another model using the given
    /// transformer.
    #[allow(clippy::missing_errors_doc)]
    pub fn transform_model<T: Transform<Type<M>>>(
        self,
        transformer: &mut T,
    ) -> Result<Block<T::Target>, T::Error> {
        Ok(Block {
            instructions: self
                .instructions
                .into_iter()
                .map(|x| x.transform_model(transformer))
                .collect::<Result<_, T::Error>>()?,
            terminator: self
                .terminator
                .map(|x| x.transform_model(transformer))
                .transpose()?,
            predecessors: self
                .predecessors
                .into_iter()
                .map(|x| ID::from_index(x.into_index()))
                .collect(),
            is_entry: self.is_entry,
        })
    }

    /// Splices the instructions in the block with the given range with the
    /// replacement instructions.
    pub fn splice(
        &mut self,
        range: impl RangeBounds<usize>,
        replacement: impl IntoIterator<Item = Instruction<M>>,
    ) {
        self.instructions.splice(range, replacement);
    }

    /// Inserts a multiple instructions to the block at the given index.
    ///
    /// The instructions are inserted in the order they are given.
    pub fn insert_instructions(
        &mut self,
        index: usize,
        instructions: impl IntoIterator<Item = Instruction<M>>,
    ) {
        self.instructions.splice(index..index, instructions);
    }

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
    pub fn add_instruction(&mut self, instruction: Instruction<M>) -> bool {
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
    Serialize,
    Deserialize,
    derive_more::Index,
    derive_more::IndexMut,
)]
pub struct ControlFlowGraph<M: pernixc_term::Model> {
    /// Contains all the blocks in the control flow graph.
    #[get = "pub"]
    #[index]
    #[index_mut]
    blocks: Arena<Block<M>>,

    /// The id of the entry block.
    #[get_copy = "pub"]
    entry_block_id: ID<Block<M>>,
}

/// An iterator for traversing through the control flow graph.
///
/// Every block is guaranteed to be visited exactly once and reachable from
/// the entry block.
///
/// The iterator is called in a depth-first and pre-order manner.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Traverser<'a, M: pernixc_term::Model> {
    graph: &'a ControlFlowGraph<M>,
    visited: HashSet<ID<Block<M>>>,
    stack: Vec<ID<Block<M>>>,
}

impl<'a, M: pernixc_term::Model> Iterator for Traverser<'a, M> {
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
                .filter_map(|x| x.as_jump())
                .flat_map(Jump::jump_targets),
        );

        Some((block_id, block))
    }
}

/// An iterator used for retrieving the removed unreachable blocks from the
/// control flow graph.
///
/// This iterator behaves similarly like `drain` in Rust's standard library.
#[derive(Debug, PartialEq, Eq)]
pub struct RemoveUnreachableBlocks<'a, M: pernixc_term::Model> {
    graph: &'a mut ControlFlowGraph<M>,
    unreachable_blocks: Vec<ID<Block<M>>>,
}

impl<M: pernixc_term::Model> Iterator for RemoveUnreachableBlocks<'_, M> {
    type Item = (ID<Block<M>>, Block<M>);

    fn next(&mut self) -> Option<Self::Item> {
        let block_id = self.unreachable_blocks.pop()?;
        let block = self.graph.blocks.remove(block_id);
        Some((block_id, block.unwrap()))
    }
}

impl<M: pernixc_term::Model> Drop for RemoveUnreachableBlocks<'_, M> {
    fn drop(&mut self) {
        for block in self.unreachable_blocks.iter().copied() {
            self.graph.blocks.remove(block).unwrap();
        }
    }
}

impl<M: pernixc_term::Model> ControlFlowGraph<M> {
    /// Transforms the [`ControlFlowGraph`] to another model using the given
    /// transformer.
    #[allow(clippy::missing_errors_doc)]
    pub fn transform_model<T: Transform<Type<M>>>(
        self,
        transformer: &mut T,
    ) -> Result<ControlFlowGraph<T::Target>, T::Error> {
        Ok(ControlFlowGraph {
            blocks: self
                .blocks
                .into_iter()
                .map(|(id, x)| {
                    Ok((
                        ID::from_index(id.into_index()),
                        x.transform_model(transformer)?,
                    ))
                })
                .collect::<Result<_, T::Error>>()?,
            entry_block_id: ID::from_index(self.entry_block_id.into_index()),
        })
    }

    /// Creates an iterator used for traversing through the control flow graph.
    ///
    /// See [`Traverser`] for more information.
    #[must_use]
    pub fn traverse(&self) -> Traverser<M> {
        Traverser {
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

    /// Creates a [`Reachability`], used for efficiently determining the
    /// reachability of particular blocks.
    #[must_use]
    pub fn reachability(&self) -> Reachability<M> {
        let mut block_ids_to_index = HashMap::new();
        let mut index_to_block_ids = Vec::new();

        for (index, id) in self.blocks().ids().enumerate() {
            block_ids_to_index.insert(id, index);
            index_to_block_ids.push(id);
        }

        let mut edges = Vec::new();

        // add edges to the transitive closure
        for (block_id, block) in self.blocks.iter() {
            let Some(Terminator::Jump(jump)) = &block.terminator else {
                continue;
            };

            match jump {
                Jump::Unconditional(jump) => {
                    edges.push((
                        block_ids_to_index[&block_id],
                        block_ids_to_index[&jump.target],
                    ));
                }
                Jump::Conditional(jump) => {
                    edges.push((
                        block_ids_to_index[&block_id],
                        block_ids_to_index[&jump.true_target],
                    ));
                    edges.push((
                        block_ids_to_index[&block_id],
                        block_ids_to_index[&jump.false_target],
                    ));
                }
                Jump::Switch(select) => {
                    for target in select.branches.values() {
                        edges.push((
                            block_ids_to_index[&block_id],
                            block_ids_to_index[target],
                        ));
                    }

                    if let Some(otherwise) = select.otherwise {
                        edges.push((
                            block_ids_to_index[&block_id],
                            block_ids_to_index[&otherwise],
                        ));
                    }
                }
            }
        }

        // compute transitive closure
        let closure = TransitiveClosure::new(
            edges.iter().map(|(f, t)| (*f, *t)),
            self.blocks.len(),
            false,
        )
        .expect("failed to create transitive closure");

        Reachability {
            blocks_to_index: block_ids_to_index,
            index_to_blocks: index_to_block_ids,
            transitive_closure: closure,
        }
    }

    /// From the given block and instruction index, checks if is it possible to
    /// reach an instruction that satisfies the given predicate.
    ///
    /// The predicate might be invoked multiple times for the same instruction.
    ///
    /// # Returns
    ///
    /// Returns `Some(true)` if the predicate is satisfied, `Some(false)` if
    /// the predicate is not satisfied, and `None` if the block ID or the
    /// instruction index is invalid.
    #[must_use]
    pub fn is_reachable(
        &self,
        from_block_id: ID<Block<M>>,
        from_instruction_index: usize,
        mut predicate: impl FnMut(&Instruction<M>) -> bool,
    ) -> Option<bool> {
        self.is_reachable_internal(
            from_block_id,
            from_instruction_index,
            true,
            &mut predicate,
            &mut HashSet::new(),
        )
    }

    fn is_reachable_internal(
        &self,
        from_block_id: ID<Block<M>>,
        from_instruction_index: usize,
        is_root: bool,
        predicate: &mut impl FnMut(&Instruction<M>) -> bool,
        visited: &mut HashSet<ID<Block<M>>>,
    ) -> Option<bool> {
        if !is_root && visited.insert(from_block_id) {
            return Some(false);
        }

        let block = self.blocks.get(from_block_id)?;

        // check index
        if from_instruction_index >= block.instructions.len() {
            return None;
        }

        for inst in self
            .blocks
            .get(from_block_id)?
            .instructions
            .iter()
            .skip(from_instruction_index)
        {
            if predicate(inst) {
                return Some(true);
            }
        }

        let Some(terminator) = &block.terminator else {
            return None;
        };

        match terminator {
            Terminator::Jump(Jump::Conditional(jump)) => {
                if self.is_reachable_internal(
                    jump.true_target,
                    0,
                    false,
                    predicate,
                    visited,
                )? || self.is_reachable_internal(
                    jump.false_target,
                    0,
                    false,
                    predicate,
                    visited,
                )? {
                    return Some(true);
                }

                Some(false)
            }
            Terminator::Jump(Jump::Unconditional(jump)) => self
                .is_reachable_internal(
                    jump.target,
                    0,
                    false,
                    predicate,
                    visited,
                ),

            Terminator::Jump(Jump::Switch(condition)) => {
                let blocks = condition
                    .branches
                    .values()
                    .copied()
                    .chain(condition.otherwise);

                for block in blocks {
                    if self.is_reachable_internal(
                        block, 0, false, predicate, visited,
                    )? {
                        return Some(true);
                    }
                }

                Some(false)
            }

            Terminator::Return(_) | Terminator::Panic => Some(false),
        }
    }

    /// Returns an iterator that iterates through all the blocks in the control
    /// flow graph.
    pub fn block_muts(
        &mut self,
    ) -> impl Iterator<Item = (ID<Block<M>>, &mut Block<M>)> {
        self.blocks
            .iter_mut()
            .map(|(id, x)| (ID::from_index(id.into_index()), x))
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

    /// Creates a new block with the given explicitly specified ID.
    ///
    /// # Returns
    ///
    /// Returns `true` if the block is successfully created.
    pub fn new_block_with_id(&mut self, id: ID<Block<M>>) -> bool {
        self.blocks
            .insert_with_id(id, Block {
                instructions: Vec::new(),
                predecessors: HashSet::new(),
                terminator: None,
                is_entry: false,
            })
            .is_ok()
    }

    /// Checks if the `to_block_id` is reachable from the `from_block_id`.
    ///
    /// # Returns
    ///
    /// Returns `None` if the `from_block_id` or `to_block_id` is invalid.
    #[must_use]
    pub fn is_reachable_from(
        &self,
        from_block_id: ID<Block<M>>,
        to_block_id: ID<Block<M>>,
    ) -> Option<bool> {
        self.is_reachable_from_internal(
            from_block_id,
            to_block_id,
            &mut HashSet::new(),
        )
    }

    fn is_reachable_from_internal(
        &self,
        from_block_id: ID<Block<M>>,
        to_block_id: ID<Block<M>>,
        visited: &mut HashSet<ID<Block<M>>>,
    ) -> Option<bool> {
        if from_block_id == to_block_id {
            return Some(true);
        }

        if !visited.insert(from_block_id) {
            return Some(false);
        }

        let block = self.blocks.get(from_block_id)?;
        let Some(Terminator::Jump(jump)) = &block.terminator else {
            return Some(false);
        };

        match jump {
            Jump::Unconditional(unconditional_jump) => self
                .is_reachable_from_internal(
                    unconditional_jump.target,
                    to_block_id,
                    visited,
                ),
            Jump::Conditional(conditional_jump) => Some(
                self.is_reachable_from_internal(
                    conditional_jump.true_target,
                    to_block_id,
                    visited,
                )? || self.is_reachable_from_internal(
                    conditional_jump.false_target,
                    to_block_id,
                    visited,
                )?,
            ),
            Jump::Switch(select_jump) => {
                let blocks = select_jump
                    .branches
                    .values()
                    .copied()
                    .chain(select_jump.otherwise);

                for block in blocks {
                    if self.is_reachable_from_internal(
                        block,
                        to_block_id,
                        visited,
                    )? {
                        return Some(true);
                    }
                }

                Some(false)
            }
        }
    }

    /// Inserts a new terminator instruction to the given block ID.
    ///
    /// # Errors
    ///
    /// If the block hasn't been terminated yet, then the `terminator` is
    /// inserted and `true` is returned.
    pub fn insert_terminator(
        &mut self,
        block_id: ID<Block<M>>,
        terminator: Terminator<M>,
    ) -> bool {
        if self[block_id].is_unreachable_or_terminated() {
            return false;
        }

        match terminator {
            Terminator::Jump(Jump::Unconditional(jump)) => {
                self[block_id].terminator =
                    Some(Terminator::Jump(Jump::Unconditional(jump)));

                // add the predecessor for the jump target
                self[jump.target].predecessors.insert(block_id);
            }

            Terminator::Jump(Jump::Conditional(jump)) => {
                // add the predecessors for the jump targets
                self[jump.true_target].predecessors.insert(block_id);
                self[jump.false_target].predecessors.insert(block_id);

                self.get_block_mut(block_id).unwrap().terminator =
                    Some(Terminator::Jump(Jump::Conditional(jump)));
            }

            Terminator::Jump(Jump::Switch(select)) => {
                for target in
                    select.branches.values().copied().chain(select.otherwise)
                {
                    self[target].predecessors.insert(block_id);
                }

                self[block_id].terminator =
                    Some(Terminator::Jump(Jump::Switch(select)));
            }

            Terminator::Return(ret) => {
                self[block_id].terminator = Some(Terminator::Return(ret));
            }

            Terminator::Panic => {
                self[block_id].terminator = Some(Terminator::Panic);
            }
        }

        true
    }
}

impl<M: pernixc_term::Model> Default for ControlFlowGraph<M> {
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

/// Represents an identifier to a particular instruction in the control flow
/// graph.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Point<M: pernixc_term::Model> {
    /// The index of the instruction in the block.
    pub instruction_index: usize,

    /// The ID of the block that contains the instruction.
    pub block_id: ID<Block<M>>,
}
