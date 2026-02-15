//! Contains the definition of [`Block`] and [`ControlFlowGraph`].

use std::ops::{Not, RangeBounds};

use getset::{CopyGetters, Getters};
use pernixc_arena::{Arena, ID};
use pernixc_hash::{HashMap, HashSet};
use pernixc_term::{constant::Constant, lifetime::Lifetime, r#type::Type};
use pernixc_transitive_closure::TransitiveClosure;
use qbice::{Decode, Encode, StableHash};

use super::instruction::{Instruction, Jump, Terminator};
use crate::transform::{self, Transformer};

/// A data structure used for computing whether a particular block in the
/// control flow graph is reachable to another.
///
/// This data structure used an efficient algorithm to determine the
/// reachability in constant time.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Reachability {
    blocks_to_index: HashMap<ID<Block>, usize>,
    index_to_blocks: Vec<ID<Block>>,

    transitive_closure: TransitiveClosure,
}

impl Reachability {
    /// Checks if there is a path from the `from` block to the `to` block.
    #[must_use]
    pub fn block_reachable(
        &self,
        from: ID<Block>,
        to: ID<Block>,
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
    pub fn point_reachable(&self, from: Point, to: Point) -> Option<bool> {
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
    Debug,
    Clone,
    PartialEq,
    Eq,
    Encode,
    Decode,
    Getters,
    StableHash,
    CopyGetters,
)]
pub struct Block {
    /// List of instructions that are executed in sequence.
    #[get = "pub"]
    instructions: Vec<Instruction>,
    /// The terminator instruction that will be executed last.
    terminator: Option<Terminator>,
    /// List of blocks that are predecessors of this block.
    #[get = "pub"]
    predecessors: HashSet<ID<Self>>,
    /// Determines if the block is the entry block.
    #[get_copy = "pub"]
    is_entry: bool,
}

impl Block {
    /// Splices the instructions in the block with the given range with the
    /// replacement instructions.
    pub fn splice(
        &mut self,
        range: impl RangeBounds<usize>,
        replacement: impl IntoIterator<Item = Instruction>,
    ) {
        self.instructions.splice(range, replacement);
    }

    /// Gets the terminator instruction of the block, if it has one.
    #[must_use]
    pub const fn terminator(&self) -> Option<&Terminator> {
        self.terminator.as_ref()
    }

    /// Inserts a multiple instructions to the block at the given index.
    ///
    /// The instructions are inserted in the order they are given.
    pub fn insert_instructions(
        &mut self,
        index: usize,
        instructions: impl IntoIterator<Item = Instruction>,
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
    #[must_use]
    pub fn is_reachable(&self) -> bool {
        !self.predecessors.is_empty() || self.is_entry
    }

    /// Adds a basic instruction to the block.
    #[must_use]
    pub fn add_instruction(&mut self, instruction: Instruction) -> bool {
        if self.is_unreachable_or_terminated() {
            false
        } else {
            self.instructions.push(instruction);
            true
        }
    }
}

impl transform::Element for Block {
    async fn transform<
        T: Transformer<Lifetime> + Transformer<Type> + Transformer<Constant>,
    >(
        &mut self,
        transformer: &mut T,
        engine: &pernixc_qbice::TrackedEngine,
    ) {
        for inst in &mut self.instructions {
            inst.transform(transformer, engine).await;
        }

        if let Some(terminator) = &mut self.terminator {
            terminator.transform(transformer, engine).await;
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
    Encode,
    Decode,
    StableHash,
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

/// An iterator for traversing through the control flow graph.
///
/// Every block is guaranteed to be visited exactly once and reachable from
/// the entry block.
///
/// The iterator is called in a depth-first and pre-order manner.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Traverser<'a> {
    graph: &'a ControlFlowGraph,
    visited: HashSet<ID<Block>>,
    stack: Vec<ID<Block>>,
}

impl<'a> Iterator for Traverser<'a> {
    type Item = (ID<Block>, &'a Block);

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

/// Iterator over mutable instructions in a control flow graph.
///
/// This iterator traverses the control flow graph in a depth-first manner,
/// yielding mutable references to each instruction in the graph.
///
/// Each instruction is guaranteed to be visited exactly once and reachable
/// from the entry block.
#[derive(Debug)]
pub struct MutInstructionTraverser<'a> {
    graph: &'a mut ControlFlowGraph,
    visited: HashSet<ID<Block>>,
    stack: Vec<ID<Block>>,

    current_block: ID<Block>,
    current_instruction_index: usize,
}

impl<'x> Iterator for MutInstructionTraverser<'x> {
    type Item = &'x mut Instruction;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let block = &mut self.graph.blocks[self.current_block];
            let inst =
                block.instructions.get_mut(self.current_instruction_index);

            if let Some(inst) = inst {
                self.current_instruction_index += 1;

                return unsafe {
                    Some(std::mem::transmute::<
                        &mut Instruction,
                        &'x mut Instruction,
                    >(inst))
                };
            }

            let block_id = loop {
                let block_id = self.stack.pop()?;
                if self.visited.insert(block_id) {
                    break block_id;
                }
            };

            // expand the stack
            self.stack.extend(
                self.graph.blocks[block_id]
                    .terminator()
                    .iter()
                    .filter_map(|x| x.as_jump())
                    .flat_map(Jump::jump_targets),
            );

            self.current_block = block_id;
            self.current_instruction_index = 0;

            // loop and try get instruction again
        }
    }
}

/// An iterator used for retrieving the removed unreachable blocks from the
/// control flow graph.
///
/// This iterator behaves similarly like `drain` in Rust's standard library.
#[derive(Debug, PartialEq, Eq)]
pub struct RemoveUnreachableBlocks<'a> {
    graph: &'a mut ControlFlowGraph,
    unreachable_blocks: Vec<ID<Block>>,
}

impl Iterator for RemoveUnreachableBlocks<'_> {
    type Item = (ID<Block>, Block);

    fn next(&mut self) -> Option<Self::Item> {
        let block_id = self.unreachable_blocks.pop()?;
        let block = self.graph.blocks.remove(block_id);
        Some((block_id, block.unwrap()))
    }
}

impl Drop for RemoveUnreachableBlocks<'_> {
    fn drop(&mut self) {
        for block in self.unreachable_blocks.iter().copied() {
            self.graph.blocks.remove(block).unwrap();
        }
    }
}

impl ControlFlowGraph {
    /// Creates an iterator used for traversing through the control flow graph.
    ///
    /// See [`Traverser`] for more information.
    #[must_use]
    pub fn traverse(&self) -> Traverser<'_> {
        Traverser {
            graph: self,
            visited: HashSet::default(),
            stack: vec![self.entry_block_id],
        }
    }

    /// Creates an iterator used for traversing through every instruction in
    /// the control flow graph mutably.
    ///
    /// See [`MutInstructionTraverser`] for more information.
    pub fn traverse_mut_instructions(&mut self) -> MutInstructionTraverser<'_> {
        MutInstructionTraverser {
            visited: HashSet::default(),
            stack: self.blocks[self.entry_block_id]
                .terminator()
                .and_then(|x| x.as_jump())
                .into_iter()
                .flat_map(Jump::jump_targets)
                .collect(),
            current_block: self.entry_block_id,
            current_instruction_index: 0,
            graph: self,
        }
    }

    /// Removes all the blocks in the control flow graphs that are unreachable
    /// from the entry block.
    pub fn remove_unreachable_blocks(&mut self) -> RemoveUnreachableBlocks<'_> {
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
    pub fn reachability(&self) -> Reachability {
        let mut block_ids_to_index = HashMap::default();
        let mut index_to_block_ids = Vec::new();

        for (index, id) in self.blocks.ids().enumerate() {
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
        from_block_id: ID<Block>,
        from_instruction_index: usize,
        mut predicate: impl FnMut(&Instruction) -> bool,
    ) -> Option<bool> {
        self.is_reachable_internal(
            from_block_id,
            from_instruction_index,
            true,
            &mut predicate,
            &mut HashSet::default(),
        )
    }

    fn is_reachable_internal(
        &self,
        from_block_id: ID<Block>,
        from_instruction_index: usize,
        is_root: bool,
        predicate: &mut impl FnMut(&Instruction) -> bool,
        visited: &mut HashSet<ID<Block>>,
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
    ) -> impl Iterator<Item = (ID<Block>, &mut Block)> {
        self.blocks.iter_mut()
    }

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

    /// Creates a new block and returns its ID.
    ///
    /// When the block is created, it is not connected to any other block.
    /// Therefore, the block is unreachable and any instructions added to it
    /// will not be executed.
    #[must_use]
    pub fn new_block(&mut self) -> ID<Block> {
        self.blocks.insert(Block {
            instructions: Vec::new(),
            predecessors: HashSet::default(),
            terminator: None,
            is_entry: false,
        })
    }

    /// Creates a new block with the given explicitly specified ID.
    ///
    /// # Returns
    ///
    /// Returns `true` if the block is successfully created.
    pub fn new_block_with_id(&mut self, id: ID<Block>) -> bool {
        self.blocks
            .insert_with_id(id, Block {
                instructions: Vec::new(),
                predecessors: HashSet::default(),
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
        from_block_id: ID<Block>,
        to_block_id: ID<Block>,
    ) -> Option<bool> {
        self.is_reachable_from_internal(
            from_block_id,
            to_block_id,
            &mut HashSet::default(),
        )
    }

    fn is_reachable_from_internal(
        &self,
        from_block_id: ID<Block>,
        to_block_id: ID<Block>,
        visited: &mut HashSet<ID<Block>>,
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
        block_id: ID<Block>,
        terminator: Terminator,
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

impl transform::Element for ControlFlowGraph {
    async fn transform<
        T: Transformer<Lifetime> + Transformer<Type> + Transformer<Constant>,
    >(
        &mut self,
        transformer: &mut T,
        engine: &pernixc_qbice::TrackedEngine,
    ) {
        for block in self.blocks.iter_mut().map(|(_, x)| x) {
            block.transform(transformer, engine).await;
        }
    }
}

impl Default for ControlFlowGraph {
    fn default() -> Self {
        let mut blocks = Arena::new();

        // create an entry block
        let entry_block_id = blocks.insert(Block {
            instructions: Vec::new(),
            predecessors: HashSet::default(),
            terminator: None,
            is_entry: true,
        });

        Self { blocks, entry_block_id }
    }
}

/// Represents an identifier to a particular instruction in the control flow
/// graph.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Point {
    /// The index of the instruction in the block.
    pub instruction_index: usize,

    /// The ID of the block that contains the instruction.
    pub block_id: ID<Block>,
}
