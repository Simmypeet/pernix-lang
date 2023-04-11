use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
    sync::atomic::{AtomicUsize, Ordering},
};

use derive_more::{Deref, DerefMut};
use getset::{CopyGetters, Getters};

use crate::{
    control_flow_graph::BasicBlockID,
    hir::value::{IntermediateType, VariableID},
};

/// Is the unique identifier used to identify a block in the [`BlockManager`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) struct BlockID(usize);

impl BlockID {
    pub(super) fn fresh() -> Self {
        // use atomic counter to generate unique id
        static COUNTER: AtomicUsize = AtomicUsize::new(0);

        Self(COUNTER.fetch_add(1, Ordering::Relaxed))
    }
}

#[derive(Debug, Clone, Deref, DerefMut)]
pub(super) struct BlockInfoWithID {
    block_id: BlockID,
    #[deref]
    #[deref_mut]
    block_info: BlockInfo,
}

/// Gets the information of a block.
#[derive(Debug, Clone, Getters, CopyGetters)]
pub(super) struct BlockInfo {
    #[get_copy = "pub(super)"]
    basic_block_id: BasicBlockID,

    #[get_copy = "pub(super)"]
    successor_basic_block_id: BasicBlockID,

    #[get_copy = "pub(super)"]
    ty: Option<IntermediateType>,

    #[get_copy = "pub(super)"]
    variable_id: VariableID,
}

impl BlockInfo {
    pub(super) fn new(
        basic_block_id: BasicBlockID,
        successor_basic_block_id: BasicBlockID,
        variable_id: VariableID,
    ) -> Self {
        Self {
            basic_block_id,
            successor_basic_block_id,
            ty: None,
            variable_id,
        }
    }

    pub(super) fn set_ty(&mut self, ty: IntermediateType) { self.ty = Some(ty); }
}

#[derive(Debug, Clone)]
pub(super) struct BlockManager<'a> {
    block_infos: HashMap<BlockID, BlockInfoWithID>,
    block_scope: Vec<(Option<&'a str>, BlockID)>,
}

impl Index<BlockID> for BlockManager<'_> {
    type Output = BlockInfoWithID;

    fn index(&self, index: BlockID) -> &Self::Output { &self.block_infos[&index] }
}

impl IndexMut<BlockID> for BlockManager<'_> {
    fn index_mut(&mut self, index: BlockID) -> &mut Self::Output {
        self.block_infos.get_mut(&index).unwrap()
    }
}

impl<'a> BlockManager<'a> {
    /// Creates a new [`BlockManager`] with no blocks.
    pub(super) fn new() -> Self {
        Self {
            block_infos: HashMap::new(),
            block_scope: Vec::new(),
        }
    }

    /// Creates a new block with the given [`BlockInfo`] and returns its [`BlockID`].
    pub(super) fn new_scope(&mut self, block_info: BlockInfo, label: Option<&'a str>) -> BlockID {
        let block_id = BlockID::fresh();
        self.block_infos.insert(block_id, BlockInfoWithID {
            block_id,
            block_info,
        });
        self.block_scope.push((label, block_id));
        block_id
    }

    /// Pops the last block scope.
    pub(super) fn pop_scope(&mut self) { self.block_scope.pop(); }

    /// Gets the top-most block scope.
    pub(super) fn get_last_scope(&self) -> Option<BlockID> {
        self.block_scope.last().map(|(_, block_id)| *block_id)
    }

    /// Searches for a block scope with the given label.
    pub(super) fn search_scope(&self, label: &'a str) -> Option<BlockID> {
        self.block_scope
            .iter()
            .rev()
            .find(|(block_label, _)| block_label.map_or(false, |block_label| block_label == label))
            .map(|(_, block_id)| *block_id)
    }
}
