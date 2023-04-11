use std::{
    collections::HashMap,
    hash::Hash,
    ops::{Index, IndexMut},
};

use getset::{CopyGetters, Getters};

use crate::{
    control_flow_graph::BasicBlockID,
    hir::value::{IntermediateType, VariableID},
};

/// Is a data structure that emulates the stack of blocks and loops scopes.
#[derive(Debug, Clone)]
pub(super) struct LabelManager<'a, K, V> {
    key_values: HashMap<K, V>,
    label_stack: Vec<(Option<&'a str>, K)>,
}

impl<K: Hash + Eq, V> Index<K> for LabelManager<'_, K, V> {
    type Output = V;

    fn index(&self, index: K) -> &Self::Output { &self.key_values[&index] }
}

impl<K: Hash + Eq, V> IndexMut<K> for LabelManager<'_, K, V> {
    fn index_mut(&mut self, index: K) -> &mut Self::Output {
        self.key_values.get_mut(&index).unwrap()
    }
}

impl<'a, K: Hash + Eq + Identifier + Clone, V> LabelManager<'a, K, V> {
    /// Creates a new container with no values.
    pub(super) fn new() -> Self {
        Self {
            key_values: HashMap::new(),
            label_stack: Vec::new(),
        }
    }

    /// Pushes a new value in the stack of scopes and returns the key associated with the value.
    pub(super) fn push(&mut self, value: V, label: Option<&'a str>) -> K {
        let key = K::fresh();
        self.key_values.insert(key.clone(), value);
        self.label_stack.push((label, key.clone()));
        key
    }

    /// Pops the last value from the stack of scopes.
    pub(super) fn pop(&mut self) { self.label_stack.pop(); }

    /// Gets the last pused value's key.
    pub(super) fn get(&self) -> Option<K> { self.label_stack.last().map(|(_, key)| key.clone()) }

    /// Gets the last pushed value's key with the given name.
    ///
    /// The search is done in reverse order.
    pub(super) fn get_with_name(&self, name: &str) -> Option<K> {
        self.label_stack
            .iter()
            .rev()
            .find(|(label, _)| label.map(|label| label == name).unwrap_or(false))
            .map(|(_, key)| key.clone())
    }
}

/// Is a trait that allows to generate fresh identifiers.
pub(super) trait Identifier {
    fn fresh() -> Self;
}

/// Gets the information of a block.
#[derive(Debug, Clone, Getters, CopyGetters)]
pub(super) struct BlockInfo {
    /// Is the basic block that is executed after the current block.
    #[get_copy = "pub(super)"]
    successor_basic_block_id: BasicBlockID,

    /// Is the type of the block. It is `None` if the block doesn't express any value.
    #[get_copy = "pub(super)"]
    ty: Option<IntermediateType>,

    /// The temporary variable that stores the value of the block.
    #[get_copy = "pub(super)"]
    variable_id: VariableID,
}

impl BlockInfo {
    pub(super) fn new(successor_basic_block_id: BasicBlockID, variable_id: VariableID) -> Self {
        Self {
            successor_basic_block_id,
            ty: None,
            variable_id,
        }
    }

    pub(super) fn set_ty(&mut self, ty: IntermediateType) { self.ty = Some(ty); }
}

/// Is the unique identifier used to identify a block in the [`BlockManager`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) struct BlockID(usize);

impl Identifier for BlockID {
    fn fresh() -> Self {
        // use atomic counter for the identifier
        static COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

        Self(COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }
}

#[derive(Debug, Clone, Getters, CopyGetters)]
pub(super) struct LoopInfo {
    /// Is the 
    #[get_copy = "pub(super)"]
    header_block_id: BlockID,

    /// Is the basic block that is executed after the current block.
    #[get_copy = "pub(super)"]
    successor_basic_block_id: BasicBlockID,

    /// Is the type of the block. It is `None` if the block doesn't express any value.
    #[get_copy = "pub(super)"]
    ty: Option<IntermediateType>,

    /// The temporary variable that stores the value of the block.
    #[get_copy = "pub(super)"]
    variable_id: VariableID,
}

pub(super) type BlockManager<'a> = LabelManager<'a, BlockID, BlockInfo>;
