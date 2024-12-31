//! Contains the definition of [`Input`].
use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::symbol::{component, Global, ItemID};

/// Contains the input components information required for building the full
/// table.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Input {
    symbol_names: HashMap<Global<ItemID>, String>,

    /// The syntax tree map won't be serialized
    #[serde(skip)]
    pub(in crate::symbol) syntax_tree_map: component::syntax_tree::Map,
    pub(in crate::symbol) accessibility_map: component::accessibility::Map,
    pub(in crate::symbol) parent_map: component::parent::Map,
    pub(in crate::symbol) member_map: component::member::Map,
}
