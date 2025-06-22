//! Contains the definition of the [`Database`] struct.

use std::sync::atomic::AtomicBool;

use getset::CopyGetters;
use parking_lot::Mutex;
use pernixc_serialize::{Deserialize, Serialize};

use crate::{
    runtime::serde::{DynamicDeserialize, DynamicSerialize},
    Key,
};

pub mod call_graph;
pub mod map;

/// Represents the main database structure used for storing and managing
/// compiler state, including mappings, call graphs, and versioning information.
#[derive(Debug, Default, Serialize, Deserialize, CopyGetters)]
#[serde(
    ser_extension(DynamicSerialize<__S>),
    de_extension(DynamicDeserialize<__D>)
)]
pub struct Database {
    map: map::Map,
    call_graph: Mutex<call_graph::CallGraph>,

    /// The current version of the database, which is incremented whenever an
    /// update is made to the database.
    #[get_copy = "pub"]
    version: usize,
    last_was_query: AtomicBool,
}

impl Database {
    /// Checks if the database contains a key.
    pub fn contains_key<K: Key>(&self, key: &K) -> bool {
        self.map.contains_key(key)
    }
}
