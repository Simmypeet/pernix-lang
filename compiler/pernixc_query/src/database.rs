//! Contains the definition of the [`Database`] struct.

use std::sync::atomic::AtomicBool;

use getset::CopyGetters;
use parking_lot::Mutex;

pub mod call_graph;
pub mod map;

/// Represents the main database structure used for storing and managing
/// compiler state, including mappings, call graphs, and versioning information.
#[derive(Debug, Default, CopyGetters)]
pub struct Database {
    map: map::Map,
    call_graph: Mutex<call_graph::CallGraph>,
    /// The current version of the database, which is incremented whenever an
    /// update is made to the database.
    #[get_copy = "pub"]
    version: usize,
    last_was_query: AtomicBool,
}
