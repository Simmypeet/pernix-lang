//! Implements a query system powering the incremental compilation and semantic
//! analysis

use std::sync::atomic::AtomicBool;

use getset::{CopyGetters, Getters};
pub use key::Key;
use map::Map;
use parking_lot::Mutex;
pub use pernixc_query_derive::Key;

pub mod call_graph;
pub mod executor;
pub mod key;
mod map;
mod reflector;

/// The central data structure for the Pernix compiler storing all the semantic
/// information about the program.
#[derive(Debug, Default, Getters, CopyGetters)]
pub struct Database {
    call_graph: Mutex<call_graph::CallGraph>,

    /// The registry of executors allowing registering and retrieving executors
    /// for different query key types.
    executor_registry: executor::Registry,

    map: Map,

    /// Gets the current version counter of the database.
    #[get_copy = "pub"]
    version: usize,

    last_was_query: AtomicBool,
}

#[cfg(test)]
mod test;
