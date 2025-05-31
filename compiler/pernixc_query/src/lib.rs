//! Implements a query system powering the incremental compilation and semantic
//! analysis

use std::sync::atomic::AtomicBool;

use getset::{CopyGetters, Getters};
pub use key::Key;
use parking_lot::Mutex;
pub use pernixc_query_derive::Key;

pub mod call_graph;
pub mod key;
pub mod map;
pub mod runtime;

/// The central data structure for the Pernix compiler storing all the semantic
/// information about the program.
#[derive(Debug, Default, Getters, CopyGetters)]
pub struct Database {
    map: map::Map,
    call_graph: Mutex<call_graph::CallGraph>,
    /// The current version of the database, which is incremented whenever an
    /// update is made to the database.
    #[get_copy = "pub"]
    version: usize,
    last_was_query: AtomicBool,

    /// The runtime environment for the query system, containing executors and
    /// serialization components.
    pub runtime: runtime::Runtime,
}

#[cfg(test)]
mod test;
