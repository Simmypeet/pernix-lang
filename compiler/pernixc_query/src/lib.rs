//! Implements a query system powering the incremental compilation and semantic
//! analysis

use getset::Getters;
pub use key::Key;
use map::Map;
use parking_lot::Mutex;
pub use pernixc_query_derive::Key;

pub mod call_graph;
pub mod executor;
pub mod key;
mod map;

/// The central data structure for the Pernix compiler storing all the semantic
/// information about the program.
#[derive(Debug, Default, Getters)]
pub struct Database {
    call_graph: Mutex<call_graph::CallGraph>,
    /// The registry of executors allowing registering and retrieving executors
    /// for different query key types.
    #[get = "pub"]
    executor_registry: executor::Registry,
    map: Map,
}
