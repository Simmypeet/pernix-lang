//! Contains the definition of the [`Database`] struct.

use std::sync::atomic::AtomicBool;

use getset::CopyGetters;
use parking_lot::Mutex;
use serde::{ser::SerializeStruct, Serialize};

use crate::{runtime::serde::set_serde_registry, Engine};

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

impl Serialize for Engine {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("Database", 4)?;

        set_serde_registry(&self.runtime.serde, || {
            state.serialize_field("map", &self.database.map)?;
            state.serialize_field("call_graph", &self.database.call_graph)?;
            state.serialize_field("version", &self.database.version)?;
            state.serialize_field(
                "last_was_query",
                &self.database.last_was_query,
            )?;
            state.end()
        })
    }
}
