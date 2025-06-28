//! Contains the definition of the [`Database`] struct.

use std::{io::BufWriter, sync::atomic::AtomicBool};

use getset::CopyGetters;
use parking_lot::Mutex;
use pernixc_serialize::{
    binary::ser::BinarySerializer,
    ser::{Serializer, Tuple},
    Deserialize, Serialize,
};

use crate::{
    runtime::{
        persistence::Persistence,
        serde::{DynamicDeserialize, DynamicSerialize},
    },
    Engine, Key,
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

impl Engine {
    /// Attempts to save the current database to persistent storage (if
    /// persistence is configured).
    pub fn try_save_database(&self) -> Result<(), std::io::Error> {
        let Some(persistence) = self.runtime.persistence.as_ref() else {
            return Ok(());
        };

        let call_graph = self.database.call_graph.lock();

        let mut serialize_map_result = Ok(());
        let mut serialize_call_graph_result = Ok(());
        let mut version_info_result = Ok(());

        rayon::scope(|s| {
            s.spawn(|_| {
                serialize_map_result =
                    persistence.serialize_map(&self.database.map);
            });

            s.spawn(|_| {
                serialize_call_graph_result =
                    persistence.serialize_call_graph(&call_graph);
            });

            s.spawn(|_| {
                let path = persistence
                    .path()
                    .join(Persistence::CALL_GRAPH_DIRECTORY)
                    .join("version_info.dat");

                // make sure that the parent directory exists
                if let Some(parent) = path.parent() {
                    if !parent.exists() {
                        if let Err(err) = std::fs::create_dir_all(parent) {
                            version_info_result = Err(err);
                            return;
                        }
                    }
                } else {
                    version_info_result = Err(std::io::Error::other(
                        "Invalid path for version info",
                    ));
                    return;
                }

                let file = match std::fs::File::create(&path) {
                    Ok(file) => file,
                    Err(error) => {
                        version_info_result = Err(error);
                        return;
                    }
                };

                let mut serializer =
                    BinarySerializer::new(BufWriter::new(file));

                match serializer.emit_tuple(2, &(), |mut tuple, ext| {
                    tuple.serialize_element(&self.database.version, ext)?;
                    tuple.serialize_element(
                        &self
                            .database
                            .last_was_query
                            .load(std::sync::atomic::Ordering::Relaxed),
                        ext,
                    )?;

                    Ok(())
                }) {
                    Ok(()) => {}
                    Err(err) => {
                        version_info_result = Err(err);
                    }
                }
            });
        });

        serialize_map_result?;
        serialize_call_graph_result?;
        version_info_result?;

        Ok(())
    }
}
