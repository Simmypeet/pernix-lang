//! Contains the definition of the [`Database`] struct.

use std::{
    io::{BufReader, BufWriter},
    sync::atomic::AtomicBool,
};

use getset::CopyGetters;
use parking_lot::Mutex;
use pernixc_serialize::{
    binary::{de::BinaryDeserializer, ser::BinarySerializer},
    de::{Deserializer as _, TupleAccess as _},
    ser::{Serializer, Tuple},
    Deserialize, Serialize,
};

use crate::{
    database::map::Map,
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

impl Persistence {
    /// Loads the incremental compilation database from the persistence
    /// storage, including the call graph and version information.
    pub fn load_database(&self) -> Result<Database, std::io::Error> {
        let mut call_graph = None;
        let mut version = None;

        rayon::scope(|s| {
            s.spawn(|_| call_graph = Some(self.load_call_graph()));
            s.spawn(|_| {
                let mut version_file_path = self.path().to_path_buf();
                version_file_path.push(Self::CALL_GRAPH_DIRECTORY);
                version_file_path.push("version.dat");

                let file = match std::fs::File::open(&version_file_path) {
                    Ok(file) => file,
                    Err(error) => {
                        version = Some(Err(error));
                        return;
                    }
                };

                let mut binary_deserializer =
                    BinaryDeserializer::new(BufReader::new(file));

                version = Some(binary_deserializer.expect_tuple(
                    2,
                    &(),
                    |mut x, extension| {
                        let version = x.next_element::<usize>(extension)?;
                        let last_was_query =
                            AtomicBool::new(x.next_element::<bool>(extension)?);

                        Ok((version, last_was_query))
                    },
                ));
            });
        });

        let call_graph = call_graph.unwrap()?;
        let version = version.unwrap()?;

        Ok(Database {
            map: Map::default(),
            call_graph: Mutex::new(call_graph),
            version: version.0,
            last_was_query: version.1,
        })
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
