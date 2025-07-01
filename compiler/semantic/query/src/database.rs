//! Contains the definition of the [`Database`] struct.

use std::{
    io::{BufReader, BufWriter},
    sync::atomic::AtomicBool,
};

use getset::Getters;
use parking_lot::Mutex;
use pernixc_serialize::{
    binary::{de::BinaryDeserializer, ser::BinarySerializer},
    Deserialize, Serialize,
};

use crate::{
    database::{map::Map, query_tracker::QueryTracker},
    runtime::persistence::{
        serde::{DynamicDeserialize, DynamicSerialize},
        Persistence,
    },
    Engine, Key,
};

pub mod map;
pub mod query_tracker;

/// Represents the main database structure used for storing and managing
/// compiler state, including mappings, call graphs, and versioning information.
#[derive(Debug, Default, Serialize, Deserialize, Getters)]
#[serde(
    ser_extension(DynamicSerialize<__S>),
    de_extension(DynamicDeserialize<__D>)
)]
pub struct Database {
    map: map::Map,
    query_tracker: Mutex<query_tracker::QueryTracker>,

    /// Represents a snapshot of the database version.
    #[get = "pub"]
    snapshot: Snapshot,
}

impl Drop for Database {
    fn drop(&mut self) {
        let map = std::mem::take(&mut self.map);
        let query_tracker = std::mem::take(&mut self.query_tracker);

        rayon::scope(|s| {
            s.spawn(|_| drop(map));
            s.spawn(|_| drop(query_tracker));
        });
    }
}

/// Stores the version information of the database, including whether the
/// last operation was a query or not.
#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Snapshot {
    /// The current version of the database.
    ///
    /// This value is incremented whenever the database is modified,
    /// such as when a new key-value pair is added or an existing one is
    /// updated.
    pub version: usize,

    /// Indicates whether the last operation was a query.
    pub last_was_query: AtomicBool,
}

impl Database {
    /// Checks if the database contains a key.
    pub fn contains_key<K: Key>(&self, key: &K) -> bool {
        self.map.contains_key(key)
    }
}

impl Persistence {
    /// Loads the incremental compilation database from the persistence
    /// storage, including the query tracker and version information.
    pub fn load_database(&self) -> Result<Database, std::io::Error> {
        let file = std::fs::File::open(self.database_snapshot_path())?;

        let mut binary_deserializer =
            BinaryDeserializer::new(BufReader::new(file));

        let version = Snapshot::deserialize(&mut binary_deserializer, &())?;

        Ok(Database {
            map: Map::default(),
            query_tracker: Mutex::new(QueryTracker::default()),
            snapshot: version,
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

        let query_tracker = self.database.query_tracker.lock();

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
                    persistence.serialize_call_graph(&query_tracker);
            });

            s.spawn(|_| {
                let path = persistence
                    .path()
                    .join(Persistence::DATABASE_SNAPSHOT_FILE);

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

                match self.database.snapshot.serialize(&mut serializer, &()) {
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
