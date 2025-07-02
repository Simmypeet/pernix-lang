//! Contains the definition of the [`Database`] struct.

use std::io::{BufReader, BufWriter};

use getset::Getters;
use parking_lot::Mutex;
use pernixc_serialize::{
    binary::{de::BinaryDeserializer, ser::BinarySerializer},
    Deserialize, Serialize,
};

use crate::{
    database::{
        map::Map,
        query_tracker::{QueryTracker, Snapshot},
    },
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

        let snapshot = Snapshot::deserialize(&mut binary_deserializer, &())?;

        Ok(Database {
            map: Map::default(),
            query_tracker: Mutex::new(QueryTracker::with_snapshot(snapshot)),
        })
    }
}

impl Engine {
    /// Attempts to save the current database to persistent storage (if
    /// persistence is configured).
    pub fn try_save_database(&self) -> Result<(), std::io::Error> {
        let _span = tracing::trace_span!("save database").entered();
        let Some(persistence) = self.runtime.persistence.as_ref() else {
            return Ok(());
        };

        let query_tracker = self.database.query_tracker.lock();

        let path = persistence
            .path()
            .join(Persistence::QUERY_TRACKER_DIRECTORY)
            .join(Persistence::DATABASE_SNAPSHOT_FILE);

        // make sure that the parent directory exists
        if let Some(parent) = path.parent() {
            if !parent.exists() {
                std::fs::create_dir_all(parent)?;
            }
        }

        let file = std::fs::File::create(&path)?;

        let mut serializer = BinarySerializer::new(BufWriter::new(file));

        query_tracker.snapshot().serialize(&mut serializer, &())?;
        persistence.commit()?;

        Ok(())
    }
}
