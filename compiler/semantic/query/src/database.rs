//! Contains the definition of the [`Database`] struct.

use std::sync::atomic::AtomicBool;

use getset::CopyGetters;
use parking_lot::Mutex;

use crate::Key;

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

/*
/// A serializable wrapper for the `Database` struct.
///
/// This struct provides a way to serialize a `Database` instance along with
/// its associated serialization context. It contains references to both the
/// database and the serde configuration needed for proper serialization.
#[derive(Debug, Clone, Copy)]
pub struct SerializableDatabase<'a> {
    /// Reference to the database instance to be serialized.
    pub database: &'a Database,
    /// Reference to the serde configuration for serialization.
    pub serde: &'a crate::serde::Serde,
}

impl Database {
    /// Creates a serializable wrapper for this database.
    ///
    /// This method returns a `SerializableDatabase` instance that can be
    /// serialized using the provided serde configuration.
    ///
    /// # Arguments
    /// * `serde` - The serde configuration to use for serialization
    ///
    /// # Returns
    /// A `SerializableDatabase` wrapper containing references to this database
    /// and the provided serde configuration.
    #[must_use]
    pub const fn serializable<'a>(
        &'a self,
        serde: &'a crate::serde::Serde,
    ) -> SerializableDatabase<'a> {
        SerializableDatabase { database: self, serde }
    }
}

impl Serialize for SerializableDatabase<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("Database", 4)?;

        state.serialize_field(
            "map",
            &self.database.map.serializable(self.serde),
        )?;
        state.serialize_field("call_graph", &SerializableCallGraph {
            call_graph: &self.database.call_graph.lock(),
            serde: self.serde,
        })?;
        state.serialize_field("version", &self.database.version)?;
        state
            .serialize_field("last_was_query", &self.database.last_was_query)?;
        state.end()
    }
}

*/

impl Database {
    /// Checks if the database contains a key.
    pub fn contains_key<K: Key>(&self, key: &K) -> bool {
        self.map.contains_key(key)
    }
}

/*
/// A deserializer for the `Database` struct that uses a registry for dynamic
/// type handling.
#[derive(Debug, Clone, Copy)]
pub struct DatabaseDeserializer<'a>(pub &'a crate::serde::Serde);

impl crate::serde::Serde {
    /// Creates a new deserializer for the `Database` struct.
    ///
    /// This method returns a `DatabaseDeserializer` that can be used to
    /// deserialize a `Database` instance from a serde-compatible format.
    #[must_use]
    pub const fn database_deserializer(&self) -> DatabaseDeserializer<'_> {
        DatabaseDeserializer(self)
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Deserialize,
)]
#[serde(field_identifier)]
enum DatabaseFields {
    #[serde(rename = "map")]
    Map,
    #[serde(rename = "call_graph")]
    CallGraph,
    #[serde(rename = "version")]
    Version,
    #[serde(rename = "last_was_query")]
    LastWasQuery,
}

impl<'de> DeserializeSeed<'de> for DatabaseDeserializer<'_> {
    type Value = Database;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_struct(
            "Database",
            &["map", "call_graph", "version", "last_was_query"],
            self,
        )
    }
}

impl<'de> serde::de::Visitor<'de> for DatabaseDeserializer<'_> {
    type Value = Database;

    fn expecting(
        &self,
        formatter: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        formatter.write_str("a Database struct")
    }

    fn visit_map<M>(self, mut map_access: M) -> Result<Self::Value, M::Error>
    where
        M: serde::de::MapAccess<'de>,
    {
        let mut map = None;
        let mut call_graph = None;
        let mut version = None;
        let mut last_was_query = None;

        while let Some(key) = map_access.next_key()? {
            match key {
                DatabaseFields::Map => {
                    if map.is_some() {
                        return Err(serde::de::Error::duplicate_field("map"));
                    }

                    let default_map = map::Map::default();
                    map_access.next_value_seed(
                        &default_map.incremental_deserializable(self.0),
                    )?;

                    map = Some(default_map);
                }
                DatabaseFields::CallGraph => {
                    if call_graph.is_some() {
                        return Err(serde::de::Error::duplicate_field(
                            "call_graph",
                        ));
                    }
                    call_graph = Some(map_access.next_value_seed(
                        call_graph::DeserializableCallGraph(self.0),
                    )?);
                }
                DatabaseFields::Version => {
                    if version.is_some() {
                        return Err(serde::de::Error::duplicate_field(
                            "version",
                        ));
                    }
                    version = Some(map_access.next_value()?);
                }
                DatabaseFields::LastWasQuery => {
                    if last_was_query.is_some() {
                        return Err(serde::de::Error::duplicate_field(
                            "last_was_query",
                        ));
                    }
                    last_was_query = Some(map_access.next_value()?);
                }
            }
        }

        let map = map.ok_or_else(|| serde::de::Error::missing_field("map"))?;
        let call_graph = call_graph
            .ok_or_else(|| serde::de::Error::missing_field("call_graph"))?;
        let version = version
            .ok_or_else(|| serde::de::Error::missing_field("version"))?;
        let last_was_query = last_was_query
            .ok_or_else(|| serde::de::Error::missing_field("last_was_query"))?;

        Ok(Database {
            map,
            call_graph: Mutex::new(call_graph),
            version,
            last_was_query,
        })
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        let map = map::Map::default();
        seq.next_element_seed(&map.incremental_deserializable(self.0))?
            .ok_or_else(|| {
                serde::de::Error::invalid_length(
                    0,
                    &"struct Database with 4 elements",
                )
            })?;

        let call_graph = seq
            .next_element_seed(call_graph::DeserializableCallGraph(self.0))?
            .ok_or_else(|| {
                serde::de::Error::invalid_length(
                    1,
                    &"struct Database with 4 elements",
                )
            })?;

        let version = seq.next_element()?.ok_or_else(|| {
            serde::de::Error::invalid_length(
                2,
                &"struct Database with 4 elements",
            )
        })?;

        let last_was_query = seq.next_element()?.ok_or_else(|| {
            serde::de::Error::invalid_length(
                3,
                &"struct Database with 4 elements",
            )
        })?;

        Ok(Database {
            map,
            call_graph: Mutex::new(call_graph),
            version,
            last_was_query,
        })
    }
}

*/
