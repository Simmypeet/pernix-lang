//! Contains the definition of the [`Database`] struct.

use std::sync::atomic::AtomicBool;

use getset::CopyGetters;
use parking_lot::Mutex;
use serde::{
    de::DeserializeSeed, ser::SerializeStruct, Deserialize, Serialize,
};

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

/// A deserializer for the `Database` struct that uses a registry for dynamic
/// type handling.
#[derive(Debug, Clone, Copy)]
pub struct DatabaseDeserializer<'a>(pub &'a crate::runtime::serde::Registry);

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Deserialize,
)]
#[serde(rename_all = "snake_case")]
enum DatabaseFields {
    Map,
    CallGraph,
    Version,
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
        set_serde_registry(self.0, || {
            let mut map = None;
            let mut call_graph = None;
            let mut version = None;
            let mut last_was_query = None;

            while let Some(key) = map_access.next_key()? {
                match key {
                    DatabaseFields::Map => {
                        if map.is_some() {
                            return Err(serde::de::Error::duplicate_field(
                                "map",
                            ));
                        }
                        map = Some(map_access.next_value()?);
                    }
                    DatabaseFields::CallGraph => {
                        if call_graph.is_some() {
                            return Err(serde::de::Error::duplicate_field(
                                "call_graph",
                            ));
                        }
                        call_graph = Some(map_access.next_value()?);
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

            let map =
                map.ok_or_else(|| serde::de::Error::missing_field("map"))?;
            let call_graph = call_graph
                .ok_or_else(|| serde::de::Error::missing_field("call_graph"))?;
            let version = version
                .ok_or_else(|| serde::de::Error::missing_field("version"))?;
            let last_was_query = last_was_query.ok_or_else(|| {
                serde::de::Error::missing_field("last_was_query")
            })?;

            Ok(Database {
                map,
                call_graph: Mutex::new(call_graph),
                version,
                last_was_query,
            })
        })
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        set_serde_registry(self.0, || {
            let map = seq.next_element()?.ok_or_else(|| {
                serde::de::Error::invalid_length(
                    0,
                    &"struct Database with 4 elements",
                )
            })?;

            let call_graph = seq.next_element()?.ok_or_else(|| {
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
        })
    }
}
