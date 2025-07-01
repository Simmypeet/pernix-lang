//! Defines the protocol for persistence incremental compilation database.

use std::{
    self,
    any::Any,
    cell::RefCell,
    path::{Path, PathBuf},
    sync::Arc,
};

use getset::Getters;
use parking_lot::RwLock;
use pernixc_hash::HashSet;
use pernixc_serialize::{
    binary::{de::BinaryDeserializer, ser::BinarySerializer},
    Deserialize as _, Serialize as _,
};
use pernixc_stable_type_id::StableTypeID;
use rayon::iter::{IntoParallelRefIterator as _, ParallelIterator};
use redb::TableDefinition;
use tracing::trace_span;

use crate::{
    database::query_tracker::{QueryTracker, VersionInfo},
    fingerprint,
    key::DynamicBox,
    runtime::persistence::serde::{DynamicDeserialize, DynamicSerialize},
    Key,
};

pub mod serde;

const TABLE: TableDefinition<(u128, u128), &[u8]> =
    TableDefinition::new("persistence");

/// Manages the persistence of the incremental compilation database including
/// writing and reading the database to and from a storage path.
#[derive(Debug, Getters)]
#[allow(clippy::type_complexity)]
pub struct Persistence {
    value_cache_database: redb::Database,
    dependency_graph_database: redb::Database,
    version_info_database: redb::Database,

    path: PathBuf,

    /// The directory where all the value caches databases are stored.
    #[get = "pub"]
    value_cache_path: PathBuf,

    /// The directory where the query dependencies databases are stored.
    #[get = "pub"]
    query_dependency_graph_path: PathBuf,

    /// The directory where query version information databases are stored.
    #[get = "pub"]
    query_version_info_path: PathBuf,

    /// The file where the [`crate::database::Database`] version state is
    /// stored.
    #[get = "pub"]
    database_snapshot_path: PathBuf,

    serde_extension: Arc<dyn Any + Send + Sync>,
    skip_keys: HashSet<StableTypeID>,

    serialize_any_value: fn(
        StableTypeID,
        &dyn Any,
        &mut BinarySerializer<Box<dyn WriteAny>>,
        &dyn Any,
    ) -> Result<(), std::io::Error>,
    deserialize_any_value: fn(
        StableTypeID,
        &mut dyn Any,
        &mut BinaryDeserializer<Box<dyn ReadAny>>,
        &dyn Any,
    ) -> Result<(), std::io::Error>,
    deserialize_dependencies: fn(
        &dyn Any,
        &mut BinaryDeserializer<Box<dyn ReadAny>>,
    )
        -> Result<HashSet<DynamicBox>, std::io::Error>,
    deserialize_version_info: fn(
        &dyn Any,
        &mut BinaryDeserializer<Box<dyn ReadAny>>,
    ) -> Result<VersionInfo, std::io::Error>,

    serialize_call_graph: fn(
        &QueryTracker,
        &dyn Any,
        &redb::Database,
        &redb::Database,
    ) -> Result<(), std::io::Error>,
}

thread_local! {
    static BUFFER: RefCell<Vec<u8>> = const { RefCell::new(Vec::new()) };
}

/// A supertrait combining [`std::io::Write`] and [Any] traits, allowing
/// [`BinarySerializer`] and [`Persistence`] to serialize data to any target
/// by using dynamic dispatch.
///
/// [`Any`] allows the implementation to downcasting the writer to a specific
/// type after serialization, if needed.
pub trait WriteAny: std::io::Write + Any {}

impl<T: std::io::Write + Any> WriteAny for T {}

/// A supertrait combining [`std::io::Read`] and [Any] traits, allowing
/// [`BinaryDeserializer`] and [`Persistence`] to deserialize data from any
/// source by using dynamic dispatch.
///
/// [`Any`] allows the implementation to downcast the reader to a specific
/// type after deserialization, if needed.
pub trait ReadAny: std::io::Read + Any {}

impl<T: std::io::Read + Any> ReadAny for T {}

impl Persistence {
    /// The directory where the query tracker is stored.
    pub const QUERY_TRACKER_DIRECTORY: &'static str = "query_tracker";

    /// The directory where the value map databases are stored.
    pub const VALUE_MAP_DB: &'static str = "value_cache.db";

    /// The directory where the dependency graph is stored.
    pub const DEPENDENCY_GRAPH_DB: &'static str = "dependency_graph.db";

    /// The directory where the version information is stored.
    pub const VERSION_INFO_DB: &'static str = "version_info.db";

    /// The file where the database snapshot is stored.
    pub const DATABASE_SNAPSHOT_FILE: &'static str = "snapshot.dat";

    /// Creates a new instance of [`Persistence`] with the specified path where
    /// the database is stored and the serde extension where the types that will
    /// be serialized and deserialized are registered.
    #[allow(clippy::too_many_lines)]
    pub fn new<
        E: DynamicSerialize<BinarySerializer<Box<dyn WriteAny>>>
            + DynamicDeserialize<BinaryDeserializer<Box<dyn ReadAny>>>
            + Send
            + Sync
            + 'static,
    >(
        path: PathBuf,
        serde_extension: Arc<E>,
    ) -> Result<Self, std::io::Error> {
        let serialize_any_value =
            |stable_type_id: StableTypeID,
             any_value: &dyn Any,
             serializer: &mut BinarySerializer<Box<dyn WriteAny>>,
             serde_extension: &dyn Any| {
                let serde_extension = serde_extension
                    .downcast_ref::<E>()
                    .expect("serde_extension must match the expected type");

                let helper = serde_extension
                    .serialization_helper_by_type_id()
                    .get(&stable_type_id)
                    .unwrap_or_else(|| {
                        panic!(
                            "No serialization helper found for type ID: \
                             {stable_type_id:?}",
                        )
                    });

                helper.serialize_any_value(
                    any_value,
                    serializer,
                    serde_extension,
                )
            };

        let deserialize_any_value =
            |stable_type_id: StableTypeID,
             result_buffer: &mut dyn Any,
             deserializer: &mut BinaryDeserializer<Box<dyn ReadAny>>,
             serde_extension: &dyn Any| {
                let serde_extension = serde_extension
                    .downcast_ref::<E>()
                    .expect("serde_extension must match the expected type");

                let helper = serde_extension
                    .deserialization_helper_by_type_id()
                    .get(&stable_type_id)
                    .unwrap_or_else(|| {
                        panic!(
                            "No deserialization helper found for type ID: \
                             {stable_type_id:?}",
                        )
                    });

                helper.deserialize_any_value(
                    result_buffer,
                    deserializer,
                    serde_extension,
                )
            };

        let deserialize_dependencies =
            |serde_extension: &dyn Any,
             deserializer: &mut BinaryDeserializer<Box<dyn ReadAny>>| {
                let serde_extension = serde_extension
                    .downcast_ref::<E>()
                    .expect("serde_extension must match the expected type");

                HashSet::<DynamicBox>::deserialize(
                    deserializer,
                    serde_extension,
                )

            };

        let deserialize_version_info =
            |serde_extension: &dyn Any,
             deserializer: &mut BinaryDeserializer<Box<dyn ReadAny>>| {
                let serde_extension = serde_extension
                    .downcast_ref::<E>()
                    .expect("serde_extension must match the expected type");

                VersionInfo::deserialize(deserializer, serde_extension)
            };

        if !path.exists() {
            std::fs::create_dir_all(&path)
                .expect("Failed to create persistence directory");
        }

        let value_cache_path = {
            let mut path = path.clone();
            path.push(Self::VALUE_MAP_DB);
            path
        };
        let query_dependency_graph_path = {
            let mut path = path.clone();
            path.push(Self::QUERY_TRACKER_DIRECTORY);
            path.push(Self::DEPENDENCY_GRAPH_DB);
            path
        };
        let query_version_info_path = {
            let mut path = path.clone();
            path.push(Self::QUERY_TRACKER_DIRECTORY);
            path.push(Self::VERSION_INFO_DB);
            path
        };
        let database_snapshot_path = {
            let mut path = path.clone();
            path.push(Self::QUERY_TRACKER_DIRECTORY);
            path.push(Self::DATABASE_SNAPSHOT_FILE);
            path
        };

        let value_cache_database = Self::create_database(&value_cache_path)?;
        let dependency_graph_database =
            Self::create_database(&query_dependency_graph_path)?;
        let version_info_database =
            Self::create_database(&query_version_info_path)?;

        Ok(Self {
            value_cache_path,
            query_dependency_graph_path,
            query_version_info_path,
            database_snapshot_path,
            path,

            value_cache_database,
            dependency_graph_database,
            version_info_database,

            serde_extension: serde_extension as Arc<dyn Any + Send + Sync>,
            skip_keys: HashSet::default(),

            serialize_any_value,
            deserialize_any_value,
            deserialize_dependencies,
            deserialize_version_info,

            serialize_call_graph: serialize_call_graph::<E>,
        })
    }

    fn create_database(path: &Path) -> Result<redb::Database, std::io::Error> {
        match path.parent() {
            Some(parent) if !parent.exists() => {
                std::fs::create_dir_all(parent).map_err(|e| {
                    std::io::Error::other(format!(
                        "Failed to create parent directory: {e}",
                    ))
                })?;
            }

            _ => {}
        }

        redb::Database::create(path).map_err(|e| {
            std::io::Error::other(format!(
                "Failed to create database at path {}: {e}",
                path.display(),
            ))
        })
    }

    /// Returns the directory path where the database is stored.
    #[must_use]
    #[allow(clippy::missing_const_for_fn)]
    pub fn path(&self) -> &Path { &self.path }

    /// Registers a key to be skipped during serialization.
    pub fn register_skip_key<K: Key>(&mut self) {
        self.skip_keys.insert(K::STABLE_TYPE_ID);
    }

    /// Serializes thne entire map to the persistence storage.
    pub fn serialize_call_graph(
        &self,
        query_tracker: &QueryTracker,
    ) -> Result<(), std::io::Error> {
        (self.serialize_call_graph)(
            query_tracker,
            self.serde_extension.as_ref(),
            &self.dependency_graph_database,
            &self.version_info_database,
        )
    }

    /// Attempts to load the version information for a given key fingerprint
    /// from the persistence storage.
    pub fn try_load_version_info<K: Key>(
        &self,
        key_fingerprint: u128,
    ) -> Result<Option<VersionInfo>, std::io::Error> {
        let table = self.version_info_database.begin_read().map_err(|e| {
            std::io::Error::other(format!(
                "Failed to begin read transaction for table: {e}"
            ))
        })?;
        let table = table.open_table(TABLE).map_err(|e| {
            std::io::Error::other(format!("Failed to open table: {e}"))
        })?;

        if let Some(buffer) =
            table.get(&(K::STABLE_TYPE_ID.as_u128(), key_fingerprint)).unwrap()
        {
            let mut deserializer = BinaryDeserializer::<Box<dyn ReadAny>>::new(
                Box::new(std::io::Cursor::new(buffer.value().to_vec())),
            );

            Ok(Some((self.deserialize_version_info)(
                self.serde_extension.as_ref(),
                &mut deserializer,
            )?))
        } else {
            Ok(None)
        }
    }

    /// Attempts to load a list of dependencies of a given key fingerprint
    pub fn try_load_dependencies<K: Key>(
        &self,
        key_fingerprint: u128,
    ) -> Result<Option<HashSet<DynamicBox>>, std::io::Error> {
        let table =
            self.dependency_graph_database.begin_read().map_err(|e| {
                std::io::Error::other(format!(
                    "Failed to begin read transaction for table: {e}"
                ))
            })?;
        let table = table.open_table(TABLE).map_err(|e| {
            std::io::Error::other(format!("Failed to open table: {e}"))
        })?;

        if let Some(buffer) =
            table.get(&(K::STABLE_TYPE_ID.as_u128(), key_fingerprint)).unwrap()
        {
            let mut deserializer = BinaryDeserializer::<Box<dyn ReadAny>>::new(
                Box::new(std::io::Cursor::new(buffer.value().to_vec())),
            );

            Ok(Some((self.deserialize_dependencies)(
                self.serde_extension.as_ref(),
                &mut deserializer,
            )?))
        } else {
            Ok(None)
        }
    }

    /// Attempts to load a value from the persistence storage.
    pub fn try_load_value<K: Key>(
        &self,
        value_fingerprint: u128,
    ) -> Result<Option<K::Value>, std::io::Error> {
        let table = self.value_cache_database.begin_read().map_err(|e| {
            std::io::Error::other(format!(
                "Failed to begin read transaction for table: {e}"
            ))
        })?;
        let table = table.open_table(TABLE).map_err(|e| {
            std::io::Error::other(format!("Failed to open table: {e}"))
        })?;

        if let Some(buffer) = table
            .get(&(K::STABLE_TYPE_ID.as_u128(), value_fingerprint))
            .unwrap()
        {
            let mut deserializer = BinaryDeserializer::<Box<dyn ReadAny>>::new(
                Box::new(std::io::Cursor::new(buffer.value().to_vec())),
            );

            let mut result_buffer: Option<K::Value> = None;
            (self.deserialize_any_value)(
                K::STABLE_TYPE_ID,
                &mut result_buffer as &mut dyn Any,
                &mut deserializer,
                self.serde_extension.as_ref(),
            )?;

            Ok(Some(result_buffer.unwrap()))
        } else {
            Ok(None)
        }
    }

    /// Saves the version information for a given key fingerprint to the
    /// persistence storage.
    pub fn save_version_info<K: Key>(
        &self,
        fingerprint: u128,
        version_info: &VersionInfo,
    ) -> Result<(), std::io::Error> {
        let buffer = BUFFER.with(|b| std::mem::take(&mut *b.borrow_mut()));

        let mut binary_serializer = BinarySerializer::new(buffer);

        version_info.serialize(&mut binary_serializer, &())?;

        let mut buffer = binary_serializer.into_inner();

        let tx = self.version_info_database.begin_write().map_err(|e| {
            std::io::Error::other(format!(
                "Failed to begin write transaction: {e}",
            ))
        })?;

        {
            let mut table = tx.open_table(TABLE).unwrap();
            table
                .insert(
                    (K::STABLE_TYPE_ID.as_u128(), fingerprint),
                    buffer.as_slice(),
                )
                .map_err(|e| {
                    std::io::Error::other(format!(
                        "Failed to insert entry into table: {e}",
                    ))
                })?;
        }

        tx.commit().map_err(|e| {
            std::io::Error::other(format!("Failed to commit transaction: {e}",))
        })?;

        BUFFER.with(|b| {
            // clear the buffer but keep the allocated memory
            buffer.clear();
            *b.borrow_mut() = buffer;
        });

        Ok(())
    }

    /// Saves a value to the persistence storage.
    ///
    /// # Safety
    ///
    /// The [`fingerprint`] provided must match the fingerprint of the
    /// value being saved. This function is meant to be used for avoiding
    /// re-computing the fingerprint of the value.
    pub unsafe fn save_with_fingerprint<K: Key>(
        &self,
        value: &K::Value,
        fingerprint: u128,
    ) -> Result<(), std::io::Error> {
        if self.skip_keys.contains(&K::STABLE_TYPE_ID) {
            // If the key is registered to be skipped, do not save it.
            return Ok(());
        }

        let buffer = BUFFER.with(|b| std::mem::take(&mut *b.borrow_mut()));
        let mut binary_serializer =
            BinarySerializer::<Box<dyn WriteAny>>::new(Box::new(buffer));

        (self.serialize_any_value)(
            K::STABLE_TYPE_ID,
            value as &dyn Any,
            &mut binary_serializer,
            self.serde_extension.as_ref(),
        )?;

        let box_any: Box<dyn Any> = binary_serializer.into_inner();
        let mut buffer =
            box_any.downcast::<Vec<u8>>().expect("Buffer must be a Vec<u8>");

        let tx = self.value_cache_database.begin_write().map_err(|e| {
            std::io::Error::other(format!(
                "Failed to begin write transaction: {e}",
            ))
        })?;

        {
            let mut table = tx.open_table(TABLE).unwrap();
            table
                .insert(
                    (K::STABLE_TYPE_ID.as_u128(), fingerprint),
                    buffer.as_slice(),
                )
                .map_err(|e| {
                    std::io::Error::other(format!(
                        "Failed to insert entry into table: {e}",
                    ))
                })?;
        }

        tx.commit().map_err(|e| {
            std::io::Error::other(format!("Failed to commit transaction: {e}",))
        })?;

        BUFFER.with(|b| {
            // clear the buffer but keep the allocated memory
            buffer.clear();
            *b.borrow_mut() = *buffer;
        });

        Ok(())
    }

    /// Saves a value to the persistence storage.
    pub fn save<K: Key>(&self, value: &K::Value) -> Result<(), std::io::Error> {
        unsafe {
            self.save_with_fingerprint::<K>(
                value,
                fingerprint::fingerprint(value),
            )
        }
    }
}

#[allow(clippy::too_many_lines)]
fn serialize_call_graph<
    E: DynamicSerialize<BinarySerializer<Box<dyn WriteAny>>>
        + DynamicDeserialize<BinaryDeserializer<Box<dyn ReadAny>>>
        + Send
        + Sync
        + 'static,
>(
    query_tracker: &QueryTracker,
    serde_extension: &dyn Any,
    dependency_graph_database: &redb::Database,
    version_info_database: &redb::Database,
) -> Result<(), std::io::Error> {
    let extension = serde_extension
        .downcast_ref::<E>()
        .expect("serde_extension must match the expected type");

    let mut dependency_graph = Ok(());
    let mut version_info = Ok(());

    rayon::scope(|s| {
        s.spawn(|_| {
            let _span = trace_span!("serialize dependency graph").entered();

            let tx =
                match dependency_graph_database.begin_write().map_err(|e| {
                    std::io::Error::other(format!(
                        "Failed to begin write transaction: {e}",
                    ))
                }) {
                    Ok(tx) => tx,
                    Err(e) => {
                        dependency_graph = Err(e);
                        return;
                    }
                };

            let table = RwLock::new(
                match tx.open_table(TABLE).map_err(|e| {
                    std::io::Error::other(format!("Failed to open table: {e}",))
                }) {
                    Ok(table) => table,
                    Err(e) => {
                        dependency_graph = Err(e);
                        return;
                    }
                },
            );

            let result = query_tracker
                .dependency_graph()
                .par_iter()
                .map(|(key, dependencies)| {
                    let buffer =
                        BUFFER.with(|b| std::mem::take(&mut *b.borrow_mut()));

                    let mut serializer =
                        BinarySerializer::<Box<dyn WriteAny>>::new(Box::new(
                            buffer,
                        ));

                    dependencies.serialize(&mut serializer, extension)?;

                    let any_box: Box<dyn Any> = serializer.into_inner();
                    let mut buffer = any_box.downcast::<Vec<u8>>().unwrap();

                    table
                        .write()
                        .insert(
                            (key.stable_type_id().as_u128(), key.fingerprint()),
                            buffer.as_slice(),
                        )
                        .map_err(|e| {
                            std::io::Error::other(format!(
                                "Failed to insert entry into table: {e}",
                            ))
                        })?;

                    BUFFER.with(|b| {
                        // clear the buffer but keep the allocated
                        // memory
                        buffer.clear();
                        *b.borrow_mut() = *buffer;
                    });

                    Ok(())
                })
                .collect::<Result<(), std::io::Error>>();

            if let Err(e) = result {
                dependency_graph = Err(e);
                return;
            }

            drop(table);

            dependency_graph = tx.commit().map_err(|e| {
                std::io::Error::other(format!(
                    "Failed to commit transaction: {e}",
                ))
            });
        });

        s.spawn(|_| {
            let _span = trace_span!("serialize version info").entered();

            let tx = match version_info_database.begin_write().map_err(|e| {
                std::io::Error::other(format!(
                    "Failed to begin write transaction: {e}",
                ))
            }) {
                Ok(tx) => tx,
                Err(e) => {
                    version_info = Err(e);
                    return;
                }
            };

            let table = RwLock::new(
                match tx.open_table(TABLE).map_err(|e| {
                    std::io::Error::other(format!("Failed to open table: {e}",))
                }) {
                    Ok(table) => table,
                    Err(e) => {
                        version_info = Err(e);
                        return;
                    }
                },
            );

            let result = query_tracker
                .version_info_by_keys()
                .par_iter()
                .map(|(key, version_info)| {
                    let buffer =
                        BUFFER.with(|b| std::mem::take(&mut *b.borrow_mut()));
                    let mut serializer =
                        BinarySerializer::<Box<dyn WriteAny>>::new(Box::new(
                            buffer,
                        ));

                    version_info.serialize(&mut serializer, extension)?;

                    let any_box: Box<dyn Any> = serializer.into_inner();
                    let mut buffer = any_box.downcast::<Vec<u8>>().unwrap();

                    table
                        .write()
                        .insert(
                            (key.stable_type_id().as_u128(), key.fingerprint()),
                            buffer.as_slice(),
                        )
                        .map_err(|e| {
                            std::io::Error::other(format!(
                                "Failed to insert entry into table: {e}",
                            ))
                        })?;

                    BUFFER.with(|b| {
                        // clear the buffer but keep the allocated
                        // memory
                        buffer.clear();
                        *b.borrow_mut() = *buffer;
                    });

                    Ok(())
                })
                .collect::<Result<(), std::io::Error>>();

            if let Err(e) = result {
                version_info = Err(e);
                return;
            }

            drop(table);

            version_info = tx.commit().map_err(|e| {
                std::io::Error::other(format!(
                    "Failed to commit transaction: {e}",
                ))
            });
        });
    });

    dependency_graph?;
    version_info?;

    Ok(())
}

#[cfg(test)]
mod test;
