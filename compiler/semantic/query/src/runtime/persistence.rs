//! Defines the protocol for persistence incremental compilation database.

use pernixc_hash::HashSet;
use pernixc_stable_hash::Value;

use crate::{
    database::{DynamicKey, ValueVersion},
    Engine, Key,
};

impl Engine {
    pub(crate) fn try_load_value<K: Key>(
        &self,
        value_fingerprint: u128,
    ) -> Option<K::Value> {
        // TODO: implement this
        None
    }

    pub(crate) fn try_load_value_version<K: Key>(
        &self,
        key_fingerprint: u128,
    ) -> Option<ValueVersion> {
        // TODO: implement this
        None
    }

    pub(crate) fn try_load_dependencies<K: Key>(
        &self,
        key_fingerprint: u128,
    ) -> Option<HashSet<DynamicKey>> {
        // TODO: implement this
        None
    }
}
/*

use std::{
    self,
    any::Any,
    cell::RefCell,
    io::{BufReader, BufWriter, Cursor},
    path::{Path, PathBuf},
    sync::Arc,
};

use enum_as_inner::EnumAsInner;
use getset::Getters;
use ouroboros::self_referencing;
use parking_lot::{RwLock, RwLockUpgradableReadGuard};
use pernixc_hash::HashSet;
use pernixc_serialize::{
    binary::{de::BinaryDeserializer, ser::BinarySerializer},
    Deserialize as _, Serialize as _,
};
use pernixc_stable_type_id::StableTypeID;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use redb::{ReadableTable as _, Table, TableDefinition, WriteTransaction};

use crate::{
    database::query_tracker::VersionInfo,
    fingerprint,
    key::DynamicKey,
    runtime::persistence::serde::{DynamicDeserialize, DynamicSerialize},
    Key,
};

pub mod serde;

const TABLE: TableDefinition<(u128, u128), &[u8]> =
    TableDefinition::new("persistence");

/// Enumeration of writer that will be used throughout the persistence
/// system.
#[derive(Debug, EnumAsInner)]
#[allow(missing_docs)]
pub enum Writer {
    Vec(Vec<u8>),
    File(BufWriter<std::fs::File>),
}

impl std::io::Write for Writer {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            Self::Vec(v) => v.write(buf),
            Self::File(f) => f.write(buf),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            Self::Vec(v) => v.flush(),
            Self::File(f) => f.flush(),
        }
    }

    fn write_vectored(
        &mut self,
        bufs: &[std::io::IoSlice<'_>],
    ) -> std::io::Result<usize> {
        match self {
            Self::Vec(v) => v.write_vectored(bufs),
            Self::File(f) => f.write_vectored(bufs),
        }
    }

    fn write_all(&mut self, buf: &[u8]) -> std::io::Result<()> {
        match self {
            Self::Vec(v) => v.write_all(buf),
            Self::File(f) => f.write_all(buf),
        }
    }

    fn write_fmt(
        &mut self,
        args: std::fmt::Arguments<'_>,
    ) -> std::io::Result<()> {
        match self {
            Self::Vec(v) => v.write_fmt(args),
            Self::File(f) => f.write_fmt(args),
        }
    }

    fn by_ref(&mut self) -> &mut Self
    where
        Self: Sized,
    {
        self
    }
}

/// Enumeration of readers that will be used throughout the persistence
/// system.
#[derive(Debug, EnumAsInner)]
#[allow(missing_docs)]
pub enum Reader {
    Vec(Cursor<Vec<u8>>),
    File(BufReader<std::fs::File>),
}

impl std::io::Read for Reader {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        match self {
            Self::Vec(v) => v.read(buf),
            Self::File(f) => f.read(buf),
        }
    }

    fn read_vectored(
        &mut self,
        bufs: &mut [std::io::IoSliceMut<'_>],
    ) -> std::io::Result<usize> {
        match self {
            Self::Vec(v) => v.read_vectored(bufs),
            Self::File(f) => f.read_vectored(bufs),
        }
    }

    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> std::io::Result<usize> {
        match self {
            Self::Vec(v) => v.read_to_end(buf),
            Self::File(f) => f.read_to_end(buf),
        }
    }

    fn read_to_string(&mut self, buf: &mut String) -> std::io::Result<usize> {
        match self {
            Self::Vec(v) => v.read_to_string(buf),
            Self::File(f) => f.read_to_string(buf),
        }
    }

    fn read_exact(&mut self, buf: &mut [u8]) -> std::io::Result<()> {
        match self {
            Self::Vec(v) => v.read_exact(buf),
            Self::File(f) => f.read_exact(buf),
        }
    }

    fn by_ref(&mut self) -> &mut Self
    where
        Self: Sized,
    {
        self
    }
}

#[self_referencing]
struct WriteTransactionWithTable {
    write_transaction: WriteTransaction,

    #[borrows(write_transaction)]
    #[not_covariant]
    table: Table<'this, (u128, u128), &'static [u8]>,
}

/// Manages the persistence of the incremental compilation database including
/// writing and reading the database to and from a storage path.
#[derive(Getters)]
#[allow(clippy::type_complexity)]
pub struct Persistence {
    value_cache_database: redb::Database,
    dependency_graph_database: redb::Database,
    version_info_database: redb::Database,

    value_cache_write_transaction: RwLock<Option<WriteTransactionWithTable>>,
    dependency_graph_write_transaction:
        RwLock<Option<WriteTransactionWithTable>>,
    version_info_write_transaction: RwLock<Option<WriteTransactionWithTable>>,

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
        &mut BinarySerializer<Writer>,
        &dyn Any,
    ) -> Result<(), std::io::Error>,
    deserialize_any_value: fn(
        StableTypeID,
        &mut dyn Any,
        &mut BinaryDeserializer<Reader>,
        &dyn Any,
    ) -> Result<(), std::io::Error>,
    deserialize_dependencies: fn(
        &dyn Any,
        &mut BinaryDeserializer<Reader>,
    )
        -> Result<HashSet<DynamicKey>, std::io::Error>,
    deserialize_version_info: fn(
        &dyn Any,
        &mut BinaryDeserializer<Reader>,
    ) -> Result<VersionInfo, std::io::Error>,

    serialize_dependency_graph: fn(
        &HashSet<DynamicKey>,
        &mut BinarySerializer<Writer>,
        &dyn Any,
    ) -> Result<(), std::io::Error>,
}

impl std::fmt::Debug for Persistence {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Persistence")
            .field("value_cache_path", &self.value_cache_path)
            .field(
                "query_dependency_graph_path",
                &self.query_dependency_graph_path,
            )
            .field("query_version_info_path", &self.query_version_info_path)
            .field("database_snapshot_path", &self.database_snapshot_path)
            .finish_non_exhaustive()
    }
}

thread_local! {
    static BUFFER: RefCell<Vec<u8>> = const { RefCell::new(Vec::new()) };
}

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
        E: DynamicSerialize<BinarySerializer<Writer>>
            + DynamicDeserialize<BinaryDeserializer<Reader>>
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
             serializer: &mut BinarySerializer<Writer>,
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
             deserializer: &mut BinaryDeserializer<Reader>,
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
             deserializer: &mut BinaryDeserializer<Reader>| {
                let serde_extension = serde_extension
                    .downcast_ref::<E>()
                    .expect("serde_extension must match the expected type");

                HashSet::<DynamicKey>::deserialize(
                    deserializer,
                    serde_extension,
                )
            };

        let deserialize_version_info =
            |serde_extension: &dyn Any,
             deserializer: &mut BinaryDeserializer<Reader>| {
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

            value_cache_write_transaction: RwLock::new(None),
            dependency_graph_write_transaction: RwLock::new(None),
            version_info_write_transaction: RwLock::new(None),

            serde_extension: serde_extension as Arc<dyn Any + Send + Sync>,
            skip_keys: HashSet::default(),

            serialize_any_value,
            deserialize_any_value,
            deserialize_dependencies,
            deserialize_version_info,

            serialize_dependency_graph: serialize_dependency_graph::<E>,
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

    /// Commits all the saved changes made by various `save_*` methods to the
    /// persistence storage.
    pub fn commit(&self) -> Result<(), std::io::Error> {
        let value_cache_write_transaction =
            self.value_cache_write_transaction.write().take();
        let dependency_graph_write_transaction =
            self.dependency_graph_write_transaction.write().take();
        let version_info_write_transaction =
            self.version_info_write_transaction.write().take();

        [
            value_cache_write_transaction,
            dependency_graph_write_transaction,
            version_info_write_transaction,
        ]
        .into_par_iter()
        .try_for_each(|tx| {
            tx.map_or(Ok(()), |tx| {
                let tx = tx.into_heads();
                tx.write_transaction.commit().map_err(|e| {
                    std::io::Error::other(format!(
                        "Failed to commit transaction: {e}",
                    ))
                })
            })
        })
    }

    /// Generic helper function for loading data from a database table.
    #[allow(clippy::unused_self)]
    fn load_from_table<T>(
        &self,
        stable_type_id: StableTypeID,
        fingerprint: u128,
        database: &redb::Database,
        transaction: &RwLock<Option<WriteTransactionWithTable>>,
        deserialize_fn: impl FnOnce(
            &mut BinaryDeserializer<Reader>,
        ) -> Result<T, std::io::Error>,
    ) -> Result<Option<T>, std::io::Error> {
        let mut write_transaction = transaction.upgradable_read();
        Self::ensure_transaction(database, &mut write_transaction)?;

        write_transaction.as_ref().unwrap().with_table(|table| {
            if let Some(buffer) =
                table.get(&(stable_type_id.as_u128(), fingerprint)).unwrap()
            {
                let mut deserializer = BinaryDeserializer::new(Reader::Vec(
                    std::io::Cursor::new(buffer.value().to_vec()),
                ));
                Ok(Some(deserialize_fn(&mut deserializer)?))
            } else {
                Ok(None)
            }
        })
    }

    /// Attempts to load the version information for a given key fingerprint
    /// from the persistence storage.
    pub fn try_load_version_info<K: Key>(
        &self,
        key_fingerprint: u128,
    ) -> Result<Option<VersionInfo>, std::io::Error> {
        self.load_from_table(
            K::STABLE_TYPE_ID,
            key_fingerprint,
            &self.version_info_database,
            &self.version_info_write_transaction,
            |deserializer| {
                (self.deserialize_version_info)(
                    self.serde_extension.as_ref(),
                    deserializer,
                )
            },
        )
    }

    /// Attempts to load a list of dependencies of a given key fingerprint
    pub fn try_load_dependencies<K: Key>(
        &self,
        key_fingerprint: u128,
    ) -> Result<Option<HashSet<DynamicKey>>, std::io::Error> {
        self.load_from_table(
            K::STABLE_TYPE_ID,
            key_fingerprint,
            &self.dependency_graph_database,
            &self.dependency_graph_write_transaction,
            |deserializer| {
                (self.deserialize_dependencies)(
                    self.serde_extension.as_ref(),
                    deserializer,
                )
            },
        )
    }

    /// Attempts to load a value from the persistence storage.
    pub fn try_load_value<K: Key>(
        &self,
        value_fingerprint: u128,
    ) -> Result<Option<K::Value>, std::io::Error> {
        self.load_from_table(
            K::STABLE_TYPE_ID,
            value_fingerprint,
            &self.value_cache_database,
            &self.value_cache_write_transaction,
            |deserializer| {
                let mut result_buffer: Option<K::Value> = None;
                (self.deserialize_any_value)(
                    K::STABLE_TYPE_ID,
                    &mut result_buffer as &mut dyn Any,
                    deserializer,
                    self.serde_extension.as_ref(),
                )?;
                Ok(result_buffer.unwrap())
            },
        )
    }

    /// Generic helper function for saving data to a database table.
    #[allow(clippy::unused_self, clippy::too_many_arguments)]
    fn save_to_table<S>(
        &self,
        stable_type_id: StableTypeID,
        fingerprint: u128,
        database: &redb::Database,
        transaction: &RwLock<Option<WriteTransactionWithTable>>,
        create_serializer: impl FnOnce(Vec<u8>) -> S,
        serialize_fn: impl FnOnce(&mut S) -> Result<(), std::io::Error>,
        extract_buffer: impl FnOnce(S) -> Vec<u8>,
    ) -> Result<(), std::io::Error> {
        let buffer = BUFFER.with(|b| std::mem::take(&mut *b.borrow_mut()));
        let mut binary_serializer = create_serializer(buffer);

        serialize_fn(&mut binary_serializer)?;

        let mut buffer = extract_buffer(binary_serializer);

        let mut tx = transaction.upgradable_read();
        Self::ensure_transaction(database, &mut tx)?;

        tx.with_upgraded(|tx| {
            tx.as_mut().unwrap().with_table_mut(
                |table| -> Result<(), std::io::Error> {
                    table
                        .insert(
                            (stable_type_id.as_u128(), fingerprint),
                            buffer.as_slice(),
                        )
                        .map_err(|e| {
                            std::io::Error::other(format!(
                                "Failed to insert entry into table: {e}",
                            ))
                        })?;
                    Ok(())
                },
            )
        })?;

        BUFFER.with(|b| {
            // clear the buffer but keep the allocated memory
            buffer.clear();
            *b.borrow_mut() = buffer;
        });

        Ok(())
    }

    /// Saves the version information for a given key fingerprint to the
    /// persistence storage.
    #[allow(clippy::mutable_key_type)]
    pub fn save_dependency_graph(
        &self,
        stable_type_id: StableTypeID,
        key_fingerprint: u128,
        dependencies: &HashSet<DynamicKey>,
    ) -> Result<(), std::io::Error> {
        self.save_to_table(
            stable_type_id,
            key_fingerprint,
            &self.dependency_graph_database,
            &self.dependency_graph_write_transaction,
            |buffer| BinarySerializer::new(Writer::Vec(buffer)),
            |serializer| {
                (self.serialize_dependency_graph)(
                    dependencies,
                    serializer,
                    self.serde_extension.as_ref(),
                )
            },
            |s| s.into_inner().into_vec().unwrap(),
        )
    }

    /// Saves the version information for a given key fingerprint to the
    /// persistence storage.
    pub fn save_version_info<K: Key>(
        &self,
        fingerprint: u128,
        version_info: &VersionInfo,
    ) -> Result<(), std::io::Error> {
        self.save_to_table(
            K::STABLE_TYPE_ID,
            fingerprint,
            &self.version_info_database,
            &self.version_info_write_transaction,
            BinarySerializer::new,
            |serializer| version_info.serialize(serializer, &()),
            BinarySerializer::into_inner,
        )
    }

    fn ensure_transaction(
        database: &redb::Database,
        transaction: &mut RwLockUpgradableReadGuard<
            Option<WriteTransactionWithTable>,
        >,
    ) -> Result<(), std::io::Error> {
        if transaction.is_some() {
            return Ok(());
        }

        transaction.with_upgraded(|x| {
            if x.is_some() {
                return Ok(());
            }

            let tx = database.begin_write().map_err(|e| {
                std::io::Error::other(format!(
                    "Failed to begin write transaction: {e}",
                ))
            })?;

            *x = Some(
                WriteTransactionWithTableTryBuilder {
                    write_transaction: tx,
                    table_builder: |tx| {
                        tx.open_table(TABLE).map_err(|e| {
                            std::io::Error::other(format!(
                                "Failed to open table: {e}",
                            ))
                        })
                    },
                }
                .try_build()?,
            );

            Ok(())
        })
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

        self.save_to_table(
            K::STABLE_TYPE_ID,
            fingerprint,
            &self.value_cache_database,
            &self.value_cache_write_transaction,
            |buffer| BinarySerializer::new(Writer::Vec(buffer)),
            |serializer| {
                (self.serialize_any_value)(
                    K::STABLE_TYPE_ID,
                    value as &dyn Any,
                    serializer,
                    self.serde_extension.as_ref(),
                )
            },
            |s| s.into_inner().into_vec().unwrap(),
        )
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

#[allow(clippy::mutable_key_type)]
fn serialize_dependency_graph<
    E: DynamicSerialize<BinarySerializer<Writer>>
        + DynamicDeserialize<BinaryDeserializer<Reader>>
        + Send
        + Sync
        + 'static,
>(
    dependency_graph: &HashSet<DynamicKey>,
    serializer: &mut BinarySerializer<Writer>,
    serde_extension: &dyn Any,
) -> Result<(), std::io::Error> {
    let extension = serde_extension
        .downcast_ref::<E>()
        .expect("serde_extension must match the expected type");

    dependency_graph.serialize(serializer, extension)
}

impl Drop for Persistence {
    fn drop(&mut self) {
        let value_cache_write_transaction =
            self.value_cache_write_transaction.write().take();
        let dependency_graph_write_transaction =
            self.dependency_graph_write_transaction.write().take();
        let version_info_write_transaction =
            self.version_info_write_transaction.write().take();

        [
            value_cache_write_transaction,
            dependency_graph_write_transaction,
            version_info_write_transaction,
        ]
        .into_par_iter()
        .for_each(|tx| {
            if let Some(tx) = tx {
                drop(tx.into_heads());
            }
        });
    }
}

#[cfg(test)]
mod test;

*/
