//! Defines the protocol for persistence incremental compilation database.

use std::{
    any::Any,
    borrow::Borrow,
    cell::RefCell,
    io::{BufReader, BufWriter, Cursor},
    path::PathBuf,
    sync::Arc,
};

use enum_as_inner::EnumAsInner;
use ouroboros::self_referencing;
use parking_lot::{MappedRwLockReadGuard, RwLock, RwLockReadGuard};
use pernixc_hash::HashSet;
use pernixc_serialize::{
    binary::{de::BinaryDeserializer, ser::BinarySerializer},
    Deserialize as _, Serialize as _,
};
use pernixc_stable_type_id::StableTypeID;
use redb::{ReadableTable, Result, TableDefinition, WriteTransaction};

use crate::{
    database::ValueMetadata,
    fingerprint,
    runtime::persistence::{
        background::Table,
        serde::{DynamicDeserialize, DynamicSerialize},
    },
    Engine, Key,
};

pub mod serde;

mod background;

struct Database {
    database: redb::Database,
    transaction: RwLock<Option<WriteTransactionWithTable>>,
}

impl Database {
    fn commit(&self) -> Result<(), std::io::Error> {
        let mut write_transaction = self.transaction.write();

        if let Some(transaction) = write_transaction.take() {
            let transaction = transaction.into_heads();
            transaction.write_transaction.commit().map_err(|e| {
                std::io::Error::other(format!(
                    "Failed to commit write transaction: {e}",
                ))
            })?;
        }

        Ok(())
    }

    fn write_transaction(
        &self,
    ) -> Result<
        MappedRwLockReadGuard<'_, WriteTransactionWithTable>,
        std::io::Error,
    > {
        loop {
            let transaction = self.transaction.read();
            if let Ok(value) = RwLockReadGuard::try_map(
                transaction,
                |x: &Option<WriteTransactionWithTable>| x.as_ref(),
            ) {
                return Ok(value);
            }

            let mut with_table = self.transaction.write();
            let write_transaction =
                self.database.begin_write().map_err(|e| {
                    std::io::Error::other(format!(
                        "Failed to begin write transaction: {e}",
                    ))
                })?;

            if with_table.as_mut().is_none() {
                with_table.replace(
                    WriteTransactionWithTableTryBuilder {
                        write_transaction,
                        cache_table_builder: |tx| {
                            tx.open_table(VALUE_CACHE)
                                .map_err(|e| {
                                    std::io::Error::other(format!(
                                        "Failed to open value cache table: {e}",
                                    ))
                                })
                                .map(RwLock::new)
                        },

                        metadata_table_builder: |tx| {
                            tx.open_table(VALUE_METADATA)
                                .map_err(|e| {
                                    std::io::Error::other(format!(
                                        "Failed to open value metadata table: \
                                         {e}",
                                    ))
                                })
                                .map(RwLock::new)
                        },
                    }
                    .try_build()?,
                );
            }
        }
    }
}

#[self_referencing]
struct WriteTransactionWithTable {
    write_transaction: WriteTransaction,

    #[borrows(write_transaction)]
    #[not_covariant]
    cache_table: RwLock<redb::Table<'this, (u128, u128), &'static [u8]>>,

    #[borrows(write_transaction)]
    #[not_covariant]
    metadata_table: RwLock<redb::Table<'this, (u128, u128), &'static [u8]>>,
}

impl Drop for Database {
    fn drop(&mut self) { self.transaction.write().take(); }
}

impl Engine {
    pub(crate) fn try_load_value<K: Key>(
        &self,
        value_fingerprint: u128,
    ) -> Option<K::Value> {
        let persistence = self.runtime.persistence.as_ref()?;

        match persistence.load_value::<K>(value_fingerprint) {
            Ok(value) => {
                tracing::debug!(
                    "Loaded value for key {} with value fingerprint {}",
                    std::any::type_name::<K>(),
                    value_fingerprint,
                );

                value
            }
            Err(error) => {
                tracing::error!(
                    "Failed to load value for key {} with fingerprint {}: \
                     {error}",
                    std::any::type_name::<K>(),
                    value_fingerprint,
                );
                None
            }
        }
    }

    pub(crate) fn try_load_value_metadata<K: Key>(
        &self,
        key_fingerprint: u128,
    ) -> Option<ValueMetadata> {
        let persistence = self.runtime.persistence.as_ref()?;

        match persistence.load_value_metadata::<K>(key_fingerprint) {
            Ok(value_metadata) => {
                tracing::debug!(
                    "Loaded metadata for key {} with key fingerprint {} got \
                     {value_metadata:?}",
                    std::any::type_name::<K>(),
                    key_fingerprint,
                );

                value_metadata
            }
            Err(error) => {
                tracing::error!(
                    "Failed to load value metadata for key {} with \
                     fingerprint {}: {error}",
                    std::any::type_name::<K>(),
                    key_fingerprint,
                );
                None
            }
        }
    }

    pub(crate) fn save_value_metadata<K: Key>(
        &self,
        key_fingerprint: u128,
        value_metadata: ValueMetadata,
    ) {
        let Some(persistence) = self.runtime.persistence.as_ref() else {
            return;
        };

        persistence.save_value_metadata::<K>(key_fingerprint, value_metadata);
    }

    pub(crate) fn save_value<K: Key>(
        &self,
        fingerprint: Option<u128>,
        value: K::Value,
    ) {
        let Some(persistence) = self.runtime.persistence.as_ref() else {
            return;
        };

        persistence.save_value::<K>(fingerprint, value);
    }
}

/// Table definition for the query value cache. The key is a tuple of
/// `STABLE_TYPE_ID` of the key and fingerprint of the value.
const VALUE_CACHE: TableDefinition<(u128, u128), &[u8]> =
    TableDefinition::new("value_cache");

const VALUE_METADATA: TableDefinition<(u128, u128), &[u8]> =
    TableDefinition::new("value_metadata");

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

/// Manages the persistence of the incremental compilation database including
/// writing and reading the database to and from a storage path.
pub struct Persistence {
    database: Arc<Database>,
    background_writer: RwLock<Option<background::Writer>>,

    skip_keys: HashSet<StableTypeID>,
    directory_path: PathBuf,

    serialize_value_metadata: fn(
        &mut BinarySerializer<Writer>,
        &ValueMetadata,
        &dyn Any,
    ) -> Result<(), std::io::Error>,

    #[allow(clippy::type_complexity)]
    serialize_dynamic_value: fn(
        &mut BinarySerializer<Writer>,
        StableTypeID,
        &'static str,
        &dyn Any,
        &dyn Any,
    ) -> Result<(), std::io::Error>,

    deserialize_value_metadata: fn(
        &mut BinaryDeserializer<Reader>,
        &dyn Any,
    )
        -> Result<ValueMetadata, std::io::Error>,

    #[allow(clippy::type_complexity)]
    deserialize_dynamic_value: fn(
        &mut dyn Any,
        &mut BinaryDeserializer<Reader>,
        StableTypeID,
        &'static str,
        &dyn Any,
    ) -> Result<(), std::io::Error>,

    serde_extension: Arc<dyn std::any::Any + Send + Sync>,
}

impl Drop for Persistence {
    fn drop(&mut self) { self.database.transaction.write().take(); }
}

impl std::fmt::Debug for Persistence {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Persistence")
            .field("database", &self.database.database)
            .finish_non_exhaustive()
    }
}

fn serialize_value_metadata<
    E: DynamicSerialize<BinarySerializer<Writer>> + 'static,
>(
    serializer: &mut BinarySerializer<Writer>,
    value_metdata: &ValueMetadata,
    extension: &dyn Any,
) -> Result<(), std::io::Error> {
    let extension = extension
        .downcast_ref::<E>()
        .expect("serde_extension must match the expected type");

    value_metdata.serialize(serializer, extension)
}

fn serialize_dynamic_value<
    E: DynamicSerialize<BinarySerializer<Writer>> + 'static,
>(
    serializer: &mut BinarySerializer<Writer>,
    key_stable_type_id: StableTypeID,
    value_type_name: &'static str,
    value: &dyn Any,
    extension: &dyn Any,
) -> Result<(), std::io::Error> {
    let extension = extension
        .downcast_ref::<E>()
        .expect("serde_extension must match the expected type");

    let helper = extension
        .serialization_helper_by_type_id()
        .get(&key_stable_type_id)
        .unwrap_or_else(|| {
            panic!(
                "No serialization helper found for type value: \
                 {value_type_name}",
            )
        });

    (helper.value_serializer())(value, serializer, extension)
}

fn deserialize_value_metadata<
    E: DynamicDeserialize<BinaryDeserializer<Reader>> + 'static,
>(
    deserializer: &mut BinaryDeserializer<Reader>,
    extension: &dyn Any,
) -> Result<ValueMetadata, std::io::Error> {
    let extension = extension
        .downcast_ref::<E>()
        .expect("serde_extension must match the expected type");

    ValueMetadata::deserialize(deserializer, extension)
}

fn deserialize_dynamic_value<
    E: DynamicDeserialize<BinaryDeserializer<Reader>> + 'static,
>(
    buffer: &mut dyn Any,
    deserializer: &mut BinaryDeserializer<Reader>,
    key_stable_type_id: StableTypeID,
    value_type_name: &'static str,
    extension: &dyn Any,
) -> Result<(), std::io::Error> {
    let extension = extension
        .downcast_ref::<E>()
        .expect("serde_extension must match the expected type");

    let helper = extension
        .deserialization_helper_by_type_id()
        .get(&key_stable_type_id)
        .unwrap_or_else(|| {
            panic!(
                "No deserialization helper found for type value: \
                 {value_type_name}",
            )
        });

    (helper.value_deserializer())(buffer, deserializer, extension)
}

thread_local! {
    static BUFFER: RefCell<Vec<u8>> = const { RefCell::new(Vec::new()) };
}

impl Persistence {
    const DATABASE_FILE: &'static str = "persistence.db";
    const VERSION_SNAPSHOT_FILE: &'static str = "version_snapshot.db";

    /// Creates a new instance of [`Persistence`] with the specified path where
    /// the database is stored and the serde extension where the types that will
    /// be serialized and deserialized are registered.
    pub fn new<
        E: DynamicSerialize<BinarySerializer<Writer>>
            + DynamicDeserialize<BinaryDeserializer<Reader>>
            + Any
            + Send
            + Sync
            + 'static,
    >(
        directory_path: PathBuf,
        serde_extension: Arc<E>,
    ) -> Result<Self, std::io::Error> {
        // ensure the path is directory
        if directory_path.exists() {
            if !directory_path.is_dir() {
                return Err(std::io::Error::other(format!(
                    "Path {} is not a directory",
                    directory_path.display(),
                )));
            }
        }
        // ensure the folder exists
        else {
            std::fs::create_dir_all(&directory_path)?;
        }

        let database_path = directory_path.join(Self::DATABASE_FILE);

        Ok(Self {
            database: Arc::new(Database {
                database: redb::Database::create(&database_path).map_err(
                    |e| {
                        std::io::Error::other(format!(
                            "Failed to create database at path {}: {e}",
                            database_path.display()
                        ))
                    },
                )?,
                transaction: RwLock::new(None),
            }),
            background_writer: RwLock::new(None),

            skip_keys: HashSet::default(),

            directory_path,

            serialize_value_metadata: serialize_value_metadata::<E>,
            serialize_dynamic_value: serialize_dynamic_value::<E>,
            deserialize_value_metadata: deserialize_value_metadata::<E>,
            deserialize_dynamic_value: deserialize_dynamic_value::<E>,
            serde_extension,
        })
    }

    /// Instructs the persistence system to skip saving the value cache for the
    /// given query key type.
    pub fn skip_cache_value<K: Key>(&mut self) {
        self.skip_keys.insert(K::STABLE_TYPE_ID);
    }

    fn load_from_table<K: redb::Key, Q: for<'k> Borrow<K::SelfType<'k>>, V>(
        table: &redb::Table<'_, K, &[u8]>,
        key: Q,
        deserialize: impl FnOnce(
            &mut BinaryDeserializer<Reader>,
        ) -> Result<V, std::io::Error>,
    ) -> Result<Option<V>, std::io::Error> {
        let value = if let Some(value_access) = table.get(key).map_err(|e| {
            std::io::Error::other(format!(
                "Failed to get value from table: {e}",
            ))
        })? {
            let mut buffer =
                BUFFER.with(|b| std::mem::take(&mut *b.borrow_mut()));

            buffer.clear();
            buffer.extend_from_slice(value_access.value());

            let reader = Reader::Vec(Cursor::new(buffer));
            let mut deserializer = BinaryDeserializer::new(reader);

            let value = deserialize(&mut deserializer)?;

            let mut buffer =
                deserializer.into_inner().into_vec().unwrap().into_inner();

            BUFFER.with(|b| {
                // clear the buffer but keep the allocated memory
                buffer.clear();

                *b.borrow_mut() = buffer;
            });

            Ok(Some(value))
        } else {
            Ok(None)
        };

        value
    }

    #[allow(unused)]
    fn save_to_table(
        serialize: impl FnOnce(
            &mut BinarySerializer<Writer>,
        ) -> Result<(), std::io::Error>,
        save: impl FnOnce(&[u8]) -> Result<(), redb::Error>,
    ) -> Result<(), std::io::Error> {
        let buffer = BUFFER.with(|b| std::mem::take(&mut *b.borrow_mut()));

        let mut serializer = BinarySerializer::new(Writer::Vec(buffer));

        serialize(&mut serializer)?;

        let mut buffer = serializer.into_inner().into_vec().unwrap();

        save(&buffer).map_err(|x| {
            std::io::Error::other(format!("Failed to save value to table: {x}"))
        })?;

        BUFFER.with(|b| {
            // clear the buffer but keep the allocated memory
            buffer.clear();

            *b.borrow_mut() = buffer;
        });

        Ok(())
    }

    fn save_value<K: Key>(
        &self,
        value_fingerprint: Option<u128>,
        value: K::Value,
    ) {
        // skip saving if the key is wPin the skip list
        if self.skip_keys.contains(&K::STABLE_TYPE_ID) {
            return;
        }

        let serializer_fn = self.serialize_dynamic_value;
        let serde_extension = self.serde_extension.clone();

        self.ensure_background_writer().new_serialize_task(
            move |mut buffer| {
                let mut serializer = BinarySerializer::new(Writer::Vec(buffer));

                // if the fingerprint is not provided, calculate it
                let fingerprint = value_fingerprint
                    .unwrap_or_else(|| fingerprint::fingerprint(&value));

                let result = (serializer_fn)(
                    &mut serializer,
                    K::STABLE_TYPE_ID,
                    std::any::type_name::<K::Value>(),
                    &value,
                    serde_extension.as_ref(),
                );

                buffer = serializer.into_inner().into_vec().unwrap();

                match result {
                    Ok(()) => {}
                    Err(err) => {
                        tracing::error!(
                            "Failed to serialize value for key {} with \
                             fingerprint {}: {err}",
                            std::any::type_name::<K>(),
                            fingerprint,
                        );

                        // return the buffer to the pool
                        return Err(buffer);
                    }
                }

                Ok(background::SerializeResult {
                    table: Table::Value,
                    stable_type_id: K::STABLE_TYPE_ID,
                    fingerprint,
                    buffer,
                })
            },
        );
    }

    fn save_value_metadata<K: Key>(
        &self,
        key_fingerprint: u128,
        value_metadata: ValueMetadata,
    ) {
        let serialize_value_metadata = self.serialize_value_metadata;
        let serde_extension = self.serde_extension.clone();

        self.ensure_background_writer().new_serialize_task(
            move |mut buffer| {
                let mut serializer = BinarySerializer::new(Writer::Vec(buffer));

                let result = (serialize_value_metadata)(
                    &mut serializer,
                    &value_metadata,
                    serde_extension.as_ref(),
                );

                buffer = serializer.into_inner().into_vec().unwrap();

                match result {
                    Ok(()) => {}
                    Err(err) => {
                        tracing::error!(
                            "Failed to serialize value metadata for key {} \
                             with fingerprint {}: {err}",
                            std::any::type_name::<K>(),
                            key_fingerprint,
                        );

                        // return the buffer to the pool
                        return Err(buffer);
                    }
                }

                Ok(background::SerializeResult {
                    table: Table::Metadata,
                    stable_type_id: K::STABLE_TYPE_ID,
                    fingerprint: key_fingerprint,
                    buffer,
                })
            },
        );
    }

    fn load_value_metadata<K: Key>(
        &self,
        key_fingerprint: u128,
    ) -> Result<Option<ValueMetadata>, std::io::Error> {
        let tx = self.database.write_transaction()?;

        tx.with_metadata_table(|x| {
            Self::load_from_table(
                &x.read(),
                (K::STABLE_TYPE_ID.as_u128(), key_fingerprint),
                |deserializer| {
                    (self.deserialize_value_metadata)(
                        deserializer,
                        self.serde_extension.as_ref(),
                    )
                },
            )
        })
    }

    fn load_value<K: Key>(
        &self,
        value_fingerprint: u128,
    ) -> Result<Option<K::Value>, std::io::Error> {
        let type_name = std::any::type_name::<K>();
        let _tracing =
            tracing::info_span!("Loading value", type_name, value_fingerprint,)
                .entered();

        if self.skip_keys.contains(&K::STABLE_TYPE_ID) {
            return Ok(None);
        }

        let tx = self.database.write_transaction()?;

        tx.with_cache_table(|x| {
            Self::load_from_table(
                &x.read(),
                (K::STABLE_TYPE_ID.as_u128(), value_fingerprint),
                |deserializer| {
                    let mut buffer: Option<K::Value> = None;

                    (self.deserialize_dynamic_value)(
                        &mut buffer,
                        deserializer,
                        K::STABLE_TYPE_ID,
                        std::any::type_name::<K::Value>(),
                        self.serde_extension.as_ref(),
                    )?;

                    Ok(buffer.unwrap())
                },
            )
        })
    }

    fn ensure_background_writer(
        &self,
    ) -> MappedRwLockReadGuard<'_, background::Writer> {
        loop {
            let background_writer = self.background_writer.read();
            if let Ok(value) = RwLockReadGuard::try_map(
                background_writer,
                |x: &Option<background::Writer>| x.as_ref(),
            ) {
                return value;
            }

            let mut with_writer = self.background_writer.write();
            if with_writer.as_mut().is_none() {
                with_writer.replace(background::Writer::new(
                    std::thread::available_parallelism()
                        .map_or_else(|_| 4, std::num::NonZero::get),
                    self.database.clone(),
                ));
            }
        }
    }

    fn commit(&mut self) -> Result<(), std::io::Error> {
        // drop the old background writer that makes sure that all the tasks
        // are completed
        self.background_writer.write().take();

        // commit the database
        self.database.commit()?;

        tracing::info!("Persistence database committed successfully");

        Ok(())
    }
}

impl Engine {
    /// Saves the query database to the persistent storage if the persistence
    /// is enabled.
    pub fn save_database(&mut self) -> Result<(), std::io::Error> {
        // obtain the persistence
        let Some(persistence) = self.runtime.persistence.as_mut() else {
            return Ok(());
        };

        let file = std::fs::File::create(
            persistence.directory_path.join(Persistence::VERSION_SNAPSHOT_FILE),
        )?;

        persistence.commit()?;

        let version = self.version();

        let mut serializer = BinarySerializer::new(BufWriter::new(file));
        version.serialize(&mut serializer, &())?;

        Ok(())
    }
}

impl Persistence {
    /// Loads the query database from the persistent storage.
    pub fn load_database(
        &self,
    ) -> Result<crate::database::Database, std::io::Error> {
        let snapshot_file_path =
            self.directory_path.join(Self::VERSION_SNAPSHOT_FILE);

        let version = if snapshot_file_path.exists() {
            let file = std::fs::File::open(&snapshot_file_path)?;

            let reader = BufReader::new(file);
            let mut deserializer =
                BinaryDeserializer::new(Reader::File(reader));

            u64::deserialize(&mut deserializer, &())?
        } else {
            0
        };

        Ok(crate::database::Database::with_version(version))
    }
}
