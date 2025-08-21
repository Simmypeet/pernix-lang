//! Defines the protocol for persistence incremental compilation database.

use std::{
    any::Any,
    cell::RefCell,
    fmt::Debug,
    io::{BufReader, BufWriter, Cursor},
    path::PathBuf,
    sync::Arc,
};

use enum_as_inner::EnumAsInner;
use parking_lot::{MappedRwLockReadGuard, RwLock, RwLockReadGuard};
use pernixc_hash::HashSet;
use pernixc_serialize::{
    binary::{de::BinaryDeserializer, ser::BinarySerializer},
    Deserialize as _, Serialize as _,
};
use pernixc_stable_type_id::StableTypeID;
use rand::Rng;
use redb::Result;
use tracing::instrument;

use crate::{
    database::ValueMetadata,
    runtime::persistence::{
        backend::{Backend, Table},
        background::SaveTask,
        serde::{DynamicDeserialize, DynamicSerialize},
    },
    Engine, Key,
};

pub mod serde;

mod backend;
mod background;

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
        fingerprint: u128,
        value: K::Value,
    ) {
        let Some(persistence) = self.runtime.persistence.as_ref() else {
            return;
        };

        persistence.save_value::<K>(fingerprint, value);
    }
}

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
pub struct Persistence<B = backend::redb::RedbBackend> {
    database: B,

    background_writer: RwLock<Option<background::Worker>>,

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

impl<B: Debug> std::fmt::Debug for Persistence<B> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Persistence")
            .field("database", &self.database)
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

const VERSION_SNAPSHOT_FILE: &str = "version_snapshot.db";

impl<B: Backend> Persistence<B> {
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
        Ok(Self {
            database: B::create(&directory_path)?,

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

    fn load_from_table<K: Key, V>(
        &self,
        table: backend::Table,
        key: u128,
        deserialize: impl FnOnce(
            &mut BinaryDeserializer<Reader>,
        ) -> Result<V, std::io::Error>,
    ) -> Result<Option<V>, std::io::Error> {
        let mut buffer = BUFFER.with(|b| std::mem::take(&mut *b.borrow_mut()));
        buffer.clear();

        let result = match self.database.read(
            table,
            (K::STABLE_TYPE_ID.as_u128(), key),
            &mut buffer,
        ) {
            Ok(true) => {
                let reader = Reader::Vec(Cursor::new(buffer));
                let mut deserializer = BinaryDeserializer::new(reader);

                let value = deserialize(&mut deserializer)?;

                buffer =
                    deserializer.into_inner().into_vec().unwrap().into_inner();

                Ok(Some(value))
            }

            Ok(false) => Ok(None),

            Err(err) => Err(err),
        };

        BUFFER.with(|b| {
            // clear the buffer but keep the allocated memory
            buffer.clear();

            *b.borrow_mut() = buffer;
        });

        result
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

    fn save_value<K: Key>(&self, value_fingerprint: u128, value: K::Value) {
        // skip saving if the key is wPin the skip list
        if self.skip_keys.contains(&K::STABLE_TYPE_ID) {
            return;
        }

        let serializer_fn = self.serialize_dynamic_value;
        let serde_extension = self.serde_extension.clone();

        self.ensure_background_writer().new_save_task(SaveTask {
            key: (K::STABLE_TYPE_ID.as_u128(), value_fingerprint),
            table: Table::ValueCache,
            write: Box::new(move |buffer| {
                let _span = tracing::info_span!(
                    "save_value",
                    type_name = std::any::type_name::<K>(),
                    value_fingerprint = value_fingerprint,
                )
                .entered();
                println!("buffer: {buffer:?}");

                let mut serializer =
                    BinarySerializer::new(Writer::Vec(std::mem::take(buffer)));

                // if the fingerprint is not provided, calculate it

                let result = (serializer_fn)(
                    &mut serializer,
                    K::STABLE_TYPE_ID,
                    std::any::type_name::<K::Value>(),
                    &value,
                    serde_extension.as_ref(),
                );

                *buffer = serializer.into_inner().into_vec().unwrap();

                match result {
                    Ok(()) => {}
                    Err(err) => {
                        tracing::error!(
                            "Failed to serialize value for key {} with \
                             fingerprint {}: {err}",
                            std::any::type_name::<K>(),
                            value_fingerprint,
                        );

                        // return the buffer to the pool
                        return false;
                    }
                }

                true
            }),
        });
    }

    fn save_value_metadata<K: Key>(
        &self,
        key_fingerprint: u128,
        value_metadata: ValueMetadata,
    ) {
        let serialize_value_metadata = self.serialize_value_metadata;
        let serde_extension = self.serde_extension.clone();

        self.ensure_background_writer().new_save_task(SaveTask {
            key: (K::STABLE_TYPE_ID.as_u128(), key_fingerprint),
            table: Table::ValueMetadata,
            write: Box::new(move |buffer| {
                let _span = tracing::info_span!(
                    "save_value_metadata",
                    type_name = std::any::type_name::<K>(),
                    key_fingerprint,
                )
                .entered();

                println!("buffer: {buffer:?}");
                let mut serializer =
                    BinarySerializer::new(Writer::Vec(std::mem::take(buffer)));

                let result = (serialize_value_metadata)(
                    &mut serializer,
                    &value_metadata,
                    serde_extension.as_ref(),
                );

                *buffer = serializer.into_inner().into_vec().unwrap();

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
                        return false;
                    }
                }

                true
            }),
        });
    }

    #[instrument(
        fields(
            key_name = std::any::type_name::<K>(),
            key_fingerprint = key_fingerprint,
        ),
        level = "info",
        skip_all
    )]
    fn load_value_metadata<K: Key>(
        &self,
        key_fingerprint: u128,
    ) -> Result<Option<ValueMetadata>, std::io::Error> {
        self.load_from_table::<K, ValueMetadata>(
            backend::Table::ValueCache,
            key_fingerprint,
            |deserializer| {
                (self.deserialize_value_metadata)(
                    deserializer,
                    self.serde_extension.as_ref(),
                )
            },
        )
    }

    #[instrument(
        fields(
            key_name = std::any::type_name::<K>(),
            value_fingerprint = value_fingerprint,
        ),
        level = "info",
        skip_all
    )]
    fn load_value<K: Key>(
        &self,
        value_fingerprint: u128,
    ) -> Result<Option<K::Value>, std::io::Error> {
        if self.skip_keys.contains(&K::STABLE_TYPE_ID) {
            return Ok(None);
        }

        self.load_from_table::<K, K::Value>(
            backend::Table::ValueCache,
            value_fingerprint,
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
    }

    fn ensure_background_writer(
        &self,
    ) -> MappedRwLockReadGuard<'_, background::Worker> {
        loop {
            let background_writer = self.background_writer.read();
            if let Ok(value) = RwLockReadGuard::try_map(
                background_writer,
                |x: &Option<background::Worker>| x.as_ref(),
            ) {
                return value;
            }

            let mut with_writer = self.background_writer.write();
            if with_writer.as_mut().is_none() {
                with_writer.replace(background::Worker::new(
                    2,
                    self.database.background_writer(),
                ));
            }
        }
    }

    fn commit(&mut self) {
        // drop the old background writer that makes sure that all the tasks
        // are completed
        let _span =
            tracing::info_span!("commit persistence database").entered();

        self.background_writer.write().take();
        self.database
            .refresh_read()
            .expect("Failed to refresh read transaction");
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
            persistence.directory_path.join(VERSION_SNAPSHOT_FILE),
        )?;

        persistence.commit();

        let state = (self.database.random_seed(), self.version());

        let mut serializer = BinarySerializer::new(BufWriter::new(file));
        state.serialize(&mut serializer, &())?;

        Ok(())
    }
}

impl Persistence {
    /// Loads the query database from the persistent storage.
    pub fn load_database(
        &self,
    ) -> Result<crate::database::Database, std::io::Error> {
        let snapshot_file_path =
            self.directory_path.join(VERSION_SNAPSHOT_FILE);

        let (random_seed, version) = if snapshot_file_path.exists() {
            let file = std::fs::File::open(&snapshot_file_path)?;

            let reader = BufReader::new(file);
            let mut deserializer =
                BinaryDeserializer::new(Reader::File(reader));

            <(u64, u64)>::deserialize(&mut deserializer, &())?
        } else {
            (rand::thread_rng().gen(), 0)
        };

        Ok(crate::database::Database::with_state(random_seed, version))
    }
}

#[cfg(test)]
mod test;
