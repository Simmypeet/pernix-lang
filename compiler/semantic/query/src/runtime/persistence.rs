//! Defines the protocol for persistence incremental compilation database.

use std::{
    self,
    any::Any,
    cell::RefCell,
    collections::HashSet,
    path::{Path, PathBuf},
    sync::Arc,
};

use dashmap::mapref::one::Ref;
use parking_lot::RwLock;
use pernixc_hash::DashMap;
use pernixc_serialize::binary::{
    de::BinaryDeserializer, ser::BinarySerializer,
};
use pernixc_stable_type_id::StableTypeID;
use rayon::iter::{IntoParallelRefIterator as _, ParallelIterator};
use redb::TableDefinition;

use crate::{
    database::map::Map,
    fingerprint,
    runtime::serde::{DynamicDeserialize, DynamicSerialize},
    Key,
};

pub mod morton;

const TABLE: TableDefinition<u128, &[u8]> = TableDefinition::new("persistence");

/// Manages the persistence of the incremental compilation database including
/// writing and reading the database to and from a storage path.
#[derive(Debug)]
#[allow(clippy::type_complexity)]
pub struct Persistence {
    databases_by_stable_type_id: DashMap<StableTypeID, redb::Database>,
    path: PathBuf,
    serde_extension: Arc<dyn Any + Send + Sync>,
    skip_keys: HashSet<StableTypeID>,

    serialize_any_value: fn(
        StableTypeID,
        &dyn Any,
        &mut BinarySerializer<Vec<u8>>,
        &dyn Any,
    ) -> Result<(), std::io::Error>,
    deserialize_any_value: fn(
        StableTypeID,
        &mut dyn Any,
        &mut BinaryDeserializer<std::io::Cursor<Vec<u8>>>,
        &dyn Any,
    ) -> Result<(), std::io::Error>,
    serialize_map: fn(
        &Map,
        &HashSet<StableTypeID>,
        &dyn Any,
        &DashMap<StableTypeID, redb::Database>,
        &Path,
    ) -> Result<(), std::io::Error>,
}

fn serialize_map<
    E: DynamicSerialize<BinarySerializer<Vec<u8>>>
        + DynamicDeserialize<BinaryDeserializer<std::io::Cursor<Vec<u8>>>>
        + Send
        + Sync
        + 'static,
>(
    map: &Map,
    skip_keys: &HashSet<StableTypeID>,
    serde_extension: &dyn Any,
    databases_by_stable_type_id: &DashMap<StableTypeID, redb::Database>,
    path: &Path,
) -> Result<(), std::io::Error> {
    let serde_extension = serde_extension
        .downcast_ref::<E>()
        .expect("serde_extension must match the expected type");

    let result = serde_extension
        .serialization_helper_by_type_id()
        .par_iter()
        .map(|a| {
            if skip_keys.contains(a.0) {
                return Ok(true);
            }

            let stable_type_id = *a.0;
            let helper = a.1;

            let db = Persistence::get_databse(
                databases_by_stable_type_id,
                stable_type_id,
                path,
            );
            let mut tx =
                db.begin_write().expect("Failed to begin write transaction");

            tx.set_durability(redb::Durability::None);

            let table = RwLock::new(
                tx.open_table(TABLE).expect("Failed to open table"),
            );

            let result = helper.serialize_fingerprint_map(
                map,
                &|| {
                    let buffer =
                        BUFFER.with(|b| std::mem::take(&mut *b.borrow_mut()));

                    Ok(BinarySerializer::new(buffer))
                },
                serde_extension,
                &|serializer, fingerprint| {
                    let mut buffer = serializer.into_inner();
                    table
                        .write()
                        .insert(fingerprint, buffer.as_slice())
                        .unwrap();

                    BUFFER.with(|b| {
                        // clear the buffer but keep the allocated memory
                        buffer.clear();
                        *b.borrow_mut() = buffer;
                    });

                    Ok(())
                },
            );

            drop(table);

            tx.commit().unwrap();

            result
        })
        .collect::<Result<Vec<_>, std::io::Error>>()?;

    let scanned_count = result.into_iter().filter(|x| *x).count();

    assert!(
        scanned_count >= map.type_lens(),
        "not all types were serialized, expected: {}, got: {}",
        scanned_count,
        map.type_lens(),
    );

    Ok(())
}

thread_local! {
    static BUFFER: RefCell<Vec<u8>> = const { RefCell::new(Vec::new()) };
}

impl Persistence {
    /// Creates a new instance of [`Persistence`] with the specified path where
    /// the database is stored and the serde extension where the types that will
    /// be serialized and deserialized are registered.
    pub fn new<
        E: DynamicSerialize<BinarySerializer<Vec<u8>>>
            + DynamicDeserialize<BinaryDeserializer<std::io::Cursor<Vec<u8>>>>
            + Send
            + Sync
            + 'static,
    >(
        path: PathBuf,
        serde_extension: Arc<E>,
    ) -> Self {
        let serialize_any_value =
            |stable_type_id: StableTypeID,
             any_value: &dyn Any,
             serializer: &mut BinarySerializer<Vec<u8>>,
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
             deserializer: &mut BinaryDeserializer<
                std::io::Cursor<Vec<u8>>,
            >,
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

        if !path.exists() {
            std::fs::create_dir_all(&path)
                .expect("Failed to create persistence directory");
        }

        Self {
            path,
            databases_by_stable_type_id: DashMap::default(),
            serde_extension: serde_extension as Arc<dyn Any + Send + Sync>,
            skip_keys: HashSet::default(),

            serialize_any_value,
            deserialize_any_value,
            serialize_map: serialize_map::<E>,
        }
    }

    /// Registers a key to be skipped during serialization.
    pub fn register_skip_key<K: Key>(&mut self) {
        self.skip_keys.insert(K::STABLE_TYPE_ID);
    }

    /// Serializes thne entire map to the persistence storage.
    pub fn serialize_map(&self, map: &Map) -> Result<(), std::io::Error> {
        (self.serialize_map)(
            map,
            &self.skip_keys,
            self.serde_extension.as_ref(),
            &self.databases_by_stable_type_id,
            &self.path,
        )
    }

    /// Gets a content-adressable path for a given value's key stable type ID
    /// and value fingerprint.
    #[must_use]
    #[allow(clippy::cast_possible_truncation)]
    pub fn get_path(
        base_path: &Path,
        key_stable_type_id: StableTypeID,
        value_fingerprint: u128,
    ) -> PathBuf {
        let key_stable_type_id = key_stable_type_id.as_u128();
        let (hi, lo) = morton::morton(key_stable_type_id, value_fingerprint);

        // Combine hi and lo into a 256-bit value for bit extraction
        // hi (128 bits) | lo (128 bits) = 256 bits total

        // Extract first 8 bits from hi (upper 8 bits of hi)
        let segment1 = (hi >> 120) as u8;

        // Extract next 8 bits from hi (bits 112-119 of hi) - properly mask to
        // get only 8 bits
        let segment2 = ((hi >> 112) & 0xFF) as u8;

        // Extract remaining 240 bits (lower 112 bits of hi + all 128 bits of
        // lo) This creates exactly 240 bits:
        // [hi_lower_112bits][lo_128bits]
        let hi_lower = hi & 0x0000_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF; // Lower 112 bits of hi

        // Construct the three-segment path: {8bits}/{8bits}/{240bits}.bin
        let segment1_hex = format!("{segment1:02x}");
        let segment2_hex = format!("{segment2:02x}");
        let segment3_hex = format!("{hi_lower:028x}{lo:032x}"); // 112bits + 128bits = 240bits exactly

        let mut path = base_path.to_path_buf();
        path.push(segment1_hex);
        path.push(segment2_hex);
        path.push(format!("{segment3_hex}.bin"));
        path
    }

    /// Attempts to load a value from the persistence storage.
    pub fn try_load<K: Key>(
        &self,
        value_fingerprint: u128,
    ) -> Result<Option<K::Value>, std::io::Error> {
        let table = Self::get_databse(
            &self.databases_by_stable_type_id,
            K::STABLE_TYPE_ID,
            &self.path,
        )
        .begin_read()
        .unwrap();
        let table = table.open_table(TABLE).unwrap();

        if let Some(buffer) = table.get(&value_fingerprint).unwrap() {
            let mut deserializer = BinaryDeserializer::new(
                std::io::Cursor::new(buffer.value().to_vec()),
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

    fn get_databse<'a>(
        databases_by_stable_type_id: &'a DashMap<StableTypeID, redb::Database>,
        stable_type_id: StableTypeID,
        path: &Path,
    ) -> Ref<'a, StableTypeID, redb::Database> {
        if let Some(database) = databases_by_stable_type_id.get(&stable_type_id)
        {
            return database;
        }

        let stable_type_id_u128 = stable_type_id.as_u128();
        // format stable_type_id as a hex string
        let path = path.join(format!("{stable_type_id_u128:032x}.pernixc"));
        let database = redb::Database::create(&path).unwrap();
        databases_by_stable_type_id.insert(stable_type_id, database);

        databases_by_stable_type_id
            .get(&stable_type_id)
            .expect("Database should be inserted")
    }

    /// Saves a value to the persistence storage.
    pub fn save<K: Key>(&self, value: &K::Value) -> Result<(), std::io::Error> {
        let fingerprint = fingerprint::fingerprint(value);
        let buffer = BUFFER.with(|b| std::mem::take(&mut *b.borrow_mut()));
        let mut binary_serializer = BinarySerializer::new(buffer);

        (self.serialize_any_value)(
            K::STABLE_TYPE_ID,
            value as &dyn Any,
            &mut binary_serializer,
            self.serde_extension.as_ref(),
        )?;

        let mut buffer = binary_serializer.into_inner();
        let tx = Self::get_databse(
            &self.databases_by_stable_type_id,
            K::STABLE_TYPE_ID,
            &self.path,
        )
        .begin_write()
        .unwrap();

        {
            let mut table = tx.open_table(TABLE).unwrap();
            table.insert(fingerprint, buffer.as_slice()).unwrap();
        }

        tx.commit().unwrap();

        BUFFER.with(|b| {
            // clear the buffer but keep the allocated memory
            buffer.clear();
            *b.borrow_mut() = buffer;
        });

        Ok(())
    }
}

#[cfg(test)]
mod test;
