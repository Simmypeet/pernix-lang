//! Defines the protocol for persistence incremental compilation database.

use std::{
    any::Any,
    collections::HashSet,
    fs::File,
    io::{BufReader, BufWriter},
    path::{Path, PathBuf},
    sync::Arc,
};

use pernixc_serialize::binary::{
    de::BinaryDeserializer, ser::BinarySerializer,
};
use pernixc_stable_hash::{StableHash, StableHasher as _, StableSipHasher};
use pernixc_stable_type_id::StableTypeID;
use rayon::iter::{IntoParallelRefIterator as _, ParallelIterator};

use crate::{
    database::map::Map,
    runtime::serde::{DynamicDeserialize, DynamicSerialize},
    Key,
};

pub mod morton;

/// Manages the persistence of the incremental compilation database including
/// writing and reading the database to and from a storage path.
#[derive(Debug, Clone)]
#[allow(clippy::type_complexity)]
pub struct Persistence {
    path: PathBuf,
    serde_extension: Arc<dyn Any>,
    skip_keys: HashSet<StableTypeID>,

    serialize_any_value: fn(
        StableTypeID,
        &dyn Any,
        &mut BinarySerializer<BufWriter<File>>,
        &dyn Any,
    ) -> Result<(), std::io::Error>,
    deserialize_any_value: fn(
        StableTypeID,
        &mut dyn Any,
        &mut BinaryDeserializer<BufReader<File>>,
        &dyn Any,
    ) -> Result<(), std::io::Error>,
    serialize_map: fn(
        &Map,
        &HashSet<StableTypeID>,
        &Path,
        &dyn Any,
    ) -> Result<(), std::io::Error>,
}

fn serialize_map<
    E: DynamicSerialize<BinarySerializer<BufWriter<File>>>
        + DynamicDeserialize<BinaryDeserializer<BufReader<File>>>
        + Send
        + Sync
        + 'static,
>(
    map: &Map,
    skip_keys: &HashSet<StableTypeID>,
    base_path: &Path,
    serde_extension: &dyn Any,
) -> Result<(), std::io::Error> {
    let serde_extension = serde_extension
        .downcast_ref::<E>()
        .expect("serde_extension must match the expected type");

    let result = serde_extension
        .serialization_helper_by_type_id()
        .par_iter()
        .map(|a| {
            if skip_keys.contains(a.0) {
                return Ok(());
            }

            let helper = a.1;
            helper.serialize_cas_map(
                map,
                &|stable_type_id, fingerprint| {
                    // Get the path for the value
                    let path_buf = Persistence::get_path(
                        base_path,
                        stable_type_id,
                        fingerprint,
                    );

                    // Create parent directories if they don't exist
                    if let Some(parent) = path_buf.parent() {
                        std::fs::create_dir_all(parent)?;
                    }

                    // Open the file for writing
                    let file = File::create(&path_buf)?;

                    Ok(BinarySerializer::new(BufWriter::new(file)))
                },
                serde_extension,
                *a.0,
            )
        })
        .collect::<Result<Vec<_>, std::io::Error>>()?;

    assert!(
        result.len() == map.type_lens(),
        "not all types were serialized, expected: {}, got: {}",
        map.type_lens(),
        result.len()
    );

    Ok(())
}

impl Persistence {
    /// Creates a new instance of [`Persistence`] with the specified path where
    /// the database is stored and the serde extension where the types that will
    /// be serialized and deserialized are registered.
    pub fn new<
        E: DynamicSerialize<BinarySerializer<BufWriter<File>>>
            + DynamicDeserialize<BinaryDeserializer<BufReader<File>>>
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
             serializer: &mut BinarySerializer<BufWriter<File>>,
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
             deserializer: &mut BinaryDeserializer<BufReader<File>>,
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

        Self {
            path,
            serde_extension: serde_extension as Arc<dyn Any>,
            skip_keys: HashSet::default(),

            serialize_any_value,
            deserialize_any_value,
            serialize_map: serialize_map::<E>,
        }
    }

    /// Serializes thne entire map to the persistence storage.
    pub fn serialize_map(&self, map: &Map) -> Result<(), std::io::Error> {
        (self.serialize_map)(
            map,
            &self.skip_keys,
            &self.path,
            self.serde_extension.as_ref(),
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
        // path to load the value
        let path =
            Self::get_path(&self.path, K::STABLE_TYPE_ID, value_fingerprint);

        // Check if the file exists
        if !path.exists() {
            return Ok(None);
        }

        // Open the file for reading
        let file = File::open(&path)?;
        let mut binary_deserializer =
            BinaryDeserializer::new(BufReader::new(file));

        // Deserialize the value
        let mut result_buffer: Option<K::Value> = None;

        (self.deserialize_any_value)(
            K::STABLE_TYPE_ID,
            &mut result_buffer,
            &mut binary_deserializer,
            self.serde_extension.as_ref(),
        )?;

        Ok(Some(result_buffer.expect("Deserialization should return a value")))
    }

    /// Saves a value to the persistence storage.
    pub fn save<K: Key>(&self, value: &K::Value) -> Result<(), std::io::Error> {
        // path to store the value
        let path = Self::get_path(&self.path, K::STABLE_TYPE_ID, {
            let mut hasher = StableSipHasher::new();
            value.stable_hash(&mut hasher);
            hasher.finish()
        });

        // Create parent directories if they don't exist
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)
                .expect("Failed to create parent directories");
        }

        // Open the file for writing
        let file = File::create(&path)?;
        let mut binary_serializer = BinarySerializer::new(BufWriter::new(file));

        (self.serialize_any_value)(
            K::STABLE_TYPE_ID,
            value as &dyn Any,
            &mut binary_serializer,
            self.serde_extension.as_ref(),
        )
    }
}

#[cfg(test)]
mod test;
