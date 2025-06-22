//! Defines the protocol for persistence incremental compilation database.

use std::{
    any::Any,
    fs::File,
    io::{BufReader, BufWriter},
    path::PathBuf,
    sync::Arc,
};

use pernixc_serialize::binary::{
    de::BinaryDeserializer, ser::BinarySerializer,
};
use pernixc_stable_hash::{StableHash, StableHasher as _, StableSipHasher};
use pernixc_stable_type_id::StableTypeID;

use crate::{
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
}

impl Persistence {
    /// Test
    pub fn new<
        E: DynamicSerialize<BinarySerializer<BufWriter<File>>>
            + DynamicDeserialize<BinaryDeserializer<BufReader<File>>>
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

            serialize_any_value,
            deserialize_any_value,
        }
    }

    /// Gets a content-adressable path for a given value's key stable type ID
    /// and value fingerprint.
    #[must_use]
    #[allow(clippy::cast_possible_truncation)]
    pub fn get_path(
        &self,
        key_stable_type_id: StableTypeID,
        value_fingerprint: u128,
    ) -> PathBuf {
        let key_stable_type_id = key_stable_type_id.as_u128();
        let (hi, lo) = morton::morton(key_stable_type_id, value_fingerprint);

        // Extract high and low 64-bit parts from the hi component
        let hi_high = (hi >> 64) as u64;
        let hi_low = hi as u64;

        // Encode values as path-safe base64 (URL-safe base64 without padding)
        let hi_high_b64 = Self::encode_path_safe_base64(&hi_high.to_le_bytes());
        let hi_low_b64 = Self::encode_path_safe_base64(&hi_low.to_le_bytes());
        let lo_b64 = Self::encode_path_safe_base64(&lo.to_le_bytes());

        // Construct the three-segment path: {hi_high}/{hi_low}/{lo}.bin
        let mut path = self.path.clone();
        path.push(hi_high_b64);
        path.push(hi_low_b64);
        path.push(format!("{lo_b64}.bin"));
        path
    }

    /// Encodes bytes as path-safe base64 (URL-safe without padding)
    /// Uses '-' instead of '+' and '_' instead of '/' to be filesystem-safe
    /// across all platforms (macOS, Linux, Windows)
    fn encode_path_safe_base64(bytes: &[u8]) -> String {
        // Base64 alphabet for URL-safe encoding
        const ALPHABET: &[u8] =
            b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";

        let mut result = String::new();
        let mut i = 0;

        // Process 3 bytes at a time
        while i + 2 < bytes.len() {
            let b1 = bytes[i];
            let b2 = bytes[i + 1];
            let b3 = bytes[i + 2];

            result.push(ALPHABET[(b1 >> 2) as usize] as char);
            result.push(
                ALPHABET[(((b1 & 0x03) << 4) | (b2 >> 4)) as usize] as char,
            );
            result.push(
                ALPHABET[(((b2 & 0x0f) << 2) | (b3 >> 6)) as usize] as char,
            );
            result.push(ALPHABET[(b3 & 0x3f) as usize] as char);

            i += 3;
        }

        // Handle remaining bytes
        if i < bytes.len() {
            let b1 = bytes[i];
            result.push(ALPHABET[(b1 >> 2) as usize] as char);

            if i + 1 < bytes.len() {
                let b2 = bytes[i + 1];
                result.push(
                    ALPHABET[(((b1 & 0x03) << 4) | (b2 >> 4)) as usize] as char,
                );
                result.push(ALPHABET[((b2 & 0x0f) << 2) as usize] as char);
            } else {
                result.push(ALPHABET[((b1 & 0x03) << 4) as usize] as char);
            }
        }

        result
    }

    /// Attempts to load a value from the persistence storage.
    pub fn try_load<K: Key>(
        &self,
        value_fingerprint: u128,
    ) -> Result<Option<K::Value>, std::io::Error> {
        // path to load the value
        let path = self.get_path(K::STABLE_TYPE_ID, value_fingerprint);

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
        let path = dbg!(self.get_path(K::STABLE_TYPE_ID, {
            let mut hasher = StableSipHasher::new();
            value.stable_hash(&mut hasher);
            hasher.finish()
        }));

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
