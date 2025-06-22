//! Defines the protocol for persistence incremental compilation database.

use std::{any::Any, fs::File, io::BufWriter, path::PathBuf, sync::Arc};

use pernixc_serialize::binary::ser::BinarySerializer;
use pernixc_stable_hash::{
    StableHash, StableHasher as _, StableSipHasher, Value,
};
use pernixc_stable_type_id::StableTypeID;

use crate::{runtime::serde::DynamicSerialize, Key};

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
}

impl Persistence {
    /// Test
    pub fn new<
        E: DynamicSerialize<BinarySerializer<BufWriter<File>>> + 'static,
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

        Self {
            path,
            serde_extension: serde_extension as Arc<dyn Any>,
            serialize_any_value,
        }
    }

    /// Test
    pub fn save<K: Key>(value: &K::Value) {
        let key_stable_type_id = K::STABLE_TYPE_ID.as_u128();
        let value_fingerprint = {
            let mut hasher = StableSipHasher::new();
            value.stable_hash(&mut hasher);
            hasher.finish()
        };

        
    }
}
