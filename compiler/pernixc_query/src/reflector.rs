use std::{any::TypeId, collections::HashMap};

use serde::{ser::SerializeMap, Serialize};

use crate::{map::Map, Key};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Reflector {
    serialization_meta_datas: HashMap<TypeId, SerializationMetadata>,
}

type SerializeFn = fn(&Map, &mut dyn FnMut(&dyn erased_serde::Serialize));

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct SerializationMetadata {
    serialize_entry: SerializeFn,
    unique_type_name: &'static str,
}

impl Reflector {
    /// Stores the serialization information allowing serialization of [`Map`]
    /// containing the given type `T`. When all the types are registered
    /// the map can be serialized using the [`Map::serializable`] method along
    /// with the reflector.
    pub fn register<T: Key>(&mut self) {
        let type_id = TypeId::of::<T>();
        if self.serialization_meta_datas.contains_key(&type_id) {
            return; // Already registered
        }

        let serialize_entry = |map: &Map,
                               serializer: &mut dyn FnMut(
            &dyn erased_serde::Serialize,
        )| {
            map.type_storage::<T, _>(|x| {
                let typed_map = x.unwrap();
                serializer(typed_map);
            });
        };

        let metadata = SerializationMetadata {
            serialize_entry,
            unique_type_name: T::unique_type_name(),
        };

        self.serialization_meta_datas.insert(type_id, metadata);
    }
}

impl Map {
    /// Creates a serializable view of the map using the provided reflector.
    /// The provided reflector must have registered all the types that are
    /// expected to be serialized from this map.
    pub const fn serializable<'a>(
        &'a self,
        reflector: &'a Reflector,
    ) -> SerializableMap<'a> {
        SerializableMap { reflector, map: self }
    }
}

pub struct SerializableMap<'a> {
    reflector: &'a Reflector,
    map: &'a Map,
}

impl Serialize for SerializableMap<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // a temporary struct to serialize the map with the correct metadata
        struct TypedMapSer<'a> {
            serialization_metadata: &'a SerializationMetadata,
            map: &'a Map,
        }

        impl Serialize for TypedMapSer<'_> {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                let mut result = None;
                let mut serializer = Some(serializer);

                (self.serialization_metadata.serialize_entry)(
                    self.map,
                    &mut |x| {
                        result = Some(erased_serde::serialize(
                            x,
                            serializer.take().unwrap(),
                        ));
                    },
                );

                result.unwrap()
            }
        }

        let in_map_count = self.map.type_lens();
        let mut map = serializer.serialize_map(Some(self.map.type_lens()))?;

        let mut serialized_count = 0;

        for (type_id, serialization_metadata) in
            &self.reflector.serialization_meta_datas
        {
            // skip if this serialization metadata is not applicable to the
            // current map.
            if !self.map.has_type_id(*type_id) {
                continue;
            }

            map.serialize_entry(
                serialization_metadata.unique_type_name,
                &TypedMapSer { serialization_metadata, map: self.map },
            )?;

            serialized_count += 1;
        }

        if serialized_count != in_map_count {
            return Err(serde::ser::Error::custom(format!(
                "Expected {in_map_count} entries, but got {serialized_count}"
            )));
        }

        map.end()
    }
}

#[cfg(test)]
mod test;
