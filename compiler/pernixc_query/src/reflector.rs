use std::{any::TypeId, collections::HashMap, marker::PhantomData};

use serde::{
    de::{DeserializeSeed, Visitor},
    ser::SerializeMap,
    Serialize,
};

use crate::{map::Map, Database, Key};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Reflector {
    serialization_metadata_by_type_id: HashMap<TypeId, SerializationMetadata>,
    deserialization_metadata_by_type_id:
        HashMap<&'static str, DeserializationMetadata>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct SerdeMetadata {
    ser: SerializationMetadata,
    de: DeserializationMetadata,
}

type SerializeFn = fn(&Map, &mut dyn FnMut(&dyn erased_serde::Serialize));
type DeserializeFn = fn(
    &Map,
    &mut dyn erased_serde::Deserializer,
) -> Result<(), erased_serde::Error>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct SerializationMetadata {
    serialize_entry: SerializeFn,
    unique_type_name: &'static str,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct DeserializationMetadata {
    deserialize_entry: DeserializeFn,
}

struct MapDeserializeHelper<'a, K> {
    map: &'a Map,
    _phantom: PhantomData<K>,
}

impl<'de, K: Key> DeserializeSeed<'de> for MapDeserializeHelper<'_, K> {
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_map(self)
    }
}

impl<'de, K: Key> Visitor<'de> for MapDeserializeHelper<'_, K> {
    type Value = ();

    fn expecting(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        write!(formatter, "a map with keys of type {}", K::unique_type_name())
    }

    fn visit_map<M>(self, mut map: M) -> Result<Self::Value, M::Error>
    where
        M: serde::de::MapAccess<'de>,
    {
        while let Some((key, value)) = map.next_entry::<K, K::Value>()? {
            self.map.entry(key, |entry| match entry {
                dashmap::Entry::Occupied(mut occupied_entry) => {
                    let existing_value = occupied_entry.get_mut();
                    K::merge_value(existing_value, value)
                        .map_err(serde::de::Error::custom)
                }
                dashmap::Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(value);

                    Ok(())
                }
            })?;
        }

        Ok(())
    }
}

impl Database {
    /// Stores the serialization information allowing serialization of [`Map`]
    /// containing the given type `T`. When all the types are registered
    /// the map can be serialized using the [`Map::serializable`] method along
    /// with the reflector.
    pub fn register_reflector<T: Key>(&mut self) {
        let type_id = TypeId::of::<T>();
        if self
            .reflector
            .serialization_metadata_by_type_id
            .contains_key(&type_id)
        {
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

        let deserialize_entry =
            |map: &Map, deserializer: &mut dyn erased_serde::Deserializer| {
                let deserialize_helper =
                    MapDeserializeHelper { map, _phantom: PhantomData::<T> };

                deserialize_helper.deserialize(deserializer)
            };

        let serde_metadata = SerdeMetadata {
            ser: SerializationMetadata {
                serialize_entry,
                unique_type_name: T::unique_type_name(),
            },
            de: DeserializationMetadata { deserialize_entry },
        };

        self.reflector
            .serialization_metadata_by_type_id
            .insert(type_id, serde_metadata.ser);
        self.reflector
            .deserialization_metadata_by_type_id
            .insert(serde_metadata.ser.unique_type_name, serde_metadata.de);
    }
}

impl Map {
    /// Creates a serializable view of the map using the provided reflector.
    ///
    /// # Preconditions
    ///
    /// The provided reflector must have registered all the types that are
    /// expected to be serialized from this map.
    pub const fn serializable<'a>(
        &'a self,
        reflector: &'a Reflector,
    ) -> SerializableMap<'a> {
        SerializableMap { reflector, map: self }
    }

    /// Creates a deserializable view of the map using the provided reflector.
    ///
    /// # Preconditions
    ///
    /// The provided reflector must have registered all the types that are
    /// expected to be deserialized into this map.
    pub const fn deserializable<'a>(
        &'a self,
        reflector: &'a Reflector,
    ) -> DeserializableMap<'a> {
        DeserializableMap { reflector, map: self }
    }
}

#[derive(Debug, Clone, Copy)]
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
            ser_metadata: &'a SerializationMetadata,
            map: &'a Map,
        }

        impl Serialize for TypedMapSer<'_> {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                let mut result = None;
                let mut serializer = Some(serializer);

                (self.ser_metadata.serialize_entry)(self.map, &mut |x| {
                    result = Some(erased_serde::serialize(
                        x,
                        serializer.take().unwrap(),
                    ));
                });

                result.unwrap()
            }
        }

        let in_map_count = self.map.type_lens();
        let mut map = serializer.serialize_map(Some(self.map.type_lens()))?;

        let mut serialized_count = 0;

        for (type_id, ser_metadata) in
            &self.reflector.serialization_metadata_by_type_id
        {
            // skip if this serialization metadata is not applicable to the
            // current map.
            if !self.map.has_type_id(*type_id) {
                continue;
            }

            map.serialize_entry(ser_metadata.unique_type_name, &TypedMapSer {
                ser_metadata,
                map: self.map,
            })?;

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

/// Implements [`DeserializeSeed`], allowing incremental deserialization of
/// [`Map`]
#[derive(Debug, Clone, Copy)]
pub struct DeserializableMap<'a> {
    reflector: &'a Reflector,
    map: &'a Map,
}

impl<'de> DeserializeSeed<'de> for DeserializableMap<'_> {
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_map(self)
    }
}

impl<'de> Visitor<'de> for DeserializableMap<'_> {
    type Value = ();

    fn expecting(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        write!(formatter, "a map with deserializable entries")
    }

    fn visit_map<M>(self, mut map: M) -> Result<Self::Value, M::Error>
    where
        M: serde::de::MapAccess<'de>,
    {
        struct DeserializeInnerMap<'a> {
            map: &'a Map,
            deserializable_entry: DeserializeFn,
        }

        impl<'de> DeserializeSeed<'de> for DeserializeInnerMap<'_> {
            type Value = ();

            fn deserialize<D>(
                self,
                deserializer: D,
            ) -> Result<Self::Value, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                use serde::de::Error;

                let mut erased =
                    <dyn erased_serde::Deserializer>::erase(deserializer);
                (self.deserializable_entry)(self.map, &mut erased)
                    .map_err(D::Error::custom)
            }
        }

        while let Some(type_name) = map.next_key::<&str>()? {
            let deserialization_metadata = self
                .reflector
                .deserialization_metadata_by_type_id
                .get(type_name)
                .ok_or_else(|| {
                    serde::de::Error::custom(format!(
                        "No deserialization metadata for type: {type_name}"
                    ))
                })?;

            let deserialize_entry = deserialization_metadata.deserialize_entry;
            map.next_value_seed(DeserializeInnerMap {
                map: self.map,
                deserializable_entry: deserialize_entry,
            })?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod test;
