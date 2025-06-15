//! Contains the definition of the [`Registry`] struct.

use std::{any::Any, collections::HashMap, marker::PhantomData};

use pernixc_serialize::{
    de::Deserializer, ser::Serializer, Deserialize, Serialize,
};
use pernixc_stable_type_id::StableTypeID;
use smallbox::SmallBox;

use crate::{database::map::Map, key::DynamicBox};

pub trait DynamicSerialize<S: Serializer> {
    fn serialization_metadata_by_type_id(
        &self,
    ) -> &HashMap<StableTypeID, SerMetadata<S>>;
}

impl<S: Serializer> Serialize<S> for Map
where
    S::Extension: DynamicSerialize<S>,
{
    fn serialize(
        &self,
        serializer: &mut S,
    ) -> Result<(), <S as Serializer>::Error> {
        serializer.emit_map(self.type_lens(), |mut x| {
            
        })
    }
}

/// A struct enabling dynamic serialization and deserialization of
/// [`Map`]s and [`DynamicBox`]es.
#[derive(Debug, Default)]
pub struct Serde<S, D> {
    serialization_metadata_by_type_id: HashMap<StableTypeID, SerMetadata<S>>,
    _marker: PhantomData<D>, /* deserialization_metadata_by_type_id:
                              * HashMap<StableTypeID,
                              * DeMetadata<D>>, */
}

type SerializeMapFn<S> = fn(
    &Map,
    Option<&(FilterObjectBox, FilterObjectFn)>,
    &mut dyn FnMut(&dyn Serialize<S>),
);
type SerializeBoxFn<S> = fn(&dyn Any, &mut dyn FnMut(&dyn Serialize<S>));

type DeserializeMapFn<D: Deserializer> =
    fn(&Map, &mut dyn Deserialize<D>) -> Result<(), D::Error>;

type DeserializeBoxFn<D: Deserializer> =
    fn(&mut D) -> Result<DynamicBox, D::Error>;

type FilterObjectBox = SmallBox<dyn Any, fn() -> ()>;
type FilterObjectFn = fn(&dyn Any, &dyn Any) -> bool;

#[derive(Debug)]
struct SerMetadata<S> {
    serialize_map: SerializeMapFn<S>,
    serialize_box: SerializeBoxFn<S>,
    type_id: std::any::TypeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct DeMetadata<D: Deserializer> {
    // deserialize_entry: DeserializeMapFn<D>,
    deserialize_box: DeserializeBoxFn<D>,
}

/*
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
        write!(
            formatter,
            "a map with keys of type {}",
            std::any::type_name::<K>()
        )
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
                        .map_err(serde::de::Error::custom)?;

                    Ok(())
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

struct SerializeTypeMap<'a, K: Key> {
    typed_map: &'a TypedMap<K>,
    filter_object: Option<&'a (FilterObjectBox, FilterObjectFn)>,
}

impl<K: Key> Serialize for SerializeTypeMap<'_, K> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut map = serializer.serialize_map(Some(self.typed_map.len()))?;

        for a in self.typed_map {
            if let Some((filter_object, filter_fn)) =
                self.filter_object.as_ref()
            {
                if !filter_fn(a.key().any(), &**filter_object) {
                    continue; // skip this serialization entry
                }
            }

            map.serialize_entry(a.key(), a.value())?;
        }

        map.end()
    }
}

impl Serde {
    /// Stores the serialization information allowing serialization of [`Map`]
    /// containing the given type `T`. When all the types are registered
    /// the map can be serialized using the [`Map::serializable`] method along
    /// with the reflector.
    pub fn register<T: Key>(&mut self) {
        if self
            .serialization_metadata_by_type_id
            .contains_key(&T::STABLE_TYPE_ID)
        {
            panic!(
                "Type {} is already registered for serialization, use \
                 overwrite method to register it again",
                std::any::type_name::<T>()
            );
        }

        let serialize_box = |key: &dyn Any,
                             serializer: &mut dyn FnMut(
            &dyn erased_serde::Serialize,
        )| {
            let key = key.downcast_ref::<T>().expect(
                "Key type does not match the expected type for serialization",
            );

            serializer(key);
        };

        let serialize_map = |map: &Map,
                             filter_object: Option<&(
            FilterObjectBox,
            FilterObjectFn,
        )>,
                             serializer: &mut dyn FnMut(
            &dyn erased_serde::Serialize,
        )| {
            map.type_storage::<T, _>(|x| {
                let typed_map = x.unwrap();

                serializer(&SerializeTypeMap { typed_map, filter_object });
            });
        };

        let deserialize_entry =
            |map: &Map, deserializer: &mut dyn erased_serde::Deserializer| {
                let deserialize_helper =
                    MapDeserializeHelper { map, _phantom: PhantomData::<T> };

                deserialize_helper.deserialize(deserializer)
            };

        let deserialize_box =
            |deserializer: &mut dyn erased_serde::Deserializer| {
                Ok(DynamicBox(smallbox!(T::deserialize(deserializer)?)))
            };

        self.serialization_metadata_by_type_id.insert(
            T::STABLE_TYPE_ID,
            SerMetadata {
                serialize_map,
                serialize_box,
                type_id: std::any::TypeId::of::<T>(),
            },
        );
        self.deserialization_metadata_by_type_id.insert(
            T::STABLE_TYPE_ID,
            DeMetadata { deserialize_entry, deserialize_box },
        );
    }
}

/// A serializable wrapper for the `Map` struct.
///
/// This struct provides a way to serialize a `Map` instance along with
/// its associated serialization context. It contains references to both the
/// map and the serde configuration needed for proper serialization.
#[derive(Debug, Clone, Copy)]
pub struct SerializableMap<'a> {
    map: &'a Map,
    serde: &'a Serde,
}

impl Map {
    /// Returns a serializable version of the map that can be serialized using
    /// [`serde::Serialize`].
    #[must_use]
    pub const fn serializable<'a>(
        &'a self,
        serde: &'a Serde,
    ) -> SerializableMap<'a> {
        SerializableMap { map: self, serde }
    }
}

impl Serialize for SerializableMap<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // a temporary struct to serialize the map with the correct metadata
        struct TypedMapSer<'a> {
            ser_metadata: &'a SerMetadata,
            map: &'a Map,
        }

        impl Serialize for TypedMapSer<'_> {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                let mut result = None;
                let mut serializer = Some(serializer);

                (self.ser_metadata.serialize_map)(self.map, None, &mut |x| {
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
            &self.serde.serialization_metadata_by_type_id
        {
            // skip if this serialization metadata is not applicable to the
            // current map.
            if !self.map.has_type_id(ser_metadata.type_id) {
                continue;
            }

            map.serialize_entry(&type_id, &TypedMapSer {
                ser_metadata,
                map: self.map,
            })?;

            serialized_count += 1;
        }

        if serialized_count != in_map_count {
            return Err(serde::ser::Error::custom(format!(
                "Expected {in_map_count} entries, but got {serialized_count}; \
                 make sure all types are registered for serialization",
            )));
        }

        map.end()
    }
}

/// A deserializable wrapper for incrementally loading a `Map`.
///
/// This struct provides a way to deserialize into an existing `Map` instance
/// using incremental deserialization. It contains references to both the
/// map and the serde configuration needed for proper deserialization.
#[derive(Debug, Clone, Copy)]
pub struct IncrementalDeserializableMap<'a> {
    map: &'a Map,
    serde: &'a Serde,
}

impl Map {
    /// Returns a deserializable version of the map that can be deserialized
    /// using [`serde::Deserialize`].
    #[must_use]
    pub const fn incremental_deserializable<'a>(
        &'a self,
        serde: &'a Serde,
    ) -> IncrementalDeserializableMap<'a> {
        IncrementalDeserializableMap { map: self, serde }
    }
}

impl<'de> DeserializeSeed<'de> for &IncrementalDeserializableMap<'_> {
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_map(self)
    }
}

impl<'de> Visitor<'de> for &IncrementalDeserializableMap<'_> {
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
            deserializable_entry: DeserializeMapFn,
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

        while let Some(stable_type_id) = map.next_key::<StableTypeID>()? {
            let deserialization_metadata = self
                .serde
                .deserialization_metadata_by_type_id
                .get(&stable_type_id)
                .ok_or_else(|| {
                    serde::de::Error::custom(format!(
                        "No deserialization metadata for type: stable type id \
                         {stable_type_id:?}"
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

/// A serializable wrapper for the `DynamicBox` struct.
///
/// This struct provides a way to serialize a `DynamicBox` instance along with
/// its associated serialization context. It contains references to both the
/// dynamic box and the serde configuration needed for proper serialization.
#[derive(Debug, Clone, Copy)]
pub struct SerializableDynamicBox<'a> {
    dynamic_box: &'a DynamicBox,
    serde: &'a Serde,
}

impl DynamicBox {
    /// Returns a serializable version of the dynamic box that can be serialized
    /// using [`serde::Serialize`].
    #[must_use]
    pub const fn serializable<'a>(
        &'a self,
        serde: &'a Serde,
    ) -> SerializableDynamicBox<'a> {
        SerializableDynamicBox { dynamic_box: self, serde }
    }
}

impl Serialize for SerializableDynamicBox<'_> {
    fn serialize<S: serde::Serializer>(
        &self,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        struct Wrapper<'a> {
            value: &'a dyn Dynamic,
            serializer_box_fn: SerializeBoxFn,
        }

        impl Serialize for Wrapper<'_> {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                let mut result = None;
                let mut serializer = Some(serializer);

                (self.serializer_box_fn)(self.value.any(), &mut |x| {
                    result = Some(erased_serde::serialize(
                        x,
                        serializer.take().unwrap(),
                    ));
                });

                result.unwrap()
            }
        }

        let entry = self
            .serde
            .serialization_metadata_by_type_id
            .get(&self.dynamic_box.stable_type_id())
            .ok_or_else(|| {
                serde::ser::Error::custom(format!(
                    "No serialization metadata for type: {}",
                    self.dynamic_box.type_name()
                ))
            })?;

        let mut struct_serializer =
            serializer.serialize_struct("Dynamic", 2)?;

        struct_serializer.serialize_field(
            "stable_type_id",
            &self.dynamic_box.stable_type_id(),
        )?;
        struct_serializer.serialize_field("value", &Wrapper {
            value: &*self.dynamic_box.0,
            serializer_box_fn: entry.serialize_box,
        })?;

        struct_serializer.end()
    }
}

/// A deserializer for `DynamicBox` instances.
///
/// This struct provides a way to deserialize `DynamicBox` instances using
/// the associated serde configuration. It wraps a reference to the serde
/// configuration needed for proper deserialization.
#[derive(Debug, Clone, Copy)]
pub struct DynamicBoxDeserializer<'a>(pub &'a Serde);

impl Serde {
    /// Creates a deserializer for `DynamicBox` instances.
    ///
    /// This method returns a `DynamicBoxDeserializer` that can be used
    /// to deserialize `DynamicBox` instances using this serde configuration.
    ///
    /// # Returns
    /// A `DynamicBoxDeserializer` containing a reference to this serde
    /// configuration.
    #[must_use]
    pub const fn dynamic_box_deserializer(&self) -> DynamicBoxDeserializer {
        DynamicBoxDeserializer(self)
    }
}

impl<'de> DeserializeSeed<'de> for &DynamicBoxDeserializer<'_> {
    type Value = DynamicBox;

    #[allow(clippy::too_many_lines)]
    fn deserialize<D: serde::Deserializer<'de>>(
        self,
        deserializer: D,
    ) -> Result<Self::Value, D::Error> {
        deserializer.deserialize_struct(
            "DynamicBox",
            &["stable_type_id", "value"],
            self,
        )
    }
}

#[derive(Deserialize, EnumAsInner)]
#[serde(field_identifier)]
enum Field {
    #[serde(rename = "stable_type_id")]
    StableTypeID,
    #[serde(rename = "value")]
    Value,
}

struct DynamicBoxDeserializeHelper(DeserializeBoxFn);

impl<'de> DeserializeSeed<'de> for DynamicBoxDeserializeHelper {
    type Value = DynamicBox;

    fn deserialize<D: serde::Deserializer<'de>>(
        self,
        deserializer: D,
    ) -> Result<Self::Value, D::Error> {
        use serde::de::Error;

        let mut erased = <dyn erased_serde::Deserializer>::erase(deserializer);
        (self.0)(&mut erased).map_err(D::Error::custom)
    }
}

impl<'de> Visitor<'de> for &DynamicBoxDeserializer<'_> {
    type Value = DynamicBox;

    fn expecting(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        write!(formatter, "a dynamic box")
    }

    fn visit_map<A: serde::de::MapAccess<'de>>(
        self,
        mut map: A,
    ) -> Result<Self::Value, A::Error> {
        if !map
            .next_key::<Field>()?
            .ok_or(serde::de::Error::custom(
                "Expected a `stable_type_id` field",
            ))?
            .is_stable_type_id()
        {
            return Err(serde::de::Error::custom(
                "Expected a `stable_type_id` field",
            ));
        }

        let stable_type_id = map.next_value::<StableTypeID>()?;

        let deserialize_box = self
            .0
            .deserialization_metadata_by_type_id
            .get(&stable_type_id)
            .map(|x| x.deserialize_box)
            .ok_or_else(|| {
                serde::de::Error::custom(format!(
                    "No deserialization metadata for type: stable type id \
                     {stable_type_id:?}"
                ))
            })?;

        if !map.next_key::<Field>()?.is_some_and(|x| x.is_value()) {
            return Err(serde::de::Error::custom("Expected a `value` field"));
        }

        let value =
            map.next_value_seed(DynamicBoxDeserializeHelper(deserialize_box))?;

        Ok(value)
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        let stable_type_id =
            seq.next_element::<StableTypeID>()?.ok_or_else(|| {
                serde::de::Error::custom("Expected a `stable_type_id` field")
            })?;

        let deserialize_box = self
            .0
            .deserialization_metadata_by_type_id
            .get(&stable_type_id)
            .map(|x| x.deserialize_box)
            .ok_or_else(|| {
                serde::de::Error::custom(format!(
                    "No deserialization metadata for type: stable type id \
                     {stable_type_id:?}"
                ))
            })?;

        let value = seq
            .next_element_seed(DynamicBoxDeserializeHelper(deserialize_box))?
            .ok_or_else(|| {
                serde::de::Error::custom("Expected a `value` field")
            })?;

        if seq.next_element::<Field>()?.is_some() {
            return Err(serde::de::Error::custom(
                "Expected no more fields after `value`",
            ));
        }

        Ok(value)
    }
}

#[cfg(test)]
mod test;
*/
