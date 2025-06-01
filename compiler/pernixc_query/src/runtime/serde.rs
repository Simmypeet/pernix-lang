//! Contains the definition of the [`Registry`] struct.

use std::{any::Any, collections::HashMap, marker::PhantomData};

use enum_as_inner::EnumAsInner;
use serde::{
    de::{DeserializeSeed, Visitor},
    ser::{SerializeMap, SerializeStruct},
    Deserialize, Serialize,
};
use smallbox::{smallbox, SmallBox};

use crate::{
    database::map::{Map, TypedMap},
    key::{Dynamic, DynamicBox, StableTypeID},
    Key,
};

/// A struct enabling dynamic serialization and deserialization of
/// [`Map`]s and [`DynamicBox`]es.
#[derive(Debug, Default)]
pub struct Registry {
    serialization_metadata_by_type_id:
        HashMap<StableTypeID, SerializationMetadata>,
    deserialization_metadata_by_type_id:
        HashMap<StableTypeID, DeserializationMetadata>,
}

#[derive(Debug)]
struct SerdeMetadata {
    ser: SerializationMetadata,
    de: DeserializationMetadata,
}
type SerializeMapFn = fn(
    &Map,
    Option<&(FilterObjectBox, FilterObjectFn)>,
    &mut dyn FnMut(&dyn erased_serde::Serialize),
);
type SerializeBoxFn =
    fn(&dyn Any, &mut dyn FnMut(&dyn erased_serde::Serialize));

type DeserializeMapFn = fn(
    &Map,
    &mut dyn erased_serde::Deserializer,
) -> Result<(), erased_serde::Error>;
type DeserializeBoxFn = fn(
    &mut dyn erased_serde::Deserializer,
) -> Result<DynamicBox, erased_serde::Error>;

type FilterObjectBox = SmallBox<dyn Any, fn() -> ()>;
type FilterObjectFn = fn(&dyn Any, &dyn Any) -> bool;

#[derive(Debug)]
struct SerializationMetadata {
    serialize_map: SerializeMapFn,
    serialize_box: SerializeBoxFn,
    type_id: std::any::TypeId,

    filter_object: Option<(FilterObjectBox, FilterObjectFn)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct DeserializationMetadata {
    deserialize_entry: DeserializeMapFn,
    deserialize_box: DeserializeBoxFn,
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

impl Registry {
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

        let serde_metadata = SerdeMetadata {
            ser: SerializationMetadata {
                serialize_map,
                serialize_box,
                type_id: std::any::TypeId::of::<T>(),
                filter_object: None,
            },
            de: DeserializationMetadata { deserialize_entry, deserialize_box },
        };

        self.serialization_metadata_by_type_id
            .insert(T::STABLE_TYPE_ID, serde_metadata.ser);
        self.deserialization_metadata_by_type_id
            .insert(T::STABLE_TYPE_ID, serde_metadata.de);
    }

    /// Stores the serialization information allowing serialization of [`Map`]
    /// containing the given type `T`. When all the types are registered
    /// the map can be serialized using the [`Map::serializable`] method along
    /// with the reflector.
    pub fn register_with_filter<T: Key, F: Fn(&T) -> bool + Any>(
        &mut self,
        filter_object: F,
    ) {
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

        let filter_key_fn = |key: &dyn Any, filter_fn: &dyn Any| {
            let filter_fn = filter_fn
                .downcast_ref::<F>()
                .expect("Filter function does not match the expected type");

            let key = key.downcast_ref::<T>().expect(
                "Key type does not match the expected type for filtering",
            );

            filter_fn(key)
        };

        let serde_metadata = SerdeMetadata {
            ser: SerializationMetadata {
                serialize_map,
                serialize_box,
                type_id: std::any::TypeId::of::<T>(),
                filter_object: Some((smallbox!(filter_object), filter_key_fn)),
            },
            de: DeserializationMetadata { deserialize_entry, deserialize_box },
        };

        self.serialization_metadata_by_type_id
            .insert(T::STABLE_TYPE_ID, serde_metadata.ser);
        self.deserialization_metadata_by_type_id
            .insert(T::STABLE_TYPE_ID, serde_metadata.de);
    }
}

impl Map {
    /// Creates a serializable view of the map using the provided reflector.
    ///
    /// # Preconditions
    ///
    /// The provided reflector must have registered all the types that are
    /// expected to be serialized from this map.
    #[must_use]
    pub const fn serializable<'a>(
        &'a self,
        reflector: &'a Registry,
    ) -> SerializableMap<'a> {
        SerializableMap { serde: reflector, map: self }
    }

    /// Creates a deserializable view of the map using the provided reflector.
    ///
    /// # Preconditions
    ///
    /// The provided reflector must have registered all the types that are
    /// expected to be deserialized into this map.
    #[must_use]
    pub const fn deserializable<'a>(
        &'a self,
        reflector: &'a Registry,
    ) -> DeserializableMap<'a> {
        DeserializableMap { reflector, map: self }
    }
}

/// A wrapper struct allowing serialization of [`Map`] with the provided
/// reflector.
#[derive(Debug, Clone, Copy)]
pub struct SerializableMap<'a> {
    serde: &'a Registry,
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

                (self.ser_metadata.serialize_map)(
                    self.map,
                    self.ser_metadata.filter_object.as_ref(),
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
    reflector: &'a Registry,
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
                .reflector
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

thread_local! {
/// A thread-local state that holds a reflector instance allowing the
/// [`DynamicBox`] to be serialized/deserialized.
///
/// This is a little bit of a hack, but it allows us to avoid manually writing
/// the serialization and deserialization logic for on the CallGraph struct.
pub static REFLECTOR: std::cell::RefCell<Option<Registry>> = const { std::cell::RefCell::new(None) };
}

/// Sets the reflector to the current session allowing serialization of
/// [`Dynamic`] happening in the `f` closure
pub fn set_reflector<T>(reflector: &mut Registry, f: impl FnOnce() -> T) -> T {
    // temporarily take the current reflector, so that we can restore it
    let current = REFLECTOR.with(|r| r.borrow_mut().take());

    // replace the current reflector with the provided one
    REFLECTOR.with_borrow_mut(|r| *r = Some(std::mem::take(reflector)));

    // invoke the provided function
    let result = f();

    // restore the previous reflector
    REFLECTOR.with_borrow_mut(|r| {
        *reflector = r.take().expect("should be set");
        *r = current;
    });

    result
}

impl Serialize for DynamicBox {
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

        REFLECTOR.with(|r| {
            let reflector = r.borrow();
            let reflector = reflector.as_ref().expect("should've been set");
            let entry = reflector
                .serialization_metadata_by_type_id
                .get(&self.0.stable_type_id())
                .ok_or_else(|| {
                    serde::ser::Error::custom(format!(
                        "No serialization metadata for type: {}",
                        self.0.type_name()
                    ))
                })?;

            let mut struct_serializer =
                serializer.serialize_struct("Dynamic", 2)?;

            struct_serializer
                .serialize_field("stable_type_id", &self.stable_type_id())?;
            struct_serializer.serialize_field("value", &Wrapper {
                value: &*self.0,
                serializer_box_fn: entry.serialize_box,
            })?;

            struct_serializer.end()
        })
    }
}

impl<'de> Deserialize<'de> for DynamicBox {
    #[allow(clippy::too_many_lines)]
    fn deserialize<D: serde::Deserializer<'de>>(
        deserializer: D,
    ) -> Result<Self, D::Error> {
        #[derive(EnumAsInner)]
        enum Field {
            StableTypeID,
            Value,
            Ignore,
        }

        struct FieldVisitor;

        impl Visitor<'_> for FieldVisitor {
            type Value = Field;

            fn expecting(
                &self,
                formatter: &mut std::fmt::Formatter,
            ) -> std::fmt::Result {
                write!(formatter, "a field name")
            }

            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                match v {
                    0 => Ok(Field::StableTypeID),
                    1 => Ok(Field::Value),
                    _ => Ok(Field::Ignore),
                }
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                match v {
                    "stable_type_id" => Ok(Field::StableTypeID),
                    "value" => Ok(Field::Value),
                    _ => Ok(Field::Ignore),
                }
            }

            fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                match v {
                    b"stable_type_id" => Ok(Field::StableTypeID),
                    b"value" => Ok(Field::Value),
                    _ => Ok(Field::Ignore),
                }
            }
        }

        impl<'de> Deserialize<'de> for Field {
            fn deserialize<D: serde::Deserializer<'de>>(
                deserializer: D,
            ) -> Result<Self, D::Error> {
                deserializer.deserialize_identifier(FieldVisitor)
            }
        }

        struct DynamicBoxDeserializeHelper(DeserializeBoxFn);

        impl<'de> DeserializeSeed<'de> for DynamicBoxDeserializeHelper {
            type Value = DynamicBox;

            fn deserialize<D: serde::Deserializer<'de>>(
                self,
                deserializer: D,
            ) -> Result<Self::Value, D::Error> {
                use serde::de::Error;

                let mut erased =
                    <dyn erased_serde::Deserializer>::erase(deserializer);
                (self.0)(&mut erased).map_err(D::Error::custom)
            }
        }

        struct DynamicBoxVisitor;

        impl<'de> Visitor<'de> for DynamicBoxVisitor {
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
                let deserialize_box = REFLECTOR.with(|r| {
                    let reflector = r.borrow();
                    let reflector =
                        reflector.as_ref().expect("should've been set");

                    reflector
                        .deserialization_metadata_by_type_id
                        .get(&stable_type_id)
                        .map(|x| x.deserialize_box)
                        .ok_or_else(|| {
                            serde::de::Error::custom(format!(
                                "No deserialization metadata for type: stable \
                                 type id {stable_type_id:?}"
                            ))
                        })
                })?;

                if !map.next_key::<Field>()?.is_some_and(|x| x.is_value()) {
                    return Err(serde::de::Error::custom(
                        "Expected a `value` field",
                    ));
                }

                let value = map.next_value_seed(
                    DynamicBoxDeserializeHelper(deserialize_box),
                )?;

                if map.next_key::<Field>()?.is_some() {
                    return Err(serde::de::Error::custom(
                        "Expected no more fields after `value`",
                    ));
                }

                Ok(value)
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                let stable_type_id =
                    seq.next_element::<StableTypeID>()?.ok_or_else(|| {
                        serde::de::Error::custom(
                            "Expected a `stable_type_id` field",
                        )
                    })?;

                let deserialize_box = REFLECTOR.with(|r| {
                    let reflector = r.borrow();
                    let reflector =
                        reflector.as_ref().expect("should've been set");

                    reflector
                        .deserialization_metadata_by_type_id
                        .get(&stable_type_id)
                        .map(|x| x.deserialize_box)
                        .ok_or_else(|| {
                            serde::de::Error::custom(format!(
                                "No deserialization metadata for type: stable \
                                 type id {stable_type_id:?}"
                            ))
                        })
                })?;

                let value = seq
                    .next_element_seed(DynamicBoxDeserializeHelper(
                        deserialize_box,
                    ))?
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

        deserializer.deserialize_struct(
            "DynamicBox",
            &["stable_type_id", "value"],
            DynamicBoxVisitor,
        )
    }
}

#[cfg(test)]
mod test;
