use std::marker::PhantomData;

use pernixc_hash::{DashMap, HashMap};
use pernixc_serialize::{
    de::{Deserializer, MapAccess, StructAccess},
    ser::Serializer,
    Deserialize, Serialize,
};
use pernixc_stable_type_id::StableTypeID;
use smallbox::smallbox;

use crate::{database::map::Map, key::DynamicBox, Key};

pub trait DynamicSerialize<S: Serializer<Self>>: Sized {
    fn serialization_helper_by_type_id(
        &self,
    ) -> &HashMap<StableTypeID, SerializationHelper<S, Self>>;
}

pub trait DynamicDeserialize<D: Deserializer<Self>>: Sized {
    fn deserialization_helper_by_type_id(
        &self,
    ) -> &HashMap<StableTypeID, DeserializationHelper<D, Self>>;
}

pub trait DynamicRegistry<S: Serializer<Self>, D: Deserializer<Self>>:
    Sized
{
    fn register<K: Key + Serialize<S, Self> + Deserialize<D, Self>>(&mut self)
    where
        K::Value: Serialize<S, Self> + Deserialize<D, Self>;
}

#[derive(Debug, Copy)]
pub struct SerializationHelper<S: Serializer<E>, E> {
    map_serializer: fn(&Map, &mut S, &mut E) -> Result<(), S::Error>,
    dynamic_box_serializer:
        fn(&DynamicBox, &mut S, &mut E) -> Result<(), S::Error>,

    std_type_id: std::any::TypeId,
}

#[derive(Debug, Copy)]
pub struct DeserializationHelper<D: Deserializer<E>, E> {
    map_deserializer: for<'m, 'v> fn(
        &mut Map,
        <D::MapAccess<'m> as MapAccess<E>>::ValueAccess<'v>,
        &mut E,
    ) -> Result<(), D::Error>,

    dynamic_box_deserializer: for<'m, 'v> fn(
        <D::StructAccess<'m> as StructAccess<E>>::FieldAccess<'v>,
        &mut E,
    )
        -> Result<DynamicBox, D::Error>,

    std_type_id: std::any::TypeId,
}

struct SerializableTypedMap<'s, S: Serializer<E>, E> {
    map: &'s Map,
    helper: &'s SerializationHelper<S, E>,
}

impl<S: Serializer<E>, E> Serialize<S, E> for SerializableTypedMap<'_, S, E> {
    fn serialize(
        &self,
        serializer: &mut S,
        extension: &mut E,
    ) -> Result<(), <S as Serializer<E>>::Error> {
        (self.helper.map_serializer)(self.map, serializer, extension)
    }
}

impl<S: Serializer<E>, E: DynamicSerialize<S>> Serialize<S, E> for Map {
    fn serialize(
        &self,
        serializer: &mut S,
        extension: &mut E,
    ) -> Result<(), <S as Serializer<E>>::Error> {
        use pernixc_serialize::ser::{Error, Map};

        /*
        Serialize in the form of = {
            "type_id": {
                "key1": "value1",
                "key2": "value2",
                ...
            },
            "type_id2": {
                "key1": "value1",
                "key2": "value2",
                ...
            },
            ..
        }
         */

        serializer.emit_map(
            self.type_lens(),
            extension,
            |mut map, extension| {
                let mut serialized_count = 0;

                let serialization_helpers = extension
                    .serialization_helper_by_type_id()
                    .iter()
                    .map(|x| (*x.0, x.1.clone()))
                    .collect::<Vec<_>>();

                for (stable_type_id, helper) in serialization_helpers {
                    // skip this map type
                    if !self.has_type_id(helper.std_type_id) {
                        continue;
                    }

                    map.serialize_entry(
                        &stable_type_id,
                        &SerializableTypedMap { map: self, helper: &helper },
                        extension,
                    )?;

                    serialized_count += 1;
                }

                if serialized_count < self.type_lens() {
                    return Err(S::Error::custom(format!(
                        "some of the types were not serialized, expected {}, \
                         got {}; make sure all types are registered",
                        self.type_lens(),
                        serialized_count,
                    )));
                }

                Ok(())
            },
        )
    }
}

impl<S: Serializer<E>, E: DynamicSerialize<S>> Serialize<S, E> for DynamicBox {
    fn serialize(
        &self,
        serializer: &mut S,
        extension: &mut E,
    ) -> Result<(), <S as Serializer<E>>::Error> {
        use pernixc_serialize::ser::Error;

        let helper = extension
            .serialization_helper_by_type_id()
            .get(&self.stable_type_id())
            .cloned()
            .ok_or_else(|| {
                Error::custom(format!(
                    "no serialization helper found for {:?}",
                    self.stable_type_id()
                ))
            })?;

        (helper.dynamic_box_serializer)(self, serializer, extension)
    }
}

pub struct Registry<S: Serializer<E>, D: Deserializer<E>, E> {
    serialization_helpers_by_type_id:
        HashMap<StableTypeID, SerializationHelper<S, E>>,
    deserialization_helpers_by_type_id:
        HashMap<StableTypeID, DeserializationHelper<D, E>>,
}

impl<S: Serializer<E>, D: Deserializer<E>, E> Registry<S, D, E> {
    pub fn new() -> Self {
        Self {
            serialization_helpers_by_type_id: HashMap::default(),
            deserialization_helpers_by_type_id: HashMap::default(),
        }
    }
}

impl<S: Serializer<E>, D: Deserializer<E>, E> Default for Registry<S, D, E> {
    fn default() -> Self {
        Self {
            serialization_helpers_by_type_id: HashMap::default(),
            deserialization_helpers_by_type_id: HashMap::default(),
        }
    }
}

fn map_deserializer<D: Deserializer<E>, E, K: Key + Deserialize<D, E>>(
    map: &mut Map,
    value_access: <D::MapAccess<'_> as MapAccess<E>>::ValueAccess<'_>,
    extension: &mut E,
) -> Result<(), D::Error>
where
    K::Value: Deserialize<D, E>,
{
    use pernixc_serialize::de::{Error, ValueAccess};

    let typed_map =
        value_access.deserialize::<DashMap<K, K::Value>>(extension)?;

    // overwrite the existing map or insert a new one
    if map.insert_typed_map(typed_map).is_some() {
        return Err(D::Error::custom(format!(
            "the map for type {} already exists",
            std::any::type_name::<K>()
        )));
    }

    Ok(())
}

fn dynamic_box_deserializer<
    D: Deserializer<E>,
    E,
    K: Key + Deserialize<D, E>,
>(
    value_access: <D::StructAccess<'_> as StructAccess<E>>::FieldAccess<'_>,
    extension: &mut E,
) -> Result<DynamicBox, D::Error> {
    use pernixc_serialize::de::FieldAccess;

    let key: K = value_access.deserialize(extension)?;

    Ok(DynamicBox(smallbox!(key)))
}

impl<S: Serializer<E>, D: Deserializer<E>, E> Registry<S, D, E> {
    pub fn register<K: Key + Serialize<S, E> + Deserialize<D, E>>(&mut self)
    where
        K::Value: Serialize<S, E> + Deserialize<D, E>,
    {
        use pernixc_serialize::ser::Struct;

        let map_serializer =
            |map: &Map, serializer: &mut S, extension: &mut E| {
                map.type_storage::<K, _>(|x| {
                    let map = x.expect("should exists");

                    map.serialize(serializer, extension)
                })
            };

        let dyn_box_serializer =
            |dyn_box: &DynamicBox, serializer: &mut S, extension: &mut E| {
                serializer.emit_struct(
                    "DynamicBox",
                    2,
                    extension,
                    |mut st, extension| {
                        st.serialize_field(
                            "stable_type_id",
                            &dyn_box.stable_type_id(),
                            extension,
                        )?;

                        // downcast to the concrete type
                        let value = dyn_box.any().downcast_ref::<K>().expect(
                            "the dynamic box should contain a value of the \
                             registered type",
                        );

                        st.serialize_field("value", value, extension)?;

                        Ok(())
                    },
                )
            };

        self.serialization_helpers_by_type_id
            .insert(K::STABLE_TYPE_ID, SerializationHelper {
                map_serializer,
                dynamic_box_serializer: dyn_box_serializer,
                std_type_id: std::any::TypeId::of::<K>(),
            })
            .unwrap_or_else(|| {
                panic!(
                    "the key {} has already been registered",
                    std::any::type_name::<K>()
                )
            });
        self.deserialization_helpers_by_type_id
            .insert(K::STABLE_TYPE_ID, DeserializationHelper {
                map_deserializer: map_deserializer::<_, _, K>,
                dynamic_box_deserializer: dynamic_box_deserializer::<_, _, K>,
                std_type_id: std::any::TypeId::of::<K>(),
            })
            .unwrap_or_else(|| {
                panic!(
                    "the key {} has already been registered",
                    std::any::type_name::<K>()
                )
            });
    }
}

impl<S: Serializer<E>, E> Clone for SerializationHelper<S, E> {
    fn clone(&self) -> Self {
        Self {
            map_serializer: self.map_serializer,
            dynamic_box_serializer: self.dynamic_box_serializer,
            std_type_id: self.std_type_id,
        }
    }
}

impl<D: Deserializer<E>, E> Clone for DeserializationHelper<D, E> {
    fn clone(&self) -> Self {
        Self {
            map_deserializer: self.map_deserializer,
            dynamic_box_deserializer: self.dynamic_box_deserializer,
            std_type_id: self.std_type_id,
        }
    }
}

impl<D: Deserializer<E>, E: DynamicDeserialize<D>>
    pernixc_serialize::de::Deserialize<D, E> for Map
where
    D::Error: pernixc_serialize::de::Error,
{
    fn deserialize(
        deserializer: &mut D,
        extension: &mut E,
    ) -> Result<Self, D::Error> {
        use pernixc_serialize::de::{Error, MapAccess};

        deserializer.expect_map(extension, |mut map_access, extension| {
            let mut result_map = Self::default();

            // Deserialize each type's map
            loop {
                let done = map_access.next_entry(extension, |entry| {
                    if let Some((stable_type_id, value_access, extension)) =
                        entry
                    {
                        let helper = extension
                            .deserialization_helper_by_type_id()
                            .get(&stable_type_id)
                            .cloned()
                            .ok_or_else(|| {
                                D::Error::custom(format!(
                                    "no deserialization helper found for \
                                     {stable_type_id:?}"
                                ))
                            })?;

                        // The helper will deserialize the map data for this
                        // type
                        (helper.map_deserializer)(
                            &mut result_map,
                            value_access,
                            extension,
                        )?;
                        Ok(false) // Continue processing
                    } else {
                        Ok(true) // No more entries
                    }
                })?;

                if done {
                    break;
                }
            }

            Ok(result_map)
        })
    }
}

impl<D: Deserializer<E>, E: DynamicDeserialize<D>>
    pernixc_serialize::de::Deserialize<D, E> for DynamicBox
where
    D::Error: pernixc_serialize::de::Error,
{
    fn deserialize(
        deserializer: &mut D,
        extension: &mut E,
    ) -> Result<Self, D::Error> {
        use pernixc_serialize::de::{
            Error, FieldAccess, Identifier, StructAccess,
        };

        deserializer.expect_struct(
            "DynamicBox",
            &["stable_type_id", "value"],
            extension,
            |mut struct_access, extension| {
                let stable_type_id: StableTypeID =
                    struct_access.next_field(extension, |field_access| {
                        let Some((identifier, value, extension)) = field_access
                        else {
                            return Err(D::Error::custom(
                                "expected 'stable_type_id' field",
                            ));
                        };

                        if !matches!(
                            identifier,
                            Identifier::Index(0)
                                | Identifier::Name("stable_type_id")
                        ) {
                            return Err(D::Error::custom(
                                "expected 'stable_type_id' field",
                            ));
                        }

                        value.deserialize(extension)
                    })?;

                let helper = extension
                    .deserialization_helper_by_type_id()
                    .get(&stable_type_id)
                    .cloned()
                    .ok_or_else(|| {
                        D::Error::custom(format!(
                            "no deserialization helper found for \
                             {stable_type_id:?}"
                        ))
                    })?;

                struct_access.next_field(extension, |field_access| {
                    let Some((identifier, value, extension)) = field_access
                    else {
                        return Err(D::Error::custom("expected 'value' field"));
                    };

                    if !matches!(
                        identifier,
                        Identifier::Index(1) | Identifier::Name("value")
                    ) {
                        return Err(D::Error::custom("expected 'value' field"));
                    }

                    (helper.dynamic_box_deserializer)(value, extension)
                })
            },
        )
    }
}

pub struct SelfRegistry<S: Serializer<Self>, D: Deserializer<Self>> {
    registry: Registry<S, D, Self>,
}

impl<S: Serializer<Self>, D: Deserializer<Self>> SelfRegistry<S, D> {
    pub fn new() -> Self { Self { registry: Registry::new() } }
}

impl<S: Serializer<Self>, D: Deserializer<Self>> Default
    for SelfRegistry<S, D>
{
    fn default() -> Self { Self { registry: Registry::default() } }
}

impl<S: Serializer<Self>, D: Deserializer<Self>> DynamicSerialize<S>
    for SelfRegistry<S, D>
{
    fn serialization_helper_by_type_id(
        &self,
    ) -> &HashMap<StableTypeID, SerializationHelper<S, Self>> {
        &self.registry.serialization_helpers_by_type_id
    }
}

impl<S: Serializer<Self>, D: Deserializer<Self>> DynamicDeserialize<D>
    for SelfRegistry<S, D>
{
    fn deserialization_helper_by_type_id(
        &self,
    ) -> &HashMap<StableTypeID, DeserializationHelper<D, Self>> {
        &self.registry.deserialization_helpers_by_type_id
    }
}

impl<S: Serializer<Self>, D: Deserializer<Self>> DynamicRegistry<S, D>
    for SelfRegistry<S, D>
{
    fn register<K: Key + Serialize<S, Self> + Deserialize<D, Self>>(&mut self)
    where
        K::Value: Serialize<S, Self> + Deserialize<D, Self>,
    {
        self.registry.register::<K>()
    }
}

#[cfg(test)]
mod test;
