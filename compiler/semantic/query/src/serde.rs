use std::marker::PhantomData;

use pernixc_hash::HashMap;
use pernixc_serialize::{de::Deserializer, ser::Serializer, Serialize};
use pernixc_stable_type_id::StableTypeID;

use crate::{database::map::Map, key::DynamicBox, Key};

pub trait DynamicSerialize<S: Serializer<Self>>: Sized {
    fn serialization_helper_by_type_id(
        &self,
    ) -> &HashMap<StableTypeID, SerializationHelper<S, Self>>;

    fn register<K: Key + Serialize<S, Self>>(&mut self)
    where
        K::Value: Serialize<S, Self>;
}

#[derive(Debug, Copy)]
pub struct SerializationHelper<S: Serializer<E>, E> {
    map_serializer: fn(&Map, &mut S, &mut E) -> Result<(), S::Error>,
    dynamic_box_serializer:
        fn(&DynamicBox, &mut S, &mut E) -> Result<(), S::Error>,

    std_type_id: std::any::TypeId,
}

impl<S: Serializer<E>, E> Clone for SerializationHelper<S, E> {
    fn clone(&self) -> Self {
        Self {
            map_serializer: self.map_serializer,
            std_type_id: self.std_type_id,
            dynamic_box_serializer: self.dynamic_box_serializer,
        }
    }
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

pub struct Registry<S: Serializer<E>, D, E> {
    serialization_helpers_by_type_id:
        HashMap<StableTypeID, SerializationHelper<S, E>>,

    _marker: PhantomData<D>,
}

impl<S: Serializer<E>, D: Deserializer<E>, E> Registry<S, D, E> {
    pub fn register<K: Key + Serialize<S, E>>(&mut self)
    where
        K::Value: Serialize<S, E>,
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
    }
}
