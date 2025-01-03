//! The `serde` support for the `pernixc_component` crate.
//!
//! # Example
//!
//! ``` rust
//! use pernixc_component::{serde::Reflector, Storage};
//! use serde::de::DeserializeSeed;
//!
//! let storage = Storage::default();
//!
//! storage.add_component("a".to_string(), 1);
//! storage.add_component("a".to_string(), true);
//! storage.add_component("b".to_string(), "Hello".to_string());
//! storage.add_component("c".to_string(), vec![1]);
//! storage.add_component("c".to_string(), vec![true]);
//!
//! let mut reflector = Reflector::new();
//!
//! reflector.register_type::<i32>("i32".to_string());
//! reflector.register_type::<bool>("bool".to_string());
//! reflector.register_type::<String>("String".to_string());
//! reflector.register_type::<Vec<bool>>("Vec<bool>".to_string());
//! // intentionally not registering Vec<i32>
//!
//! let serializable = storage.as_serializable(&reflector);
//!
//! let serialized = serde_json::to_value(&serializable).unwrap();
//!
//! assert_eq!(serialized["a"]["i32"], 1);
//! assert_eq!(serialized["a"]["bool"], true);
//! assert_eq!(serialized["b"]["String"], "Hello");
//!
//! let c_bools = serialized["c"]["Vec<bool>"].as_array().unwrap();
//!
//! assert_eq!(c_bools.len(), 1);
//! assert_eq!(c_bools[0], true);
//!
//! // Vec<i32> is not registered
//! assert!(serialized["c"]["Vec<i32>"].is_null());
//!
//! let json_string = serde_json::to_string(&serializable).unwrap();
//! let mut deserializer = serde_json::Deserializer::from_str(&json_string);
//!
//! let deserialized: Storage<String> =
//!     reflector.deserialize(&mut deserializer).unwrap();
//!
//! assert_eq!(deserialized.get::<i32>("a".to_string()).as_deref(), Some(&1));
//! assert_eq!(
//!     deserialized.get::<bool>("a".to_string()).as_deref(),
//!     Some(&true)
//! );
//! assert_eq!(
//!     deserialized.get::<String>("b".to_string()).as_deref(),
//!     Some(&"Hello".to_string())
//! );
//! assert_eq!(
//!     deserialized.get::<Vec<bool>>("c".to_string()).as_deref(),
//!     Some(&vec![true])
//! );
//! assert!(deserialized.get::<Vec<i32>>("c".to_string()).is_none());
//! ```

use std::{
    any::{Any, TypeId},
    collections::{HashMap, HashSet},
    hash::Hash,
};

use serde::ser::SerializeMap;

use crate::Storage;

#[derive(Debug)]
struct SerializationMetaData<T> {
    serialize_fn: fn(&dyn Any, &mut dyn FnMut(&dyn erased_serde::Serialize)),
    tag: T,
}

#[derive(Debug)]
struct DeserializationMetaData {
    deserialize_fn: fn(
        &mut dyn erased_serde::Deserializer,
    ) -> Result<Box<dyn Any>, erased_serde::Error>,
}

/// Uesd to serialize/deserialize the [`Storage`] struct.
#[derive(Debug)]
pub struct Reflector<T, ID> {
    serialization_meta_datas: HashMap<TypeId, SerializationMetaData<T>>,
    deserialization_meta_datas: HashMap<T, DeserializationMetaData>,

    _phantom: std::marker::PhantomData<ID>,
}

impl<T, ID> Default for Reflector<T, ID> {
    fn default() -> Self {
        Reflector {
            serialization_meta_datas: HashMap::new(),
            deserialization_meta_datas: HashMap::new(),

            _phantom: std::marker::PhantomData,
        }
    }
}

impl<T, ID> Reflector<T, ID> {
    /// Create a new instance of [`Reflector`].
    pub fn new() -> Self {
        Reflector {
            serialization_meta_datas: HashMap::new(),
            deserialization_meta_datas: HashMap::new(),

            _phantom: std::marker::PhantomData,
        }
    }

    /// Register a type to be serialized/deserialized.
    ///
    /// The type can be registered at most once.
    ///
    /// # Non-Registered Type
    ///
    /// When serializing the [`Storage`], if the type is not registered, the
    /// component will be ignored.
    ///
    /// When deserializing the [`Storage`], if the type is not registered and
    /// the component with the tag is found, it will return an error.
    #[must_use]
    pub fn register_type<
        C: Any + serde::Serialize + for<'x> serde::Deserialize<'x>,
    >(
        &mut self,
        tag: T,
    ) -> bool
    where
        T: Clone + Eq + Hash,
    {
        let serialize_fn = |any: &dyn Any,
                            serializer: &mut dyn FnMut(
            &dyn erased_serde::Serialize,
        )| {
            let any = any.downcast_ref::<C>().unwrap();
            serializer(any);
        };

        let (
            std::collections::hash_map::Entry::Vacant(ser),
            std::collections::hash_map::Entry::Vacant(de),
        ) = (
            self.serialization_meta_datas.entry(TypeId::of::<C>()),
            self.deserialization_meta_datas.entry(tag.clone()),
        )
        else {
            return false;
        };

        ser.insert(SerializationMetaData { serialize_fn, tag });
        de.insert(DeserializationMetaData {
            deserialize_fn: |deserializer| {
                let value = C::deserialize(deserializer)?;
                Ok(Box::new(value))
            },
        });

        true
    }
}

/// The struct that enables the serialization of the [`Storage`] struct.
#[derive(Debug)]
pub struct SerializableStorage<'a, T, ID: Eq + Hash> {
    storage: &'a Storage<ID>,
    reflector: &'a Reflector<T, ID>,
}

impl<'a, T, ID: Eq + Hash> SerializableStorage<'a, T, ID> {
    /// Create a new instance of [`SerializableStorage`].
    pub fn new(
        storage: &'a Storage<ID>,
        reflector: &'a Reflector<T, ID>,
    ) -> Self {
        SerializableStorage { storage, reflector }
    }
}

impl<'a, T: serde::Serialize, ID: serde::Serialize + Eq + Hash + Clone>
    serde::Serialize for SerializableStorage<'a, T, ID>
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut type_ids_by_id = HashMap::<_, HashSet<_>>::new();

        for entry in self.storage.components.iter() {
            type_ids_by_id
                .entry(entry.key().0.clone())
                .or_insert_with(HashSet::new)
                .insert(entry.key().1);
        }

        let entity_count = type_ids_by_id
            .iter()
            .filter(|(_, type_ids)| {
                type_ids.iter().any(|x| {
                    self.reflector.serialization_meta_datas.contains_key(x)
                })
            })
            .count();

        if entity_count == 0 {
            return serializer.serialize_map(Some(0))?.end();
        }

        let mut map = serializer.serialize_map(Some(entity_count))?;

        for (id, type_ids) in type_ids_by_id {
            if !type_ids.iter().any(|x| {
                self.reflector.serialization_meta_datas.contains_key(x)
            }) {
                continue;
            }

            map.serialize_key(&id)?;
            map.serialize_value(&SerializeEntry {
                id: id.clone(),
                type_ids,
                reflector: self.reflector,
                storage: self.storage,
            })?;
        }

        map.end()
    }
}

struct SerializeEntry<'a, T, ID: Eq + Hash> {
    id: ID,
    type_ids: HashSet<TypeId>,

    reflector: &'a Reflector<T, ID>,
    storage: &'a Storage<ID>,
}

impl<'a, T: serde::Serialize, ID: Eq + Hash + Clone> serde::Serialize
    for SerializeEntry<'a, T, ID>
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let component_count = self
            .type_ids
            .iter()
            .filter(|x| {
                self.reflector.serialization_meta_datas.contains_key(*x)
            })
            .count();

        if component_count == 0 {
            return serializer.serialize_map(Some(0))?.end();
        }

        let mut map = serializer.serialize_map(Some(component_count))?;

        for type_id in &self.type_ids {
            let Some(serialize_meta) =
                self.reflector.serialization_meta_datas.get(type_id)
            else {
                continue;
            };

            let component = self
                .storage
                .components
                .get(&(self.id.clone(), *type_id))
                .unwrap();

            map.serialize_entry(&serialize_meta.tag, &SerializeComponent {
                component: &component,
                serialize_fn: serialize_meta.serialize_fn,
            })?;
        }

        map.end()
    }
}

struct SerializeComponent<'a> {
    component: &'a Box<dyn Any>,
    serialize_fn: fn(&dyn Any, &mut dyn FnMut(&dyn erased_serde::Serialize)),
}

impl<'a> serde::Serialize for SerializeComponent<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut result = None;
        let mut serializer = Some(serializer);

        (self.serialize_fn)(self.component.as_ref(), &mut |x| {
            result =
                Some(erased_serde::serialize(x, serializer.take().unwrap()));
        });

        result.unwrap()
    }
}

impl<
        'de,
        T: for<'x> serde::Deserialize<'x> + Eq + Hash + std::fmt::Debug,
        ID: Eq + Hash + Clone + for<'x> serde::Deserialize<'x>,
    > serde::de::DeserializeSeed<'de> for &Reflector<T, ID>
{
    type Value = Storage<ID>;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_map(EntitiyMapVisitor { reflector: self })
    }
}

struct EntitiyMapVisitor<'a, T, ID> {
    reflector: &'a Reflector<T, ID>,
}

impl<
        'de,
        T: for<'x> serde::Deserialize<'x> + Eq + Hash + std::fmt::Debug,
        ID: Eq + Hash + Clone + for<'x> serde::Deserialize<'x>,
    > serde::de::Visitor<'de> for EntitiyMapVisitor<'_, T, ID>
{
    type Value = Storage<ID>;

    fn expecting(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        formatter.write_str(
            "a map with the entity id as the key and the map of type tag to \
             component as the value",
        )
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::MapAccess<'de>,
    {
        let storage = Storage::default();

        while let Some(id) = map.next_key::<ID>()? {
            let components = map.next_value_seed(ComponentMapVisitor {
                reflector: self.reflector,
            })?;

            for (tag, component) in components {
                let type_id = component.as_ref().type_id();

                match storage.components.entry((id.clone(), type_id)) {
                    dashmap::Entry::Occupied(_) => {
                        return Err(serde::de::Error::custom(format!(
                            "duplicate component tag: {:?}",
                            tag
                        )));
                    }
                    dashmap::Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(component);
                    }
                }
            }
        }

        Ok(storage)
    }
}

impl<
        'de,
        T: for<'x> serde::Deserialize<'x> + Eq + Hash + std::fmt::Debug,
        ID: Eq + Hash + Clone + for<'x> serde::Deserialize<'x>,
    > serde::de::DeserializeSeed<'de> for ComponentMapVisitor<'_, T, ID>
{
    type Value = HashMap<T, Box<dyn Any>>;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_map(self)
    }
}

struct ComponentMapVisitor<'a, T, ID> {
    reflector: &'a Reflector<T, ID>,
}

impl<
        'de,
        'a,
        T: for<'x> serde::Deserialize<'x> + Eq + Hash + std::fmt::Debug,
        ID: Eq + Hash + Clone + for<'x> serde::Deserialize<'x>,
    > serde::de::Visitor<'de> for ComponentMapVisitor<'a, T, ID>
{
    type Value = HashMap<T, Box<dyn Any>>;

    fn expecting(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        formatter.write_str(
            "a map with the component tag as the key and the component as the \
             value",
        )
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::MapAccess<'de>,
    {
        let mut components = HashMap::new();

        while let Some(key) = map.next_key::<T>()? {
            let Some(deserialize_meta) =
                self.reflector.deserialization_meta_datas.get(&key)
            else {
                return Err(serde::de::Error::custom(format!(
                    "no deserialization meta data found for tag: {:?}",
                    key
                )));
            };

            let component = map.next_value_seed(ComponentDeserialzer {
                deserialize_fn: deserialize_meta.deserialize_fn,
            })?;

            if components.contains_key(&key) {
                return Err(serde::de::Error::custom(format!(
                    "duplicate component tag: {:?}",
                    key
                )));
            }

            assert!(components.insert(key, component).is_none());
        }

        Ok(components)
    }
}

struct ComponentDeserialzer {
    deserialize_fn: fn(
        &mut dyn erased_serde::Deserializer,
    ) -> Result<Box<dyn Any>, erased_serde::Error>,
}

impl<'de> serde::de::DeserializeSeed<'de> for ComponentDeserialzer {
    type Value = Box<dyn Any>;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;

        let mut erased = <dyn erased_serde::Deserializer>::erase(deserializer);
        (self.deserialize_fn)(&mut erased).map_err(D::Error::custom)
    }
}
