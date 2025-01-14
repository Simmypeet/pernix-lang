//! The `serde` support for the `pernixc_storage` crate.
//!
//! # Example
//!
//! ``` rust
//! use pernixc_storage::{serde::Reflector, Storage};
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
//!
//! ## In-place Incremental Deserialization Example
//!
//! ``` rust
//! use std::convert::Infallible;
//!
//! use pernixc_storage::{
//!     serde::{MergerFn, Reflector},
//!     Storage,
//! };
//! use serde::de::DeserializeSeed;
//!
//! let first = Storage::default();
//!
//! first.add_component("myNumber".to_string(), 1);
//! first.add_component("myBoolean".to_string(), true);
//!
//! let second = Storage::default();
//!
//! second.add_component("myNumber".to_string(), 2);
//!
//! let mut reflector: Reflector<_, _, Infallible> = Reflector::new();
//!
//! reflector.register_type_with_merger::<i32>(
//!     "i32".to_string(),
//!     // if found type `i32` multiple time, fold it by adding them together
//!     &((|src, dst| {
//!         *src += dst;
//!         Ok(())
//!     }) as MergerFn<_, _>),
//! );
//! reflector.register_type::<bool>("bool".to_string());
//!
//! let first_json =
//!     serde_json::to_string(&first.as_serializable(&reflector)).unwrap();
//! let second_json =
//!     serde_json::to_string(&second.as_serializable(&reflector)).unwrap();
//!
//! // create the storage that will be deserialized in-place
//! let final_storage = Storage::default();
//!
//! let inplace_deserializer_seed =
//!     final_storage.as_inplace_deserializer(&reflector);
//!
//! let mut first_deserializer =
//!     serde_json::Deserializer::from_str(&first_json);
//! let mut second_deserializer =
//!     serde_json::Deserializer::from_str(&second_json);
//!
//! inplace_deserializer_seed.deserialize(&mut first_deserializer).unwrap();
//! inplace_deserializer_seed.deserialize(&mut second_deserializer).unwrap();
//!
//! assert_eq!(*final_storage.get::<i32>("myNumber".to_string()).unwrap(), 3);
//! assert_eq!(
//!     *final_storage.get::<bool>("myBoolean".to_string()).unwrap(),
//!     true
//! );
//! ```

use std::{
    any::{Any, TypeId},
    collections::{hash_map::Entry, HashMap, HashSet},
    convert::Infallible,
    fmt::{Debug, Display},
    hash::Hash,
};

use derive_new::new;
use serde::{de::Error, ser::SerializeMap};

use crate::Storage;

#[derive(Debug)]
struct SerializationMetaData<T> {
    #[allow(clippy::type_complexity)]
    serialize_fn: fn(&dyn Any, &mut dyn FnMut(&dyn erased_serde::Serialize)),
    tag: T,
}

#[derive(Debug)]
struct DeserializationMetaData {
    #[allow(clippy::type_complexity)]
    deserialize_fn: fn(
        &mut dyn erased_serde::Deserializer,
    ) -> Result<Box<dyn Any>, erased_serde::Error>,

    merger_fn: &'static dyn Any,

    #[allow(clippy::type_complexity)]
    final_merger_fn: fn(
        &dyn Any,
        &mut dyn Any,
        &mut dyn erased_serde::Deserializer,
    ) -> Result<(), erased_serde::Error>,

    type_id: TypeId,
}

/// Uesd to serialize/deserialize the [`Storage`] struct.
#[derive(Debug)]
pub struct Reflector<T, ID, E = Infallible> {
    serialization_meta_datas: HashMap<TypeId, SerializationMetaData<T>>,
    deserialization_meta_datas: HashMap<T, DeserializationMetaData>,

    _phantom: std::marker::PhantomData<(ID, E)>,
}

impl<T, ID, E> Default for Reflector<T, ID, E> {
    fn default() -> Self {
        Self {
            serialization_meta_datas: HashMap::new(),
            deserialization_meta_datas: HashMap::new(),

            _phantom: std::marker::PhantomData,
        }
    }
}

/// The function pointer type used to merge two components of the same type.
pub type MergerFn<T, E> = fn(&mut T, T) -> Result<(), E>;

impl<T, ID, E: Display + 'static> Reflector<T, ID, E> {
    /// Create a new instance of [`Reflector`].
    #[must_use]
    pub fn new() -> Self {
        Self {
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
        #[allow(clippy::unnecessary_wraps)]
        fn no_op_merger_fn<C, E>(_: &mut C, _: C) -> Result<(), E> { Ok(()) }

        self.register_type_with_merger(
            tag,
            &(no_op_merger_fn::<C, E> as MergerFn<_, _>),
        )
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
    pub fn register_type_with_merger<
        C: Any + serde::Serialize + for<'x> serde::Deserialize<'x>,
    >(
        &mut self,
        tag: T,
        merger: &'static MergerFn<C, E>,
    ) -> bool
    where
        T: Clone + Eq + Hash,
    {
        fn deserialize_fn<
            C: Any + serde::Serialize + for<'x> serde::Deserialize<'x>,
        >(
            deserializer: &mut dyn erased_serde::Deserializer,
        ) -> Result<Box<dyn Any>, erased_serde::Error> {
            let value = C::deserialize(deserializer)?;
            Ok(Box::new(value))
        }

        fn serialize_fn<
            C: Any + serde::Serialize + for<'x> serde::Deserialize<'x>,
        >(
            component: &dyn Any,
            serializer: &mut dyn FnMut(&dyn erased_serde::Serialize),
        ) {
            let component = component.downcast_ref::<C>().unwrap();
            serializer(component);
        }

        fn final_inplace_merger_fn<
            C: Any + serde::Serialize + for<'x> serde::Deserialize<'x>,
            E: Display + 'static,
        >(
            merger_fn: &dyn Any,
            source: &mut dyn Any,
            deserializer: &mut dyn erased_serde::Deserializer,
        ) -> Result<(), erased_serde::Error> {
            println!("merging: {}", std::any::type_name::<C>());
            let merger_fn = merger_fn.downcast_ref::<MergerFn<C, E>>().unwrap();
            let source = source.downcast_mut::<C>().unwrap();
            let value = C::deserialize(deserializer)?;

            merger_fn(source, value).map_err(erased_serde::Error::custom)
        }

        let (Entry::Vacant(ser), Entry::Vacant(de)) = (
            self.serialization_meta_datas.entry(TypeId::of::<C>()),
            self.deserialization_meta_datas.entry(tag.clone()),
        ) else {
            return false;
        };

        ser.insert(SerializationMetaData {
            serialize_fn: serialize_fn::<C>,
            tag,
        });
        de.insert(DeserializationMetaData {
            deserialize_fn: deserialize_fn::<C>,
            merger_fn: merger,
            final_merger_fn: final_inplace_merger_fn::<C, E>,
            type_id: TypeId::of::<C>(),
        });

        true
    }
}

/// The struct that enables the serialization of the [`Storage`] struct.
#[derive(Debug, new)]
pub struct SerializableStorage<'a, T, ID: Eq + Hash, E: Display + 'static> {
    storage: &'a Storage<ID>,
    reflector: &'a Reflector<T, ID, E>,
}

impl<
        'a,
        T: serde::Serialize,
        ID: serde::Serialize + Eq + Hash + Clone,
        E: Display + 'static,
    > serde::Serialize for SerializableStorage<'a, T, ID, E>
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut type_ids_by_id = HashMap::<_, HashSet<_>>::new();

        for entry in &self.storage.components {
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

struct SerializeEntry<'a, T, ID: Eq + Hash, E: Display + 'static> {
    id: ID,
    type_ids: HashSet<TypeId>,

    reflector: &'a Reflector<T, ID, E>,
    storage: &'a Storage<ID>,
}

impl<'a, T: serde::Serialize, ID: Eq + Hash + Clone, E: Display + 'static>
    serde::Serialize for SerializeEntry<'a, T, ID, E>
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

    #[allow(clippy::type_complexity)]
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
        're,
        T: for<'x> serde::Deserialize<'x> + Eq + Hash + std::fmt::Debug,
        ID: Eq + Hash + Clone + for<'x> serde::Deserialize<'x>,
        E: Display + 'static,
    > serde::de::DeserializeSeed<'de> for &'re Reflector<T, ID, E>
{
    type Value = Storage<ID>;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_map(EntitiyMapVisitor { reflector: self })
    }
}

struct EntitiyMapVisitor<'a, T, ID, E: Display + 'static> {
    reflector: &'a Reflector<T, ID, E>,
}

impl<
        'de,
        T: for<'x> serde::Deserialize<'x> + Eq + Hash + std::fmt::Debug,
        ID: Eq + Hash + Clone + for<'x> serde::Deserialize<'x>,
        E: Display + 'static,
    > serde::de::Visitor<'de> for EntitiyMapVisitor<'_, T, ID, E>
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
                            "duplicate component tag: {tag:?}",
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
        E: Display + 'static,
    > serde::de::DeserializeSeed<'de> for ComponentMapVisitor<'_, T, ID, E>
{
    type Value = HashMap<T, Box<dyn Any>>;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_map(self)
    }
}

struct ComponentMapVisitor<'a, T, ID, E: Display + 'static> {
    reflector: &'a Reflector<T, ID, E>,
}

impl<
        'de,
        'a,
        T: for<'x> serde::Deserialize<'x> + Eq + Hash + std::fmt::Debug,
        ID: Eq + Hash + Clone + for<'x> serde::Deserialize<'x>,
        E: Display + 'static,
    > serde::de::Visitor<'de> for ComponentMapVisitor<'a, T, ID, E>
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
                    "no deserialization meta data found for tag: {key:?}",
                )));
            };

            let component = map.next_value_seed(ComponentDeserialzer {
                deserialize_fn: deserialize_meta.deserialize_fn,
            })?;

            if components.contains_key(&key) {
                return Err(serde::de::Error::custom(format!(
                    "duplicate component tag: {key:?}",
                )));
            }

            assert!(components.insert(key, component).is_none());
        }

        Ok(components)
    }
}

struct ComponentDeserialzer {
    #[allow(clippy::type_complexity)]
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

/// A data structure used for in-place-incremental deserialization for the
/// storage.
///
/// See [`Storage::as_inplace_deserializer`].
#[derive(Debug, new)]
pub struct InplaceDeserializer<
    'a,
    T,
    ID: Eq + Hash,
    E: Display + 'static = Infallible,
> {
    storage: &'a Storage<ID>,
    reflector: &'a Reflector<T, ID, E>,
}

impl<T, ID: Eq + Hash, E: Display + 'static> Clone
    for InplaceDeserializer<'_, T, ID, E>
{
    fn clone(&self) -> Self {
        Self { reflector: self.reflector, storage: self.storage }
    }
}

impl<
        'de,
        T: for<'x> serde::Deserialize<'x> + Eq + Hash + Debug,
        ID: for<'x> serde::Deserialize<'x> + Eq + Hash + Clone,
        E: Display + 'static,
    > serde::de::DeserializeSeed<'de> for &InplaceDeserializer<'_, T, ID, E>
{
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_map(self)
    }
}

impl<
        'de,
        T: for<'x> serde::Deserialize<'x> + Eq + Hash + Debug,
        ID: for<'x> serde::Deserialize<'x> + Eq + Hash + Clone,
        E: Display + 'static,
    > serde::de::Visitor<'de> for &InplaceDeserializer<'_, T, ID, E>
{
    type Value = ();

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
        while let Some(id) = map.next_key::<ID>()? {
            map.next_value_seed(InplaceComponentMapVisitor {
                inplace_deserializer: self.clone(),
                entity_id: id,
            })?;
        }

        Ok(())
    }
}

struct InplaceComponentMapVisitor<'a, T, ID: Eq + Hash, E: Display + 'static> {
    inplace_deserializer: InplaceDeserializer<'a, T, ID, E>,
    entity_id: ID,
}

impl<
        'de,
        T: for<'x> serde::Deserialize<'x> + Eq + Hash + Debug,
        ID: for<'x> serde::Deserialize<'x> + Eq + Hash + Clone,
        E: Display + 'static,
    > serde::de::DeserializeSeed<'de>
    for InplaceComponentMapVisitor<'_, T, ID, E>
{
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_map(self)
    }
}

impl<
        'de,
        T: for<'x> serde::Deserialize<'x> + Eq + Hash + Debug,
        ID: for<'x> serde::Deserialize<'x> + Eq + Hash + Clone,
        E: Display + 'static,
    > serde::de::Visitor<'de> for InplaceComponentMapVisitor<'_, T, ID, E>
{
    type Value = ();

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
        while let Some(tag) = map.next_key::<T>()? {
            let Some(deserialize_meta) = self
                .inplace_deserializer
                .reflector
                .deserialization_meta_datas
                .get(&tag)
            else {
                return Err(serde::de::Error::custom(format!(
                    "no deserialization meta data found for tag: {tag:?}",
                )));
            };

            match self
                .inplace_deserializer
                .storage
                .components
                .entry((self.entity_id.clone(), deserialize_meta.type_id))
            {
                dashmap::Entry::Occupied(mut occupied_entry) => {
                    map.next_value_seed(InplaceComponentDeserializer {
                        source: &mut **occupied_entry.get_mut(),
                        merger_fn: deserialize_meta.merger_fn,
                        final_merger_fn: deserialize_meta.final_merger_fn,
                    })?;
                }
                dashmap::Entry::Vacant(vacant_entry) => {
                    let value = map.next_value_seed(ComponentDeserialzer {
                        deserialize_fn: deserialize_meta.deserialize_fn,
                    })?;

                    vacant_entry.insert(value);
                }
            }
        }

        Ok(())
    }
}

struct InplaceComponentDeserializer<'current> {
    source: &'current mut dyn Any,
    merger_fn: &'static dyn Any,

    #[allow(clippy::type_complexity)]
    final_merger_fn: fn(
        &dyn Any,
        &mut dyn Any,
        &mut dyn erased_serde::Deserializer,
    ) -> Result<(), erased_serde::Error>,
}

impl<'de, 'current> serde::de::DeserializeSeed<'de>
    for InplaceComponentDeserializer<'current>
{
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;

        let mut erased = <dyn erased_serde::Deserializer>::erase(deserializer);

        (self.final_merger_fn)(self.merger_fn, self.source, &mut erased)
            .map_err(D::Error::custom)
    }
}
