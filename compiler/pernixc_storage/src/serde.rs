//! The `serde` support for the `pernixc_storage` crate.
//!
//! # Example
//!
//! ``` rust
//! use std::convert::Infallible;
//!
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
//! let serializable = storage.as_serializable::<_, Infallible>(&reflector);
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
//!     BoxTrait, Storage,
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
//! let mut reflector: Reflector<_, BoxTrait, _, Infallible> = Reflector::new();
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
//! ## Serialize With Filter
//!
//! ```rust
//! use std::convert::Infallible;
//!
//! use pernixc_storage::{serde::Reflector, BoxTrait, Storage};
//!
//! let storage = Storage::<_, BoxTrait>::default();
//!
//! storage.add_component("a".to_string(), 1);
//! storage.add_component("b".to_string(), 2);
//! storage.add_component("c".to_string(), 3);
//!
//! let mut reflector = Reflector::new();
//!
//! reflector.register_type::<i32>("i32".to_string());
//!
//! reflector.set_filter(|id| id == "a");
//!
//! let serializable = storage.as_serializable::<_, Infallible>(&reflector);
//!
//! let serialized = serde_json::to_value(&serializable).unwrap();
//!
//! assert_eq!(serialized["a"]["i32"], 1);
//!
//! assert!(serialized.get("b").is_none());
//! assert!(serialized.get("c").is_none());
//! ```

use std::{
    any::{Any, TypeId},
    collections::{hash_map::Entry, HashMap, HashSet},
    convert::Infallible,
    fmt::{Debug, Display},
    hash::Hash,
    ops::Deref,
};

use derive_new::new;
use serde::{de::Error, ser::SerializeMap};

use crate::{Ptr, Storage};

#[derive(Debug)]
struct SerializationMetaData<T> {
    #[allow(clippy::type_complexity)]
    serialize_fn: fn(&dyn Any, &mut dyn FnMut(&dyn erased_serde::Serialize)),
    tag: T,
}

#[derive(Debug)]
struct DeserializationMetaData<P: Ptr> {
    #[allow(clippy::type_complexity)]
    deserialize_fn:
        fn(
            &mut dyn erased_serde::Deserializer,
        )
            -> Result<P::Wrap<dyn Any + Send + Sync>, erased_serde::Error>,

    collision_handling: CollisionHandling,

    type_id: TypeId,
}

/// Uesd to serialize/deserialize the [`Storage`] struct.
#[allow(missing_debug_implementations, clippy::type_complexity)]
pub struct Reflector<ID, P: Ptr, T, E = Infallible> {
    serialization_meta_datas: HashMap<TypeId, SerializationMetaData<T>>,
    deserialization_meta_datas: HashMap<T, DeserializationMetaData<P>>,
    symbol_filter: Option<Box<dyn Fn(&ID) -> bool>>,

    _phantom: std::marker::PhantomData<(ID, E)>,
}

impl<ID, P: Ptr, T, E> Default for Reflector<ID, P, T, E> {
    fn default() -> Self {
        Self {
            serialization_meta_datas: HashMap::new(),
            deserialization_meta_datas: HashMap::new(),
            symbol_filter: None,

            _phantom: std::marker::PhantomData,
        }
    }
}

/// The function pointer type used to merge two components of the same type.
pub type MergerFn<T, E> = fn(&mut T, T) -> Result<(), E>;

#[derive(Debug)]
enum CollisionHandling {
    Compare(
        fn(
            &dyn Any,
            &mut dyn erased_serde::Deserializer,
        ) -> Result<bool, erased_serde::Error>,
    ),
    Merge {
        merger_fn: &'static dyn Any,

        #[allow(clippy::type_complexity)]
        final_merger_fn: fn(
            &dyn Any,
            &mut dyn Any,
            &mut dyn erased_serde::Deserializer,
        ) -> Result<(), erased_serde::Error>,
    },
}

impl<ID, P: Ptr, T, E: Display + 'static> Reflector<ID, P, T, E> {
    /// Create a new instance of [`Reflector`].
    #[must_use]
    pub fn new() -> Self {
        Self {
            serialization_meta_datas: HashMap::new(),
            deserialization_meta_datas: HashMap::new(),
            symbol_filter: None,

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
        C: Eq
            + Any
            + Send
            + Sync
            + serde::Serialize
            + for<'x> serde::Deserialize<'x>,
    >(
        &mut self,
        tag: T,
    ) -> bool
    where
        T: Clone + Eq + Hash,
    {
        fn compare<C: Any + Eq + for<'x> serde::Deserialize<'x>>(
            first: &dyn Any,
            deserializer: &mut dyn erased_serde::Deserializer,
        ) -> Result<bool, erased_serde::Error> {
            let value = C::deserialize(deserializer)?;
            Ok(first.downcast_ref::<C>().unwrap() == &value)
        }

        self.register_type_internal::<C>(
            tag,
            CollisionHandling::Compare(compare::<C>),
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
        C: Any + Send + Sync + serde::Serialize + for<'x> serde::Deserialize<'x>,
    >(
        &mut self,
        tag: T,
        merger: &'static MergerFn<C, E>,
    ) -> bool
    where
        T: Clone + Eq + Hash,
    {
        fn final_inplace_merger_fn<
            C: Any + serde::Serialize + for<'x> serde::Deserialize<'x>,
            E: Display + 'static,
        >(
            merger_fn: &dyn Any,
            source: &mut dyn Any,
            deserializer: &mut dyn erased_serde::Deserializer,
        ) -> Result<(), erased_serde::Error> {
            let merger_fn = merger_fn.downcast_ref::<MergerFn<C, E>>().unwrap();
            let source = source.downcast_mut::<C>().unwrap();
            let value = C::deserialize(deserializer)?;

            merger_fn(source, value).map_err(erased_serde::Error::custom)
        }

        self.register_type_internal::<C>(tag, CollisionHandling::Merge {
            merger_fn: merger as _,
            final_merger_fn: final_inplace_merger_fn::<C, E>,
        })
    }

    #[must_use]
    fn register_type_internal<
        C: Any + Send + Sync + serde::Serialize + for<'x> serde::Deserialize<'x>,
    >(
        &mut self,
        tag: T,
        collision_handling: CollisionHandling,
    ) -> bool
    where
        T: Clone + Eq + Hash,
    {
        fn deserialize_fn<
            P: Ptr,
            C: Any
                + Send
                + Sync
                + serde::Serialize
                + for<'x> serde::Deserialize<'x>,
        >(
            deserializer: &mut dyn erased_serde::Deserializer,
        ) -> Result<P::Wrap<dyn Any + Send + Sync>, erased_serde::Error>
        {
            let value = C::deserialize(deserializer)?;
            Ok(P::wrap(value))
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
            deserialize_fn: deserialize_fn::<P, C>,
            collision_handling,
            type_id: TypeId::of::<C>(),
        });

        true
    }

    /// Set a filter to only serialize the components that satisfy the filter.
    /// The filter is applied to the entity id.
    pub fn set_filter(&mut self, filter: impl Fn(&ID) -> bool + 'static) {
        self.symbol_filter = Some(Box::new(filter));
    }
}

/// The struct that enables the serialization of the [`Storage`] struct.
#[derive(new)]
#[allow(missing_debug_implementations)]
pub struct SerializableStorage<
    'a,
    ID: Eq + Hash,
    P: Ptr,
    T,
    E: Display + 'static,
> {
    storage: &'a Storage<ID, P>,
    reflector: &'a Reflector<ID, P, T, E>,
}

impl<
        ID: serde::Serialize + Eq + Hash + Clone,
        P: Ptr,
        T: serde::Serialize,
        E: Display + 'static,
    > serde::Serialize for SerializableStorage<'_, ID, P, T, E>
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut type_ids_by_id = HashMap::<_, HashSet<_>>::new();

        for entry in &self.storage.components {
            if let Some(filter) = &self.reflector.symbol_filter {
                if !filter(&entry.key().0) {
                    continue;
                }
            }

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

struct SerializeEntry<'a, ID: Eq + Hash, P: Ptr, T, E: Display + 'static> {
    id: ID,
    type_ids: HashSet<TypeId>,

    reflector: &'a Reflector<ID, P, T, E>,
    storage: &'a Storage<ID, P>,
}

impl<
        ID: Eq + Hash + Clone,
        P: Ptr,
        T: serde::Serialize,
        E: Display + 'static,
    > serde::Serialize for SerializeEntry<'_, ID, P, T, E>
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

            map.serialize_entry(
                &serialize_meta.tag,
                &SerializeComponent::<P> {
                    component: &component,
                    serialize_fn: serialize_meta.serialize_fn,
                },
            )?;
        }

        map.end()
    }
}

struct SerializeComponent<'a, P: Ptr> {
    component: &'a P::Wrap<dyn Any + Send + Sync>,

    #[allow(clippy::type_complexity)]
    serialize_fn: fn(&dyn Any, &mut dyn FnMut(&dyn erased_serde::Serialize)),
}

impl<P: Ptr> serde::Serialize for SerializeComponent<'_, P> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut result = None;
        let mut serializer = Some(serializer);

        (self.serialize_fn)(&**self.component, &mut |x| {
            result =
                Some(erased_serde::serialize(x, serializer.take().unwrap()));
        });

        result.unwrap()
    }
}

impl<
        'de,
        ID: Eq + Hash + Clone + for<'x> serde::Deserialize<'x>,
        P: Ptr,
        T: for<'x> serde::Deserialize<'x> + Eq + Hash + std::fmt::Debug,
        E: Display + 'static,
    > serde::de::DeserializeSeed<'de> for &Reflector<ID, P, T, E>
{
    type Value = Storage<ID, P>;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_map(EntitiyMapVisitor { reflector: self })
    }
}

struct EntitiyMapVisitor<'a, ID, P: Ptr, T, E: Display + 'static> {
    reflector: &'a Reflector<ID, P, T, E>,
}

impl<
        'de,
        ID: Eq + Hash + Clone + for<'x> serde::Deserialize<'x>,
        P: Ptr,
        T: for<'x> serde::Deserialize<'x> + Eq + Hash + std::fmt::Debug,
        E: Display + 'static,
    > serde::de::Visitor<'de> for EntitiyMapVisitor<'_, ID, P, T, E>
{
    type Value = Storage<ID, P>;

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
        let storage = Storage::<ID, P>::default();

        while let Some(id) = map.next_key::<ID>()? {
            let components = map.next_value_seed(ComponentMapVisitor {
                reflector: self.reflector,
            })?;

            for (tag, component) in components {
                let type_id = component.deref().type_id();

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
        ID: Eq + Hash + Clone + for<'x> serde::Deserialize<'x>,
        P: Ptr,
        T: for<'x> serde::Deserialize<'x> + Eq + Hash + std::fmt::Debug,
        E: Display + 'static,
    > serde::de::DeserializeSeed<'de> for ComponentMapVisitor<'_, ID, P, T, E>
{
    type Value = HashMap<T, P::Wrap<dyn Any + Send + Sync>>;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_map(self)
    }
}

struct ComponentMapVisitor<'a, ID, P: Ptr, T, E: Display + 'static> {
    reflector: &'a Reflector<ID, P, T, E>,
}

impl<
        'de,
        ID: Eq + Hash + Clone + for<'x> serde::Deserialize<'x>,
        P: Ptr,
        T: for<'x> serde::Deserialize<'x> + Eq + Hash + std::fmt::Debug,
        E: Display + 'static,
    > serde::de::Visitor<'de> for ComponentMapVisitor<'_, ID, P, T, E>
{
    type Value = HashMap<T, P::Wrap<dyn Any + Send + Sync>>;

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

            let component = map.next_value_seed(ComponentDeserialzer::<P> {
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

struct ComponentDeserialzer<P: Ptr> {
    #[allow(clippy::type_complexity)]
    deserialize_fn:
        fn(
            &mut dyn erased_serde::Deserializer,
        )
            -> Result<P::Wrap<dyn Any + Send + Sync>, erased_serde::Error>,
}

impl<'de, P: Ptr> serde::de::DeserializeSeed<'de> for ComponentDeserialzer<P> {
    type Value = P::Wrap<dyn Any + Send + Sync>;

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
#[derive(new)]
#[allow(missing_debug_implementations)]
pub struct InplaceDeserializer<
    'a,
    ID: Eq + Hash,
    P: Ptr,
    T,
    E: Display + 'static = Infallible,
> {
    storage: &'a Storage<ID, P>,
    reflector: &'a Reflector<ID, P, T, E>,
}

impl<ID: Eq + Hash, P: Ptr, T, E: Display + 'static> Clone
    for InplaceDeserializer<'_, ID, P, T, E>
{
    fn clone(&self) -> Self {
        Self { reflector: self.reflector, storage: self.storage }
    }
}

impl<
        'de,
        ID: for<'x> serde::Deserialize<'x> + Eq + Hash + Clone,
        P: Ptr,
        T: for<'x> serde::Deserialize<'x> + Eq + Hash + Debug,
        E: Display + 'static,
    > serde::de::DeserializeSeed<'de>
    for &InplaceDeserializer<'_, ID, P, T, E>
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
        ID: for<'x> serde::Deserialize<'x> + Eq + Hash + Clone,
        P: Ptr,
        T: for<'x> serde::Deserialize<'x> + Eq + Hash + Debug,
        E: Display + 'static,
    > serde::de::Visitor<'de> for &InplaceDeserializer<'_, ID, P, T, E>
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

struct InplaceComponentMapVisitor<
    'a,
    ID: Eq + Hash,
    P: Ptr,
    T,
    E: Display + 'static,
> {
    inplace_deserializer: InplaceDeserializer<'a, ID, P, T, E>,
    entity_id: ID,
}

impl<
        'de,
        ID: for<'x> serde::Deserialize<'x> + Eq + Hash + Clone,
        P: Ptr,
        T: for<'x> serde::Deserialize<'x> + Eq + Hash + Debug,
        E: Display + 'static,
    > serde::de::DeserializeSeed<'de>
    for InplaceComponentMapVisitor<'_, ID, P, T, E>
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
        ID: for<'x> serde::Deserialize<'x> + Eq + Hash + Clone,
        P: Ptr,
        T: for<'x> serde::Deserialize<'x> + Eq + Hash + Debug,
        E: Display + 'static,
    > serde::de::Visitor<'de> for InplaceComponentMapVisitor<'_, ID, P, T, E>
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
                    match deserialize_meta.collision_handling {
                        CollisionHandling::Compare(compare_fn) => {
                            map.next_value_seed(InplaceComponentComparer {
                                source: &**occupied_entry.get(),
                                compare_fn,
                            })?;
                        }
                        CollisionHandling::Merge {
                            merger_fn,
                            final_merger_fn,
                        } => {
                            map.next_value_seed(InplaceComponentMerger {
                                source: P::try_get_mut(
                                    occupied_entry.get_mut(),
                                )
                                .ok_or_else(|| {
                                    serde::de::Error::custom(
                                        "failed to obtain mutable reference \
                                         to merge the component",
                                    )
                                })?,
                                merger_fn,
                                final_merger_fn,
                            })?;
                        }
                    }
                }
                dashmap::Entry::Vacant(vacant_entry) => {
                    let value =
                        map.next_value_seed(ComponentDeserialzer::<P> {
                            deserialize_fn: deserialize_meta.deserialize_fn,
                        })?;

                    vacant_entry.insert(value);
                }
            }
        }

        Ok(())
    }
}

struct InplaceComponentComparer<'current> {
    source: &'current dyn Any,
    compare_fn: fn(
        &dyn Any,
        &mut dyn erased_serde::Deserializer,
    ) -> Result<bool, erased_serde::Error>,
}

impl<'de> serde::de::DeserializeSeed<'de> for InplaceComponentComparer<'_> {
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;

        let mut erased = <dyn erased_serde::Deserializer>::erase(deserializer);

        if !(self.compare_fn)(self.source, &mut erased)
            .map_err(D::Error::custom)?
        {
            return Err(D::Error::custom("found incompatible component"));
        }

        Ok(())
    }
}

struct InplaceComponentMerger<'current> {
    source: &'current mut dyn Any,
    merger_fn: &'static dyn Any,

    #[allow(clippy::type_complexity)]
    final_merger_fn: fn(
        &dyn Any,
        &mut dyn Any,
        &mut dyn erased_serde::Deserializer,
    ) -> Result<(), erased_serde::Error>,
}

impl<'de> serde::de::DeserializeSeed<'de> for InplaceComponentMerger<'_> {
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
