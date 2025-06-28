//! Dynamic serialization and deserialization framework for heterogeneous
//! collections.
//!
//! This module provides a flexible framework for serializing and deserializing
//! collections that contain values of different types determined at runtime.
//! It's designed to work with the query system's `Map` and `DynamicBox` types,
//! enabling persistence and transfer of complex, type-erased data structures.
//!
//! # Overview
//!
//! The core challenge this module addresses is serializing/deserializing
//! heterogeneous collections where the concrete types are only known at runtime
//! through stable type IDs. Traditional serialization approaches require
//! compile-time knowledge of all types, but this system uses a registry-based
//! approach with function pointers to handle dynamic typing.
//!
//! # Key Components
//!
//! ## Traits
//!
//! * [`DynamicSerialize`] - Provides access to serialization helpers indexed by
//!   type ID
//! * [`DynamicDeserialize`] - Provides access to deserialization helpers
//!   indexed by type ID
//! * [`DynamicRegistry`] - Interface for registering types with both
//!   capabilities
//!
//! ## Helper Structs
//!
//! * [`SerializationHelper`] - Contains function pointers for serializing a
//!   specific type
//! * [`DeserializationHelper`] - Contains function pointers for deserializing a
//!   specific type
//!
//! ## Registry Types
//!
//! * [`Registry`] - Main registry that manages helpers for multiple types
//! * [`SelfRegistry`] - Self-referential registry that can be used as an
//!   extension parameter
//!
//! # Architecture
//!
//! The system works by:
//!
//! 1. **Registration**: Types are registered with a `Registry` using
//!    [`Registry::register`]
//! 2. **Helper Creation**: The registry creates serialization/deserialization
//!    function pointers
//! 3. **Runtime Lookup**: During serialization/deserialization, helpers are
//!    looked up by type ID
//! 4. **Dynamic Dispatch**: The appropriate function pointer is called to
//!    handle the specific type
//!
//! # Serialization Format
//!
//! Maps are serialized as nested structures:
//! ```text
//! {
//!   "type_id_1": {
//!     "key1": "value1",
//!     "key2": "value2",
//!     ...
//!   },
//!   "type_id_2": {
//!     "key1": "value1",
//!     ...
//!   }
//! }
//! ```
//!
//! Dynamic boxes are serialized as structs with type information:
//! ```text
//! {
//!   "stable_type_id": "<type_id>",
//!   "value": <serialized_value>
//! }
//! ```
//!
//! # Usage Example
//!
//! ```rust,ignore
//! use pernixc_serialize::{Serialize, Deserialize};
//!
//! // Create a registry
//! let mut registry = Registry::new();
//!
//! // Register your key types
//! registry.register::<MyKey>();
//! registry.register::<AnotherKey>();
//!
//! // Use for serialization/deserialization
//! let map = Map::new();
//! let serialized = map.serialize(&mut serializer, &registry)?;
//! ```
//!
//! # Type Safety
//!
//! While this system provides dynamic typing capabilities, it maintains type
//! safety through:
//! * Stable type IDs that uniquely identify types across compilation units
//! * Runtime type checking during downcasting operations
//! * Panic-on-mismatch semantics for registration conflicts
//!
//! # Performance Considerations
//!
//! * Function pointer calls have minimal overhead compared to trait object
//!   dispatch
//! * Type lookups use efficient hash maps with stable type IDs as keys
//! * Registration is typically done once at startup, not in hot paths

use std::any::Any;

use getset::Getters;
use pernixc_hash::{DashMap, HashMap, HashSet};
use pernixc_serialize::{
    de::{Deserializer, MapAccess, StructAccess},
    ser::{Map as _, Serializer},
    Deserialize, Serialize,
};
use pernixc_stable_type_id::StableTypeID;
use smallbox::smallbox;

use crate::{
    database::{call_graph::VersionInfo, map::Map},
    key::DynamicBox,
    Key,
};

/// A trait for types that can provide dynamic serialization capabilities.
///
/// This trait enables types to serialize dynamic content by providing access
/// to serialization helpers indexed by stable type IDs. It's primarily used
/// for serializing heterogeneous collections where the concrete types are
/// determined at runtime.
pub trait DynamicSerialize<S: Serializer<Self>>: Sized {
    /// Returns a mapping from stable type IDs to their corresponding
    /// serialization helpers.
    ///
    /// # Returns
    /// A reference to a `HashMap` mapping `StableTypeID` to
    /// `SerializationHelper<S, Self>`. The serialization helpers contain
    /// function pointers for serializing specific types.
    fn serialization_helper_by_type_id(
        &self,
    ) -> &HashMap<StableTypeID, SerializationHelper<S, Self>>;
}

/// A trait for types that can provide dynamic deserialization capabilities.
///
/// This trait enables types to deserialize dynamic content by providing access
/// to deserialization helpers indexed by stable type IDs. It's primarily used
/// for deserializing heterogeneous collections where the concrete types are
/// determined at runtime.
pub trait DynamicDeserialize<D: Deserializer<Self>>: Sized {
    /// Returns a mapping from stable type IDs to their corresponding
    /// deserialization helpers.
    ///
    /// # Returns
    /// A reference to a `HashMap` mapping `StableTypeID` to
    /// `DeserializationHelper<D, Self>`. The deserialization helpers
    /// contain function pointers for deserializing specific types.
    fn deserialization_helper_by_type_id(
        &self,
    ) -> &HashMap<StableTypeID, DeserializationHelper<D, Self>>;
}

/// A trait for types that can register serialization and deserialization
/// helpers for dynamic types.
///
/// This trait provides a unified interface for registering types with both
/// serialization and deserialization capabilities. It's typically implemented
/// by registry types that manage collections of type-specific helpers.
pub trait DynamicRegistry<S: Serializer<Self>, D: Deserializer<Self>>:
    Sized
{
    /// Registers a key type and its associated value type for both
    /// serialization and deserialization.
    ///
    /// This method creates and stores serialization and deserialization helpers
    /// for the given key type `K` and its associated value type `K::Value`.
    /// Both types must implement the appropriate serialization and
    /// deserialization traits.
    ///
    /// # Type Parameters
    /// * `K` - The key type that implements `Key`, `Serialize`, and
    ///   `Deserialize`
    ///
    /// # Requirements
    /// * `K::Value` must implement `Serialize<S, Self>` and `Deserialize<D,
    ///   Self>`
    ///
    /// # Panics
    /// May panic if the type has already been registered.
    fn register<K: Key + Serialize<S, Self> + Deserialize<D, Self>>(&mut self)
    where
        K::Value: Serialize<S, Self> + Deserialize<D, Self>,
        S::Error: Send + Sync,
        Self: Send + Sync;
}

/// A helper struct that contains function pointers for serializing a specific
/// type.
///
/// This struct stores the necessary function pointers to serialize both `Map`
/// instances and `DynamicBox` instances for a specific type. It also stores the
/// standard library's `TypeId` for runtime type checking.
#[derive(Debug, Copy)]
#[allow(clippy::type_complexity)]
pub struct SerializationHelper<S: Serializer<E>, E> {
    map_serializer: fn(&Map, &mut S, &E) -> Result<(), S::Error>,
    dynamic_box_serializer: fn(&DynamicBox, &mut S, &E) -> Result<(), S::Error>,
    value_serializer: fn(&dyn Any, &mut S, &E) -> Result<(), S::Error>,
    cas_map_serializer: fn(
        &Map,
        &(dyn Send + Sync + Fn() -> Result<S, S::Error>),
        &E,
        &(dyn Send + Sync + Fn(S, u128) -> Result<(), S::Error>),
    ) -> Result<bool, S::Error>,

    dependency_graph_serializer: fn(
        &[(&DynamicBox, &HashSet<DynamicBox>)],
        &mut S,
        &E,
    ) -> Result<(), S::Error>,
    version_info_serializer:
        fn(&[(&DynamicBox, &VersionInfo)], &mut S, &E) -> Result<(), S::Error>,

    std_type_id: std::any::TypeId,
}

impl<S: Serializer<E>, E> SerializationHelper<S, E> {
    pub(crate) fn serialize_any_value(
        &self,
        value: &dyn Any,
        serializer: &mut S,
        extension: &E,
    ) -> Result<(), S::Error> {
        (self.value_serializer)(value, serializer, extension)
    }

    pub(crate) fn serialize_fingerprint_map(
        &self,
        map: &Map,
        create_serializer: &(dyn Send + Sync + Fn() -> Result<S, S::Error>),
        extension: &E,
        post_serialize: &(dyn Send
              + Sync
              + Fn(S, u128) -> Result<(), S::Error>),
    ) -> Result<bool, S::Error> {
        (self.cas_map_serializer)(
            map,
            create_serializer,
            extension,
            post_serialize,
        )
    }

    pub(crate) fn serialize_dependency_graph(
        &self,
        entries: &[(&DynamicBox, &HashSet<DynamicBox>)],
        serializer: &mut S,
        extension: &E,
    ) -> Result<(), S::Error> {
        (self.dependency_graph_serializer)(entries, serializer, extension)
    }

    pub(crate) fn serialize_version_info(
        &self,
        entries: &[(&DynamicBox, &VersionInfo)],
        serializer: &mut S,
        extension: &E,
    ) -> Result<(), S::Error> {
        (self.version_info_serializer)(entries, serializer, extension)
    }
}

/// A helper struct that contains function pointers for deserializing a specific
/// type.
///
/// This struct stores the necessary function pointers to deserialize both `Map`
/// instances and `DynamicBox` instances for a specific type. It also stores the
/// standard library's `TypeId` for runtime type checking.
///
/// The function pointer types are complex due to the lifetime requirements of
/// the deserialization process.
#[derive(Debug, Copy)]
#[allow(clippy::type_complexity)]
pub struct DeserializationHelper<D: Deserializer<E>, E> {
    map_deserializer: for<'m, 'v> fn(
        &mut Map,
        <D::MapAccess<'m> as MapAccess<E>>::ValueAccess<'v>,
        &E,
    ) -> Result<(), D::Error>,

    dynamic_box_deserializer: for<'m, 'v> fn(
        <D::StructAccess<'m> as StructAccess<E>>::FieldAccess<'v>,
        &E,
    )
        -> Result<DynamicBox, D::Error>,
    value_deserializer: fn(&mut dyn Any, &mut D, &E) -> Result<(), D::Error>,

    std_type_id: std::any::TypeId,
}

impl<D: Deserializer<E>, E> DeserializationHelper<D, E> {
    /// Deserializes a [`K::Value`] instance into the [`result_buffer`] which
    /// has a type of [`Option<K::Value>`] that has been downcasted to `Any`.
    pub fn deserialize_any_value(
        &self,
        result_buffer: &mut dyn Any,
        deserializer: &mut D,
        extension: &E,
    ) -> Result<(), D::Error> {
        (self.value_deserializer)(result_buffer, deserializer, extension)
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
        extension: &E,
    ) -> Result<(), <S as Serializer<E>>::Error> {
        (self.helper.map_serializer)(self.map, serializer, extension)
    }
}

impl<S: Serializer<E>, E: DynamicSerialize<S>> Serialize<S, E> for Map {
    fn serialize(
        &self,
        serializer: &mut S,
        extension: &E,
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

                for (stable_type_id, helper) in
                    extension.serialization_helper_by_type_id()
                {
                    // skip this map type
                    if !self.has_type_id(helper.std_type_id) {
                        continue;
                    }

                    map.serialize_entry(
                        &stable_type_id,
                        &SerializableTypedMap { map: self, helper },
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
        extension: &E,
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

/// A registry that manages serialization and deserialization helpers for
/// dynamic types.
///
/// The `Registry` maintains mappings from stable type IDs to their
/// corresponding serialization and deserialization helpers. This enables
/// dynamic serialization and deserialization of heterogeneous collections where
/// the concrete types are determined at runtime.
///
/// # Type Parameters
/// * `S` - The serializer type
/// * `D` - The deserializer type
/// * `E` - The extension type used by both serializer and deserializer
#[derive(Getters)]
pub struct Registry<S: Serializer<E>, D: Deserializer<E>, E> {
    /// Gets the serialization helpers indexed by stable type ID.
    #[get = "pub"]
    serialization_helpers_by_type_id:
        HashMap<StableTypeID, SerializationHelper<S, E>>,

    /// Gets the deserialization helpers indexed by stable type ID.
    #[get = "pub"]
    deserialization_helpers_by_type_id:
        HashMap<StableTypeID, DeserializationHelper<D, E>>,
}

impl<S: Serializer<E>, D: Deserializer<E>, E> Registry<S, D, E> {
    /// Creates a new empty registry.
    ///
    /// # Returns
    /// A new `Registry` instance with empty helper mappings.
    #[must_use]
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
    extension: &E,
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
    extension: &E,
) -> Result<DynamicBox, D::Error> {
    use pernixc_serialize::de::FieldAccess;

    let key: K = value_access.deserialize(extension)?;

    Ok(DynamicBox(smallbox!(key)))
}

impl<S: Serializer<E>, D: Deserializer<E>, E: DynamicSerialize<S>>
    Registry<S, D, E>
{
    /// Registers a key type and its associated value type for both
    /// serialization and deserialization.
    ///
    /// This method creates and stores serialization and deserialization helpers
    /// for the given key type `K` and its associated value type `K::Value`.
    /// The helpers contain function pointers that can serialize/deserialize
    /// both `Map` instances and `DynamicBox` instances containing values of
    /// the registered type.
    ///
    /// # Type Parameters
    /// * `K` - The key type that implements `Key`, `Serialize`, and
    ///   `Deserialize`
    ///
    /// # Requirements
    /// * `K::Value` must implement `Serialize<S, E>` and `Deserialize<D, E>`
    ///
    /// # Panics
    /// Panics if the key type `K` has already been registered in this registry.
    /// Each type can only be registered once per registry instance.
    #[allow(clippy::too_many_lines)]
    pub fn register<K: Key + Serialize<S, E> + Deserialize<D, E>>(&mut self)
    where
        K::Value: Serialize<S, E> + Deserialize<D, E>,
        S::Error: Send + Sync,
        E: Send + Sync,
    {
        use pernixc_serialize::ser::Struct;

        let map_serializer = |map: &Map, serializer: &mut S, extension: &E| {
            map.type_storage::<K, _>(|x| {
                let map = x.expect("should exists");

                map.serialize(serializer, extension)
            })
        };

        let dyn_box_serializer =
            |dyn_box: &DynamicBox, serializer: &mut S, extension: &E| {
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

        let value_serializer = |value: &dyn Any,
                                serializer: &mut S,
                                extension: &E| {
            let value = value.downcast_ref::<K::Value>().unwrap_or_else(|| {
                panic!("should've been `{}`", std::any::type_name::<K::Value>())
            });

            value.serialize(serializer, extension)
        };

        let cas_map_serializer =
            |map: &Map,
             create_serializer: &(dyn Send
                   + Sync
                   + Fn() -> Result<S, S::Error>),
             extension: &E,
             post_serialize: &(dyn Send
                   + Sync
                   + Fn(S, u128) -> Result<(), S::Error>)| {
                map.type_storage::<K, _>(|x| {
                    let Some(x) = x else { return Ok(false) };

                    x.iter()
                        .map(|value| {
                            let value = value.value();
                            let fingerprint =
                                crate::fingerprint::fingerprint(value);
                            let mut serializer = create_serializer()?;

                            value.serialize(&mut serializer, extension)?;
                            post_serialize(serializer, fingerprint)?;

                            Ok(())
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    Ok(true)
                })
            };

        let dependency_graph_serializer =
            |entries: &[(&DynamicBox, &HashSet<DynamicBox>)],
             serializer: &mut S,
             extension: &E| {
                serializer.emit_map(entries.len(), extension, |mut map, _| {
                    for (key, value) in entries {
                        let key = (**key).any().downcast_ref::<K>().expect(
                            "the dynamic box should contain a value of the \
                             registered type",
                        );
                        map.serialize_entry(key, *value, extension)?;
                    }
                    Ok(())
                })
            };

        let version_info_serializer =
            |entries: &[(&DynamicBox, &VersionInfo)],
             serializer: &mut S,
             extension: &E| {
                serializer.emit_map(entries.len(), extension, |mut map, _| {
                    for (key, value) in entries {
                        let key = (**key).any().downcast_ref::<K>().expect(
                            "the dynamic box should contain a value of the \
                             registered type",
                        );
                        map.serialize_entry(key, value, extension)?;
                    }
                    Ok(())
                })
            };

        if self
            .serialization_helpers_by_type_id
            .insert(K::STABLE_TYPE_ID, SerializationHelper {
                map_serializer,
                dynamic_box_serializer: dyn_box_serializer,
                cas_map_serializer,
                dependency_graph_serializer,
                version_info_serializer,
                std_type_id: std::any::TypeId::of::<K>(),
                value_serializer,
            })
            .is_some()
        {
            panic!(
                "the key {} has already been registered",
                std::any::type_name::<K>()
            )
        }

        let value_deserializer =
            |result_buffer: &mut dyn Any,
             deserializer: &mut D,
             extension: &E| {
                let value = result_buffer
                    .downcast_mut::<Option<K::Value>>()
                    .expect("should be downcastable");

                *value = Some(K::Value::deserialize(deserializer, extension)?);

                Ok(())
            };

        if self
            .deserialization_helpers_by_type_id
            .insert(K::STABLE_TYPE_ID, DeserializationHelper {
                map_deserializer: map_deserializer::<_, _, K>,
                dynamic_box_deserializer: dynamic_box_deserializer::<_, _, K>,
                std_type_id: std::any::TypeId::of::<K>(),
                value_deserializer,
            })
            .is_some()
        {
            panic!(
                "the key {} has already been registered",
                std::any::type_name::<K>()
            )
        }
    }
}

impl<S: Serializer<E>, D: Deserializer<E>, E> std::fmt::Debug
    for Registry<S, D, E>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Registry")
            .field(
                "serialization_helpers_count",
                &self.serialization_helpers_by_type_id.len(),
            )
            .field(
                "deserialization_helpers_count",
                &self.deserialization_helpers_by_type_id.len(),
            )
            .finish()
    }
}

impl<S: Serializer<Self>, D: Deserializer<Self>> std::fmt::Debug
    for SelfRegistry<S, D>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SelfRegistry")
            .field("registry", &"Registry { .. }")
            .finish()
    }
}

impl<S: Serializer<E>, E> Clone for SerializationHelper<S, E> {
    fn clone(&self) -> Self {
        Self {
            map_serializer: self.map_serializer,
            dynamic_box_serializer: self.dynamic_box_serializer,
            value_serializer: self.value_serializer,
            cas_map_serializer: self.cas_map_serializer,
            dependency_graph_serializer: self.dependency_graph_serializer,
            version_info_serializer: self.version_info_serializer,
            std_type_id: self.std_type_id,
        }
    }
}

impl<D: Deserializer<E>, E> Clone for DeserializationHelper<D, E> {
    fn clone(&self) -> Self {
        Self {
            map_deserializer: self.map_deserializer,
            dynamic_box_deserializer: self.dynamic_box_deserializer,
            value_deserializer: self.value_deserializer,
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
        extension: &E,
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
        extension: &E,
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

/// A specialized registry that implements the dynamic traits for
/// self-referential usage.
///
/// `SelfRegistry` is a wrapper around `Registry` that implements
/// `DynamicSerialize`, `DynamicDeserialize`, and `DynamicRegistry` for itself.
/// This is useful when you need a registry that can be passed as the extension
/// parameter to serialization and deserialization operations.
///
/// # Type Parameters
/// * `S` - The serializer type that uses `Self` as its extension type
/// * `D` - The deserializer type that uses `Self` as its extension type
pub struct SelfRegistry<S: Serializer<Self>, D: Deserializer<Self>> {
    registry: Registry<S, D, Self>,
}

impl<S: Serializer<Self>, D: Deserializer<Self>> SelfRegistry<S, D> {
    /// Creates a new empty self-registry.
    ///
    /// # Returns
    /// A new `SelfRegistry` instance with an empty internal registry.
    #[must_use]
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
        S::Error: Send + Sync,
        Self: Send + Sync,
    {
        self.registry.register::<K>();
    }
}

#[cfg(test)]
mod test;
