//! A library for creating a component-based data structure.

use std::{
    any::{Any, TypeId},
    fmt::{Debug, Display},
    hash::Hash,
    ops::{Deref, DerefMut},
};

use dashmap::{DashMap, Entry};

pub mod serde;

/// The data structure used for dynamically attaching componenet to entities.
///
/// This struct ressembles an entity-component-system (ECS) data structure
/// except that it is prioritized for accessing individual components of an
/// entity.
#[derive(Debug)]
pub struct Storage<ID: Eq + Hash> {
    components: DashMap<(ID, TypeId), Box<dyn Any>>,
}

impl<ID: Eq + Hash> Default for Storage<ID> {
    fn default() -> Self { Self { components: DashMap::new() } }
}

impl<ID: Eq + Hash> Storage<ID> {
    /// Adds a new component to the entity of the given id.
    ///
    /// If entity already has a component of the same type, it will return false
    /// and the component will not be added. Otherwise, it will return true.
    ///
    /// # Example
    ///
    /// ``` rust
    /// use pernixc_component::Storage;
    ///
    /// let storage = Storage::default();
    ///
    /// assert!(storage.add_component("Test".to_string(), 1));
    /// assert!(!storage.add_component("Test".to_string(), 2));
    /// ```
    #[must_use]
    pub fn add_component<U: Any>(&self, id: ID, component: U) -> bool
    where
        ID: Hash + Eq,
    {
        match self.components.entry((id, TypeId::of::<U>())) {
            Entry::Occupied(_) => false,
            Entry::Vacant(entry) => {
                entry.insert(Box::new(component));
                true
            }
        }
    }

    /// Similar to [`Storage::add_component`] but takes a boxed component.
    ///
    /// # Panics
    ///
    /// Panics if the boxed component is not of the correct type.
    pub fn add_component_boxed<U: Any>(
        &self,
        id: ID,
        component: Box<dyn Any>,
    ) -> bool
    where
        ID: Hash + Eq,
    {
        assert!(component.is::<U>());

        match self.components.entry((id, TypeId::of::<U>())) {
            Entry::Occupied(_) => false,
            Entry::Vacant(entry) => {
                entry.insert(component);
                true
            }
        }
    }

    /// Enables the serialization of the storage based on the given reflector.
    #[must_use]
    pub fn as_serializable<'a, T, E: Display + 'static>(
        &'a self,
        reflector: &'a serde::Reflector<T, ID, E>,
    ) -> serde::SerializableStorage<'a, T, ID, E> {
        serde::SerializableStorage::new(self, reflector)
    }

    /// Enables in-place incremental deserialization based on the given
    /// reflector.
    #[must_use]
    pub fn as_inplace_deserializer<'a, T, E: Display + 'static>(
        &'a self,
        reflector: &'a serde::Reflector<T, ID, E>,
    ) -> serde::InplaceDeserializer<'a, T, ID, E> {
        serde::InplaceDeserializer::new(self, reflector)
    }

    /// Gets the component of the given type from the entity of the given id.
    #[must_use]
    pub fn get<U: Any>(&self, id: ID) -> Option<impl Deref<Target = U> + '_>
    where
        ID: Clone + Hash + Eq,
    {
        self.components.get(&(id, TypeId::of::<U>())).map(|component| {
            dashmap::mapref::one::Ref::map(component, |x| {
                (*x).downcast_ref().unwrap()
            })
        })
    }

    /// Gets the mutable component of the given type from the entity of the
    /// given id.
    pub fn get_mut<U: Any>(
        &self,
        id: ID,
    ) -> Option<impl DerefMut<Target = U> + '_>
    where
        ID: Clone + Hash + Eq,
    {
        self.components.get_mut(&(id, TypeId::of::<U>())).map(|component| {
            dashmap::mapref::one::RefMut::map(component, |x| {
                (*x).downcast_mut().unwrap()
            })
        })
    }

    /// Removes the component of the given type from the entity of the given id.
    ///
    /// # Example
    ///
    /// ``` rust
    /// use pernixc_component::Storage;
    ///
    /// let storage = Storage::default();
    ///
    /// storage.add_component("Test".to_string(), 1);
    /// storage.add_component("Test".to_string(), "Hello".to_string());
    ///
    /// assert_eq!(
    ///     storage.remove::<i32>("Test".to_string()).as_deref(),
    ///     Some(1).as_ref()
    /// );
    /// assert_eq!(
    ///     storage
    ///         .remove::<String>("Test".to_string())
    ///         .as_deref()
    ///         .map(String::as_str),
    ///     Some("Hello")
    /// );
    ///
    /// assert!(storage.get::<i32>("Test".to_string()).is_none());
    /// ```
    pub fn remove<U: Any>(&mut self, id: ID) -> Option<Box<U>>
    where
        ID: Hash + Eq,
    {
        self.components
            .remove(&(id, TypeId::of::<U>()))
            .map(|component| component.1.downcast().unwrap())
    }
}
