//! A library for creating a component-based data structure.

use std::{
    any::{Any, TypeId},
    fmt::Debug,
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
    fn default() -> Self { Storage { components: DashMap::new() } }
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

    /// Enables the serialization of the storage based on the given reflector.
    pub fn as_serializable<'a, T>(
        &'a self,
        reflector: &'a serde::Reflector<T, ID>,
    ) -> serde::SerializableStorage<'a, T, ID> {
        serde::SerializableStorage::new(self, reflector)
    }

    /// Gets the component of the given type from the entity of the given id.
    pub fn get<'a, U: Any>(
        &'a self,
        id: ID,
    ) -> Option<impl Deref<Target = U> + 'a>
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
    pub fn get_mut<'a, U: Any>(
        &'a self,
        id: ID,
    ) -> Option<impl DerefMut<Target = U> + 'a>
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
