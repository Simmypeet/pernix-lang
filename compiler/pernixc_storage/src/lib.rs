//! A library for creating a component-based data structure.

use std::{
    any::{Any, TypeId},
    fmt::{Debug, Display},
    hash::Hash,
    ops::{Deref, DerefMut},
    sync::Arc,
};

use ::serde::{Deserialize, Serialize};
use dashmap::{DashMap, Entry};

pub mod serde;

/// A trait for wrapping a value in a pointer type. This makes the [`Storage`]
/// be generic over the smart pointer type it uses.
pub trait Ptr {
    /// Wraps a value in a pointer type.
    type Wrap<T: ?Sized>: Deref<Target = T>;

    /// Wraps a value in a pointer type.
    fn wrap<U: Any + Send + Sync>(
        value: U,
    ) -> Self::Wrap<dyn Any + Send + Sync>;

    /// Tries to get a mutable reference to the inner value.
    fn try_get_mut<U: ?Sized>(value: &mut Self::Wrap<U>) -> Option<&mut U>;
}

/// A struct implementing the [`Ptr`] trait for the [`Box`] type.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
)]
pub struct BoxTrait;

impl Ptr for BoxTrait {
    type Wrap<T: ?Sized> = Box<T>;

    fn wrap<U: Any + Send + Sync>(
        value: U,
    ) -> Self::Wrap<dyn Any + Send + Sync> {
        Box::new(value)
    }

    fn try_get_mut<U: ?Sized>(value: &mut Self::Wrap<U>) -> Option<&mut U> {
        Some(&mut **value)
    }
}

/// A struct implementing the [`Ptr`] trait for the [`Arc`] type.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
)]
pub struct ArcTrait;

impl Ptr for ArcTrait {
    type Wrap<T: ?Sized> = Arc<T>;

    fn wrap<U: Any + Send + Sync>(
        value: U,
    ) -> Self::Wrap<dyn Any + Send + Sync> {
        Arc::new(value)
    }

    fn try_get_mut<U: ?Sized>(value: &mut Self::Wrap<U>) -> Option<&mut U> {
        Arc::get_mut(value)
    }
}

/// The data structure used for dynamically attaching componenet to entities.
///
/// This struct ressembles an entity-component-system (ECS) data structure
/// except that it is prioritized for accessing individual components of an
/// entity.
#[derive(Debug)]
pub struct Storage<ID: Eq + Hash, P: Ptr = BoxTrait> {
    components: DashMap<(ID, TypeId), P::Wrap<dyn Any + Send + Sync>>,
}

impl<ID: Eq + Hash, P: Ptr> Default for Storage<ID, P> {
    fn default() -> Self { Self { components: DashMap::new() } }
}

impl<ID: Eq + Hash, P: Ptr> Storage<ID, P> {
    /// Adds a new component to the entity of the given id.
    ///
    /// If entity already has a component of the same type, it will return false
    /// and the component will not be added. Otherwise, it will return true.
    ///
    /// # Example
    ///
    /// ``` rust
    /// use pernixc_storage::Storage;
    ///
    /// let storage = Storage::default();
    ///
    /// assert!(storage.add_component("Test".to_string(), 1));
    /// assert!(!storage.add_component("Test".to_string(), 2));
    /// ```
    #[must_use]
    pub fn add_component<U: Any + Send + Sync>(
        &self,
        id: ID,
        component: U,
    ) -> bool
    where
        ID: Hash + Eq,
    {
        match self.components.entry((id, TypeId::of::<U>())) {
            Entry::Occupied(_) => false,
            Entry::Vacant(entry) => {
                entry.insert(P::wrap(component));
                true
            }
        }
    }

    /// Similar to [`Storage::add_component`] but takes a boxed component.
    ///
    /// # Panics
    ///
    /// Panics if the boxed component is not of the correct type.
    pub fn add_component_raw<U: Any + Send + Sync>(
        &self,
        id: ID,
        component: P::Wrap<dyn Any + Send + Sync>,
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
        reflector: &'a serde::Reflector<ID, P, T, E>,
    ) -> serde::SerializableStorage<'a, ID, P, T, E> {
        serde::SerializableStorage::new(self, reflector)
    }

    /// Enables in-place incremental deserialization based on the given
    /// reflector.
    #[must_use]
    pub fn as_inplace_deserializer<'a, T, E: Display + 'static>(
        &'a self,
        reflector: &'a serde::Reflector<ID, P, T, E>,
    ) -> serde::InplaceDeserializer<'a, ID, P, T, E> {
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
}

impl<ID: Eq + Hash> Storage<ID, BoxTrait> {
    /// Removes the component of the given type from the entity of the given id.
    ///
    /// # Example
    ///
    /// ``` rust
    /// use pernixc_storage::Storage;
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
}

/// An error returned when retrieving a mutable reference to a component fails.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    thiserror::Error,
    displaydoc::Display,
)]
pub enum GetMutError {
    /// The entity id or the component type was not found.
    NotFound,

    /// The component is shared and cannot be uniquely borrowed.
    ArcNotUnique,
}

impl<ID: Eq + Hash> Storage<ID, ArcTrait> {
    /// Tries to get a mutable reference to the component of the given type from
    /// the entity of the given id.
    ///
    /// # Errors
    ///
    /// See [`GetMutError`] for the possible errors.
    pub fn get_mut<U: Any>(
        &self,
        id: ID,
    ) -> Result<impl DerefMut<Target = U> + '_, GetMutError>
    where
        ID: Clone + Hash + Eq,
    {
        let component = self
            .components
            .get_mut(&(id, TypeId::of::<U>()))
            .ok_or(GetMutError::NotFound)?;

        dashmap::mapref::one::RefMut::try_map(component, |x| {
            Arc::get_mut(x).map(|x| x.downcast_mut::<U>().unwrap())
        })
        .map_err(|_| GetMutError::ArcNotUnique)
    }
}

impl<ID: Eq + Hash> Storage<ID, ArcTrait> {
    /// Gets a cloned version of the component of the given type from the entity
    /// of the given id.
    pub fn get_cloned<U: Any + Send + Sync>(&self, id: ID) -> Option<Arc<U>> {
        self.components
            .get(&(id, TypeId::of::<U>()))
            .map(|x| Arc::clone(&*x).clone().downcast::<U>().unwrap())
    }
}
