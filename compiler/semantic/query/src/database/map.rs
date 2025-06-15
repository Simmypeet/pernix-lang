//! Contains the definition of the [`Map`] struct.

use std::{any::TypeId, mem::ManuallyDrop};

use dashmap::mapref::one::{Ref, RefMut};
use pernixc_hash::DashMap;

use crate::key::Key;

/// A thread-safe concurrent map that stores multiple types of key-value pairs
/// as long as the key implements the [`Key`] trait.
///
/// This is ensentially a nested map, where the outer map is keyed by the type
/// of the key, and the inner map is keyed by the key itself.
#[derive(Default)]
#[allow(clippy::type_complexity)]
pub struct Map {
    inner: DashMap<TypeId, TransparentMap>,
}

impl std::fmt::Debug for Map {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Storage").finish()
    }
}

/// A type alias for the [`DashMap`] that is used to store key-value pairs
/// where the key is of type `K` and the value is of type `K::Value`.
pub type TypedMap<K> = DashMap<K, <K as Key>::Value>;

impl Map {
    /// Inserts a key-value pair into the map. If the key already exists,
    /// the value is updated and the old value is returned.
    pub fn insert<K: Key>(&self, key: K, value: K::Value) -> Option<K::Value> {
        let inner = self.inner.get(&TypeId::of::<K>()).map_or_else(
            || {
                assert!(self
                    .inner
                    .insert(TypeId::of::<K>(), TransparentMap::new::<K>())
                    .is_none());

                self.inner
                    .get(&TypeId::of::<K>())
                    .expect("Failed to get key-value pair")
            },
            |inner| inner,
        );

        let casted_map = unsafe { inner.cast_map::<K>() };

        casted_map.insert(key, value)
    }

    /// Checks if the map contains a key of the given type.
    #[allow(unused)]
    pub fn contains_key<K: Key>(&self, key: &K) -> bool {
        let Some(inner) = self.inner.get(&TypeId::of::<K>()) else {
            return false;
        };

        let casted_map = unsafe { inner.cast_map::<K>() };

        casted_map.contains_key(key)
    }

    /// Checks if the map contains a key of the given type.
    #[must_use]
    pub fn has_type_id(&self, type_id: TypeId) -> bool {
        self.inner.contains_key(&type_id)
    }

    /// Retrieves a value from the map by its key. Returns `None` if the key
    /// does not exist.
    pub fn get<K: Key>(&self, key: &K) -> Option<K::Value> {
        let inner = self.inner.get(&TypeId::of::<K>())?;

        let casted_map = unsafe { inner.cast_map::<K>() };

        // skipcq: RS-W1206
        casted_map.get(key).map(|x| x.clone())
    }

    /// Retrieves a reference to a value from the map by its key and applies
    /// a function to it. Returns `None` if the key does not exist.
    #[allow(unused)]
    pub fn inspect<K: Key, T>(
        &self,
        key: &K,
        f: impl FnOnce(Ref<K, K::Value>) -> T,
    ) -> Option<T> {
        let inner = self.inner.get(&TypeId::of::<K>())?;

        let casted_map = unsafe { inner.cast_map::<K>() };

        casted_map.get(key).map(f)
    }

    /// Gets the entry for a key in the map and invokes a function with it.
    pub fn entry<K: Key, O>(
        &self,
        key: K,
        f: impl FnOnce(dashmap::Entry<K, K::Value>) -> O,
    ) -> O {
        let inner = self.inner.get(&TypeId::of::<K>()).map_or_else(
            || {
                assert!(self
                    .inner
                    .insert(TypeId::of::<K>(), TransparentMap::new::<K>())
                    .is_none());

                self.inner
                    .get(&TypeId::of::<K>())
                    .expect("Failed to get key-value pair")
            },
            |inner| inner,
        );

        let casted_map = unsafe { inner.cast_map::<K>() };

        f(casted_map.entry(key))
    }

    /// Retrieves a mutable reference to a value from the map by its key and
    /// applies a function to it. Returns `None` if the key does not exist.
    #[allow(unused)]
    pub fn inspect_mut<K: Key, T>(
        &self,
        key: &K,
        f: impl FnOnce(RefMut<K, K::Value>) -> T,
    ) -> Option<T> {
        let inner = self.inner.get(&TypeId::of::<K>())?;

        let casted_map = unsafe { inner.cast_map::<K>() };

        casted_map.get_mut(key).map(f)
    }

    /// Retrieves a number of all unique types stored in the map.
    #[must_use]
    pub fn type_lens(&self) -> usize { self.inner.len() }

    /// Retrieves the underlying storage for a specific type `K` and applies
    /// a function to it. If the type does not exist, the function is called
    /// with `None`.
    pub fn type_storage<K: Key, R>(
        &self,
        f: impl FnOnce(Option<&TypedMap<K>>) -> R,
    ) -> R {
        let inner = self.inner.get(&TypeId::of::<K>());

        match inner {
            Some(transparent_map) => {
                let casted_map = unsafe { transparent_map.cast_map::<K>() };
                f(Some(casted_map))
            }
            None => f(None),
        }
    }

    pub(crate) fn insert_typed_map<K: Key>(
        &self,
        map: TypedMap<K>,
    ) -> Option<TypedMap<K>> {
        let transparent_map = TransparentMap::from_typed_map(map);
        let existing_map =
            self.inner.insert(TypeId::of::<K>(), transparent_map);

        existing_map.map(|m| unsafe { m.cast_into_map() })
    }
}

fn drop<K: Key>(ptr: *mut ()) {
    unsafe {
        let map = ptr.cast::<ManuallyDrop<DashMap<K, K::Value>>>();

        ManuallyDrop::drop(&mut *map);
    }
}

type Drop = fn(*mut ());

struct TransparentMap {
    drop_inst: Drop,
    transparent_map: ManuallyDrop<DashMap<(), ()>>,
}

impl TransparentMap {
    fn new<K: Key>() -> Self {
        let map = ManuallyDrop::new(DashMap::<K, K::Value>::default());

        Self {
            drop_inst: drop::<K>,
            transparent_map: unsafe {
                // SAFETY: this is safe assuming the internal layout of
                // DashMap is just pointers and usize.
                std::mem::transmute::<
                    ManuallyDrop<DashMap<K, K::Value>>,
                    ManuallyDrop<DashMap<(), ()>>,
                >(map)
            },
        }
    }

    fn from_typed_map<K: Key>(map: TypedMap<K>) -> Self {
        let transparent_map = ManuallyDrop::new(map);

        Self {
            drop_inst: drop::<K>,
            transparent_map: unsafe {
                // SAFETY: this is safe assuming the internal layout of
                // DashMap is just pointers and usize.
                std::mem::transmute::<
                    ManuallyDrop<DashMap<K, K::Value>>,
                    ManuallyDrop<DashMap<(), ()>>,
                >(transparent_map)
            },
        }
    }

    unsafe fn cast_into_map<K: Key>(mut self) -> DashMap<K, K::Value> {
        unsafe {
            let map = ManuallyDrop::take(&mut self.transparent_map);

            std::mem::transmute(map)
        }
    }

    #[allow(clippy::missing_const_for_fn)]
    unsafe fn cast_map<K: Key>(&self) -> &DashMap<K, K::Value> {
        let inner_map: *const DashMap<(), ()> =
            &raw const *self.transparent_map;

        // type cast to DashMap<K, K::Value, FnvBuildHasher>
        &*inner_map.cast::<DashMap<K, K::Value>>()
    }
}

impl std::ops::Drop for TransparentMap {
    fn drop(&mut self) {
        (self.drop_inst)((&raw mut self.transparent_map).cast());
    }
}

static_assertions::assert_impl_all!(Map: Send, Sync);

// #[cfg(test)]
// mod test;
