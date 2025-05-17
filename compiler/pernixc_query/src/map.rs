//! Contains the definition of the [`Map`] struct.

use std::{any::TypeId, mem::ManuallyDrop};

use dashmap::DashMap;
use fnv::FnvBuildHasher;

use crate::Key;

type Drop = fn(*mut ());

/// A thread-safe concurrent map that stores multiple types of key-value pairs
/// as long as the key implements the [`Key`] trait.
///
/// This is ensentially a nested map, where the outer map is keyed by the type
/// of the key, and the inner map is keyed by the key itself.
#[derive(Default)]
#[allow(clippy::type_complexity)]
pub struct Map {
    inner: DashMap<TypeId, TransparentMap, FnvBuildHasher>,
}

impl std::fmt::Debug for Map {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Storage").finish()
    }
}

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

    /// Retrieves a value from the map by its key. Returns `None` if the key
    /// does not exist.
    pub fn get<K: Key>(&self, key: &K) -> Option<K::Value> {
        let inner = self.inner.get(&TypeId::of::<K>())?;

        let casted_map = unsafe { inner.cast_map::<K>() };

        // skipcq: RS-W1206
        casted_map.get(key).map(|x| x.clone())
    }
}

fn drop<K: Key>(ptr: *mut ()) {
    unsafe {
        let map =
            ptr.cast::<ManuallyDrop<DashMap<K, K::Value, FnvBuildHasher>>>();

        ManuallyDrop::drop(&mut *map);
    }
}

struct TransparentMap {
    drop_inst: Drop,
    transparent_map: ManuallyDrop<DashMap<(), (), FnvBuildHasher>>,
}

impl TransparentMap {
    fn new<K: Key>() -> Self {
        let map = ManuallyDrop::new(
            DashMap::<K, K::Value, FnvBuildHasher>::default(),
        );

        Self {
            drop_inst: drop::<K>,
            transparent_map: unsafe {
                // SAFETY: this is safe assuming the internal layout of
                // DashMap is just pointers and usize.
                std::mem::transmute::<
                    ManuallyDrop<DashMap<K, K::Value, FnvBuildHasher>>,
                    ManuallyDrop<DashMap<(), (), FnvBuildHasher>>,
                >(map)
            },
        }
    }

    #[allow(clippy::missing_const_for_fn)]
    unsafe fn cast_map<K: Key>(&self) -> &DashMap<K, K::Value, FnvBuildHasher> {
        let inner_map: *const DashMap<(), (), FnvBuildHasher> =
            &raw const *self.transparent_map;

        // type cast to DashMap<K, K::Value, FnvBuildHasher>
        &*inner_map.cast::<DashMap<K, K::Value, FnvBuildHasher>>()
    }
}

impl std::ops::Drop for TransparentMap {
    fn drop(&mut self) {
        (self.drop_inst)((&raw mut self.transparent_map).cast());
    }
}

static_assertions::assert_impl_all!(Map: Send, Sync);

#[cfg(test)]
mod test;
