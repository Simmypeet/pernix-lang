//! Contains the definition of [`Arena`] and [`ID`].

use std::{
    borrow::Borrow,
    collections::{hash_map::Entry, HashMap},
    fmt::Debug,
    hash::Hash,
    marker::PhantomData,
    ops::{Index, IndexMut},
};

use derive_more::{Deref, DerefMut};
use getset::CopyGetters;

/// Represents an unique identifier to a particular entry in the [`Arena`] of type `T`.
pub struct ID<T> {
    index: usize,
    _marker: PhantomData<T>,
}

impl<T> Debug for ID<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("ID").field(&self.index).finish()
    }
}

impl<T> ID<T> {
    /// Creates a new [`ID`] with the given index.
    #[must_use]
    pub fn new(index: usize) -> Self {
        Self {
            index,
            _marker: PhantomData,
        }
    }
}

impl<T> Clone for ID<T> {
    fn clone(&self) -> Self {
        Self {
            index: self.index,
            _marker: PhantomData,
        }
    }
}

impl<T> Copy for ID<T> {}

impl<T> PartialEq for ID<T> {
    fn eq(&self, other: &Self) -> bool { self.index == other.index }
}

impl<T> Eq for ID<T> {}

impl<T> PartialOrd for ID<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.index.partial_cmp(&other.index)
    }
}

impl<T> Ord for ID<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering { self.index.cmp(&other.index) }
}

impl<T> std::hash::Hash for ID<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) { self.index.hash(state) }
}

/// Represents an item in the [`Arena`] of type `T` with its [`ID`].
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Deref, DerefMut, CopyGetters,
)]
pub struct Item<T> {
    /// Gets the [`ID`] of the item.
    #[get_copy = "pub"]
    id: ID<T>,

    #[deref]
    #[deref_mut]
    item: T,
}

/// Represents a collection of items of type `T` that can be referenced by an [`ID`].
///
/// Internally, all the items are stored in a [`Vec`], and the [`ID`] is just an index to the item
/// in the [`Vec`]. However, unlike [`Vec`], [`Arena`] doesn't allow removing items in between since
/// it will invalidate all the [`ID`]s given out before. This data structure is commonly used in
/// graph structures where the nodes are stored in an [`Arena`] and the edges are represented by
/// [`ID`]s.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Arena<T> {
    items: Vec<Item<T>>,
}

impl<T> Default for Arena<T> {
    fn default() -> Self { Self { items: Vec::new() } }
}

impl<T> Arena<T> {
    /// Creates a new empty [`Arena`].
    #[must_use]
    pub fn new() -> Self { Self::default() }

    /// Returns the number of items in the [`Arena`].
    #[must_use]
    pub fn len(&self) -> usize { self.items.len() }

    /// Returns `true` if the [`Arena`] contains no items.
    #[must_use]
    pub fn is_empty(&self) -> bool { self.items.is_empty() }

    /// Inserts a new item into the [`Arena`] and returns its [`ID`].
    pub fn insert(&mut self, item: T) -> ID<T> {
        let index = self.items.len();
        self.items.push(Item {
            id: ID::new(index),
            item,
        });
        ID::new(index)
    }

    /// Returns a reference to the item in the [`Arena`] with the given [`ID`].
    #[must_use]
    pub fn get(&self, id: ID<T>) -> Option<&Item<T>> { self.items.get(id.index) }

    /// Returns a mutable reference to the item in the [`Arena`] with the given [`ID`].
    #[must_use]
    pub fn get_mut(&mut self, id: ID<T>) -> Option<&mut Item<T>> { self.items.get_mut(id.index) }

    /// Returns an iterator over the items in the [`Arena`].
    pub fn iter(&self) -> impl Iterator<Item = &Item<T>> { self.items.iter() }

    /// Returns an mutable iterator over the items in the [`Arena`].
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Item<T>> { self.items.iter_mut() }
}

impl<T> Index<ID<T>> for Arena<T> {
    type Output = Item<T>;

    fn index(&self, id: ID<T>) -> &Self::Output { self.get(id).unwrap() }
}

impl<T> IndexMut<ID<T>> for Arena<T> {
    fn index_mut(&mut self, id: ID<T>) -> &mut Self::Output { self.get_mut(id).unwrap() }
}

impl<T> IntoIterator for Arena<T> {
    type IntoIter = std::vec::IntoIter<Item<T>>;
    type Item = Item<T>;

    fn into_iter(self) -> Self::IntoIter { self.items.into_iter() }
}

impl<'a, T> IntoIterator for &'a Arena<T> {
    type IntoIter = std::slice::Iter<'a, Item<T>>;
    type Item = &'a Item<T>;

    fn into_iter(self) -> Self::IntoIter { self.items.iter() }
}

impl<'a, T> IntoIterator for &'a mut Arena<T> {
    type IntoIter = std::slice::IterMut<'a, Item<T>>;
    type Item = &'a mut Item<T>;

    fn into_iter(self) -> Self::IntoIter { self.items.iter_mut() }
}

/// Represents a map from that allows referencing items in the [`Map`] by either their `K` key or
/// their [`ID`].
///
/// Accessing the items by their [`ID`] is more efficient than accessing them by their `K` key since
/// the former is just an index to the item in the [`Arena`], while the latter requires a hash map
/// lookup.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Map<T, K = String>
where
    K: Hash + Eq,
{
    arena: Arena<T>,
    items: HashMap<K, ID<T>>,
}

impl<T, K: Hash + Eq> Map<T, K> {
    /// Creates a new empty [`Map`].
    #[must_use]
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
            items: HashMap::new(),
        }
    }

    /// Inserts a new item into the [`Map`] with the given key
    ///
    /// # Errors
    ///
    /// Returns `Err` with a tuple of the [`ID`] of the existing item and the new item if the key
    /// already exists in the [`Map`].
    pub fn insert(&mut self, key: K, item: T) -> Result<ID<T>, (ID<T>, T)> {
        match self.items.entry(key) {
            Entry::Occupied(entry) => {
                let id = *entry.get();
                Err((id, item))
            }
            Entry::Vacant(entry) => {
                let id = self.arena.insert(item);
                entry.insert(id);
                Ok(id)
            }
        }
    }

    /// Returns a reference to the item in the [`Map`] with the given key.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the key doesn't exist in the [`Map`].
    pub fn get_id<Q: ?Sized + Hash + Eq>(&self, key: &Q) -> Option<ID<T>>
    where
        K: Borrow<Q>,
    {
        self.items.get(key).copied()
    }

    /// Returns a reference to the item in the [`Map`] with the given [`ID`].
    #[must_use]
    pub fn get(&self, id: ID<T>) -> Option<&Item<T>> { self.arena.get(id) }

    /// Returns a mutable reference to the item in the [`Map`] with the given [`ID`].
    #[must_use]
    pub fn get_mut(&mut self, id: ID<T>) -> Option<&mut Item<T>> { self.arena.get_mut(id) }

    /// Returns an iterator over the items in the [`Map`].
    pub fn iter(&self) -> impl Iterator<Item = (&K, &Item<T>)> {
        self.items.iter().map(|(k, v)| (k, self.get(*v).unwrap()))
    }

    /// Returns an iterator over the keys in the [`Map`].
    pub fn keys(&self) -> impl Iterator<Item = &K> { self.items.keys() }

    /// Returns an iterator over the values in the [`Map`].
    pub fn values(&self) -> impl Iterator<Item = &Item<T>> { self.arena.iter() }

    /// Returns an mutable iterator over the items in the [`Map`].
    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut Item<T>> { self.arena.iter_mut() }
}

impl<T, K: Eq + Hash> Default for Map<T, K> {
    fn default() -> Self { Self::new() }
}

impl<T, K: Eq + Hash> Index<ID<T>> for Map<T, K> {
    type Output = Item<T>;

    fn index(&self, id: ID<T>) -> &Self::Output { self.get(id).unwrap() }
}

impl<T, K: Eq + Hash> IndexMut<ID<T>> for Map<T, K> {
    fn index_mut(&mut self, id: ID<T>) -> &mut Self::Output { self.get_mut(id).unwrap() }
}

impl<T, K: Eq + Hash + Borrow<Q>, Q: ?Sized + Hash + Eq> Index<&Q> for Map<T, K> {
    type Output = Item<T>;

    fn index(&self, key: &Q) -> &Self::Output { self.get(self.get_id(key).unwrap()).unwrap() }
}

impl<T, K: Eq + Hash + Borrow<Q>, Q: ?Sized + Hash + Eq> IndexMut<&Q> for Map<T, K> {
    fn index_mut(&mut self, key: &Q) -> &mut Self::Output {
        self.get_mut(self.get_id(key).unwrap()).unwrap()
    }
}
