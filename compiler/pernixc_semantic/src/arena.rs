//! Contains the definition of [`Arena`] and [`ID`].

use std::{
    borrow::Borrow,
    collections::{hash_map::Entry, HashMap},
    fmt::Debug,
    hash::Hash,
    marker::PhantomData,
    ops::{Index, IndexMut},
};

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

/// Represents a collection of items of type `T` that can be referenced by an [`ID`].
///
/// Internally, all the items are stored in a [`Vec`], and the [`ID`] is just an index to the item
/// in the [`Vec`]. However, unlike [`Vec`], [`Arena`] doesn't allow removing items in between since
/// it will invalidate all the [`ID`]s given out before. This data structure is commonly used in
/// graph structures where the nodes are stored in an [`Arena`] and the edges are represented by
/// [`ID`]s.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Arena<T, Idx = T> {
    _marker: PhantomData<Idx>,
    items: Vec<T>,
}

impl<T, Idx> Default for Arena<T, Idx> {
    fn default() -> Self {
        Self {
            _marker: PhantomData,
            items: Vec::new(),
        }
    }
}

impl<T, Idx> Arena<T, Idx> {
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
    pub fn insert(&mut self, item: T) -> ID<Idx> {
        let index = self.items.len();
        self.items.push(item);
        ID::new(index)
    }

    /// Returns a reference to the item in the [`Arena`] with the given [`ID`].
    #[must_use]
    pub fn get(&self, id: ID<Idx>) -> Option<&T> { self.items.get(id.index) }

    /// Returns a mutable reference to the item in the [`Arena`] with the given [`ID`].
    #[must_use]
    pub fn get_mut(&mut self, id: ID<Idx>) -> Option<&mut T> { self.items.get_mut(id.index) }

    /// Returns an iterator over the items in the [`Arena`].
    pub fn iter(&self) -> impl Iterator<Item = &T> { self.items.iter() }

    /// Returns an mutable iterator over the items in the [`Arena`].
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> { self.items.iter_mut() }
}

impl<T, Idx> Index<ID<Idx>> for Arena<T, Idx> {
    type Output = T;

    fn index(&self, id: ID<Idx>) -> &Self::Output { self.get(id).unwrap() }
}

impl<T, Idx> IndexMut<ID<Idx>> for Arena<T, Idx> {
    fn index_mut(&mut self, id: ID<Idx>) -> &mut Self::Output { self.get_mut(id).unwrap() }
}

impl<T> IntoIterator for Arena<T> {
    type IntoIter = std::vec::IntoIter<T>;
    type Item = T;

    fn into_iter(self) -> Self::IntoIter { self.items.into_iter() }
}

impl<'a, T> IntoIterator for &'a Arena<T> {
    type IntoIter = std::slice::Iter<'a, T>;
    type Item = &'a T;

    fn into_iter(self) -> Self::IntoIter { self.items.iter() }
}

impl<'a, T> IntoIterator for &'a mut Arena<T> {
    type IntoIter = std::slice::IterMut<'a, T>;
    type Item = &'a mut T;

    fn into_iter(self) -> Self::IntoIter { self.items.iter_mut() }
}

/// Represents a map from that allows referencing items in the [`Map`] by either their `K` key or
/// their [`ID`].
///
/// Accessing the items by their [`ID`] is more efficient than accessing them by their `K` key since
/// the former is just an index to the item in the [`Arena`], while the latter requires a hash map
/// lookup.
#[derive(Debug, Clone, PartialEq, Eq, derive_more::Index, derive_more::IndexMut)]
pub struct Map<T, K = String, Idx = T>
where
    K: Hash + Eq,
{
    #[index]
    #[index_mut]
    arena: Arena<T, Idx>,

    items: HashMap<K, ID<Idx>>,
}

impl<T, K: Hash + Eq, Idx> Map<T, K, Idx> {
    /// Creates a new empty [`Map`].
    #[must_use]
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
            items: HashMap::new(),
        }
    }

    /// Returns the number of items in the [`Map`].
    #[must_use]
    pub fn len(&self) -> usize { self.arena.len() }

    /// Returns `true` if the [`Map`] contains no items.
    #[must_use]
    pub fn is_empty(&self) -> bool { self.arena.is_empty() }

    /// Inserts a new item into the [`Map`] with the given key
    ///
    /// # Errors
    ///
    /// Returns `Err` with a tuple of the [`ID`] of the existing item and the new item if the key
    /// already exists in the [`Map`].
    pub fn insert(&mut self, key: K, item: T) -> Result<ID<Idx>, (ID<Idx>, T)> {
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
    pub fn get_id<Q: ?Sized + Hash + Eq>(&self, key: &Q) -> Option<ID<Idx>>
    where
        K: Borrow<Q>,
    {
        self.items.get(key).copied()
    }

    /// Returns a reference to the item in the [`Map`] with the given [`ID`].
    #[must_use]
    pub fn get(&self, id: ID<Idx>) -> Option<&T> { self.arena.get(id) }

    /// Returns a mutable reference to the item in the [`Map`] with the given [`ID`].
    #[must_use]
    pub fn get_mut(&mut self, id: ID<Idx>) -> Option<&mut T> { self.arena.get_mut(id) }

    /// Returns an iterator over the items in the [`Map`].
    ///
    /// The order of the items is **not** maintained.
    pub fn iter(&self) -> impl Iterator<Item = (&K, &T)> {
        self.items.iter().map(|(k, v)| (k, self.get(*v).unwrap()))
    }

    /// Returns an iterator over the keys in the [`Map`].
    ///
    /// The order of the keys is **not** maintained.
    pub fn keys(&self) -> impl Iterator<Item = &K> { self.items.keys() }

    /// Returns an iterator over the values in the [`Map`].
    ///
    /// The order of the values is maintained.
    pub fn values(&self) -> impl Iterator<Item = &T> { self.arena.iter() }

    /// Returns an mutable iterator over the items in the [`Map`].
    ///
    /// The order of the values is not maintained.
    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut T> { self.arena.iter_mut() }
}

impl<T, K: Eq + Hash, Idx> Default for Map<T, K, Idx> {
    fn default() -> Self { Self::new() }
}
