//! Contains the definition of [`Arena`] and [`ID`].
//!
//! [`Arena`] is a data structure that allows storing items of type `T` and referencing them by
//! your own custom index type. This is useful for providing more type safety when working with
//! various containers of different types.

use std::{
    borrow::Borrow,
    collections::{hash_map::Entry, HashMap},
    fmt::Debug,
    hash::Hash,
    marker::PhantomData,
    ops::{Index, IndexMut},
};

/// Represents a key type that can be used to index items in the [`Arena`].
pub trait Key:
    Debug + Clone + Copy + PartialEq + Eq + PartialOrd + Ord + Hash + 'static + Send + Sync
{
    /// Creates a new [`Key`] from the given index.
    fn from_index(index: usize) -> Self;

    /// Returns the index of the [`Key`].
    fn into_index(self) -> usize;
}

/// Represents an unique identifier to a particular entry in the [`Arena`] of type `T`.
pub struct ID<T> {
    index: usize,
    _marker: PhantomData<T>,
}

impl<T: 'static> Key for ID<T> {
    fn from_index(index: usize) -> Self {
        Self {
            index,
            _marker: PhantomData,
        }
    }

    fn into_index(self) -> usize { self.index }
}

unsafe impl<T> Send for ID<T> {}
unsafe impl<T> Sync for ID<T> {}

impl<T> Debug for ID<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ty_name = std::any::type_name::<T>();
        f.debug_tuple(format!("ID<{ty_name}>").as_str())
            .field(&self.index)
            .finish()
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
    fn clone(&self) -> Self { *self }
}

impl<T> Copy for ID<T> {}

impl<T> PartialEq for ID<T> {
    fn eq(&self, other: &Self) -> bool { self.index == other.index }
}

impl<T> Eq for ID<T> {}

impl<T> PartialOrd for ID<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> { Some(self.cmp(other)) }
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
pub struct Arena<T, Idx: Key = ID<T>> {
    _marker: PhantomData<Idx>,
    items: Vec<T>,
}

impl<T, Idx: Key> Default for Arena<T, Idx> {
    fn default() -> Self {
        Self {
            _marker: PhantomData,
            items: Vec::new(),
        }
    }
}

impl<T, Idx: Key> Arena<T, Idx> {
    /// Creates a new empty [`Arena`].
    #[must_use]
    pub fn new() -> Self { Self::default() }

    /// Returns the number of items in the [`Arena`].
    #[must_use]
    pub fn len(&self) -> usize { self.items.len() }

    /// Returns `true` if the [`Arena`] contains no items.
    #[must_use]
    pub fn is_empty(&self) -> bool { self.items.is_empty() }

    /// Inserts a new item into the [`Arena`] and returns its `Idx`.
    pub fn insert(&mut self, item: T) -> Idx {
        let index = self.items.len();
        self.items.push(item);
        Idx::from_index(index)
    }

    /// Returns a reference to the item in the [`Arena`] with the given `Idx`.
    #[must_use]
    pub fn get(&self, id: Idx) -> Option<&T> { self.items.get(id.into_index()) }

    /// Returns a mutable reference to the item in the [`Arena`] with the given `Idx`.
    #[must_use]
    pub fn get_mut(&mut self, id: Idx) -> Option<&mut T> { self.items.get_mut(id.into_index()) }

    /// Returns an iterator over the items in the [`Arena`].
    #[must_use]
    pub fn iter(&self) -> impl ExactSizeIterator<Item = &T> { self.items.iter() }

    /// Returns an mutable iterator over the items in the [`Arena`].
    pub fn iter_mut(&mut self) -> impl ExactSizeIterator<Item = &mut T> { self.items.iter_mut() }

    /// Returns an iterator over the `Idx`s of the items in the [`Arena`].
    #[must_use]
    pub fn keys(&self) -> impl ExactSizeIterator<Item = Idx> {
        (0..self.items.len()).map(|i| Idx::from_index(i))
    }
}

impl<T, Idx: Key> Index<Idx> for Arena<T, Idx> {
    type Output = T;

    fn index(&self, id: Idx) -> &Self::Output { self.get(id).unwrap() }
}

impl<T, Idx: Key> IndexMut<Idx> for Arena<T, Idx> {
    fn index_mut(&mut self, id: Idx) -> &mut Self::Output { self.get_mut(id).unwrap() }
}

impl<T, Idx: Key> IntoIterator for Arena<T, Idx> {
    type IntoIter = std::vec::IntoIter<T>;
    type Item = T;

    fn into_iter(self) -> Self::IntoIter { self.items.into_iter() }
}

impl<'a, T, Idx: Key> IntoIterator for &'a Arena<T, Idx> {
    type IntoIter = std::slice::Iter<'a, T>;
    type Item = &'a T;

    fn into_iter(self) -> Self::IntoIter { self.items.iter() }
}

impl<'a, T, Idx: Key> IntoIterator for &'a mut Arena<T, Idx> {
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
pub struct Map<T, Secondary: Hash + Eq = String, Primary: Key = ID<T>> {
    #[index]
    #[index_mut]
    arena: Arena<T, Primary>,

    items: HashMap<Secondary, Primary>,
}

impl<T, Secondary: Hash + Eq, Primary: Key> Map<T, Secondary, Primary> {
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
    /// # Returns
    ///
    /// Returns `Ok` with the [`ID`] of the new item if the key doesn't exist in the [`Map`].
    ///
    /// # Errors
    ///
    /// Returns `Err` with a tuple of the [`ID`] of the existing item and the new item if the key
    /// already exists in the [`Map`].
    pub fn insert(&mut self, key: Secondary, item: T) -> Result<Primary, (Primary, T)> {
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
    pub fn get_id<Q: ?Sized + Hash + Eq>(&self, key: &Q) -> Option<Primary>
    where
        Secondary: Borrow<Q>,
    {
        self.items.get(key).copied()
    }

    /// Returns a reference to the item in the [`Map`] with the given [`ID`].
    #[must_use]
    pub fn get(&self, id: Primary) -> Option<&T> { self.arena.get(id) }

    /// Returns a mutable reference to the item in the [`Map`] with the given [`ID`].
    #[must_use]
    pub fn get_mut(&mut self, id: Primary) -> Option<&mut T> { self.arena.get_mut(id) }

    /// Returns an iterator over the items in the [`Map`].
    ///
    /// The order of the items is **not** maintained.
    #[must_use]
    pub fn iter(&self) -> impl ExactSizeIterator<Item = (&Secondary, &T)> {
        self.items.iter().map(|(k, v)| (k, self.get(*v).unwrap()))
    }

    /// Returns an iterator over the keys in the [`Map`].
    ///
    /// The order of the keys is **not** maintained.
    #[must_use]
    pub fn keys(&self) -> impl ExactSizeIterator<Item = &Secondary> { self.items.keys() }

    /// Returns an iterator over the values in the [`Map`].
    ///
    /// The order of the values is maintained.
    #[must_use]
    pub fn values(&self) -> impl ExactSizeIterator<Item = &T> { self.arena.iter() }

    /// Returns an mutable iterator over the items in the [`Map`].
    ///
    /// The order of the values is not maintained.
    pub fn values_mut(&mut self) -> impl ExactSizeIterator<Item = &mut T> { self.arena.iter_mut() }
}

impl<T, K: Eq + Hash, Idx: Key> Default for Map<T, K, Idx> {
    fn default() -> Self { Self::new() }
}
