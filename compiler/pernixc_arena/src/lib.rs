//! Contains the definition of [`Arena`] and [`ID`].
//!
//! [`Arena`] is a data structure that allows storing items of type `T` and
//! referencing them by your own custom index type. This is useful for providing
//! more type safety when working with various containers of different types.

use std::{
    collections::{hash_map::Entry, HashMap},
    fmt::Debug,
    hash::Hash,
    marker::PhantomData,
    ops::{Index, IndexMut},
};

use serde::{Deserialize, Serialize};

mod arbitrary;

/// Represents a key type that can be used to index items in the [`Arena`].
pub trait Key:
    Debug
    + Clone
    + Copy
    + PartialEq
    + Eq
    + PartialOrd
    + Ord
    + Hash
    + 'static
    + Send
    + Sync
{
    /// Creates a new [`Key`] from the given index.
    fn from_index(index: usize) -> Self;

    /// Returns the index of the [`Key`].
    fn into_index(self) -> usize;
}

/// Represents an unique identifier to a particular entry in the [`Arena`] of
/// type `T`.
pub struct ID<T: ?Sized> {
    index: usize,

    _marker: PhantomData<Box<T>>,
}

impl<T: 'static> Key for ID<T> {
    fn from_index(index: usize) -> Self { Self { index, _marker: PhantomData } }

    fn into_index(self) -> usize { self.index }
}

unsafe impl<T> Send for ID<T> {}
unsafe impl<T> Sync for ID<T> {}

impl<T> Debug for ID<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ID({})", self.index)
    }
}

impl<T> ID<T> {
    /// Creates a new [`ID`] with the given index.
    #[must_use]
    pub const fn new(index: usize) -> Self {
        Self { index, _marker: PhantomData }
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
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for ID<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.index.cmp(&other.index)
    }
}

impl<T> std::hash::Hash for ID<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.index.hash(state);
    }
}

impl<T> Serialize for ID<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.index.serialize(serializer)
    }
}

impl<'de, T: 'static> serde::Deserialize<'de> for ID<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        usize::deserialize(deserializer).map(Self::from_index)
    }
}

/// Represents a collection of items of type `T` that can be referenced by an
/// [`ID`].
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Arena<T, Idx: Key = ID<T>> {
    last_gen: usize,
    items: HashMap<Idx, T>,
}

impl<T, Idx: Key> Default for Arena<T, Idx> {
    fn default() -> Self { Self { items: HashMap::new(), last_gen: 0 } }
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

    /// Returns the next available [`Idx`] in the [`Arena`].
    #[must_use]
    pub fn next_id(&self) -> Idx { Key::from_index(self.last_gen) }

    /// Inserts a new item into the [`Arena`] and returns its `Idx`.
    pub fn insert(&mut self, item: T) -> Idx {
        let next_id = self.next_id();

        assert!(self.items.insert(next_id, item).is_none()); // no prior value
        self.last_gen += 1;

        next_id
    }

    /// Inserts a new item into the [`Arena`] by constructing it with the given
    /// function with the ID of the item as the argument.
    pub fn insert_with(&mut self, f: impl FnOnce(Idx) -> T) -> Idx {
        let next_id = self.next_id();

        assert!(self.items.insert(next_id, f(next_id)).is_none()); // no prior value
        self.last_gen += 1;

        next_id
    }

    /// Retains only the items in the [`Arena`] that satisfy the given
    /// predicate.
    pub fn retain(&mut self, mut f: impl FnMut(Idx, &mut T) -> bool) {
        self.items.retain(|&id, item| f(id, item));
    }

    /// Inserts a new item into the [`Arena`] with explicit `Idx`.
    ///
    /// If the `Idx` is already occupied, the item is reutrned back.
    ///
    /// # Returns
    ///
    /// Returns `Ok` if the item was inserted successfully.
    ///
    /// # Errors
    ///
    /// Returns `Err` with the item if the ID is already in use.
    pub fn insert_with_id(&mut self, id: Idx, item: T) -> Result<(), T> {
        match self.items.entry(id) {
            Entry::Occupied(_) => Err(item),
            Entry::Vacant(entry) => {
                entry.insert(item);
                self.last_gen = std::cmp::max(self.last_gen, id.into_index());

                Ok(())
            }
        }
    }

    /// Maps the items in the [`Arena`] to another type using the given
    /// function. The mapped items will have the same `Idx`s as the original
    /// items.
    pub fn map<U: 'static>(mut self, mut f: impl FnMut(T) -> U) -> Arena<U> {
        let items = self
            .items
            .drain()
            .map(|(id, item)| (ID::from_index(id.into_index()), f(item)))
            .collect();

        Arena { items, last_gen: self.last_gen }
    }

    /// Returns a reference to the item in the [`Arena`] with the given `Idx`.
    #[must_use]
    pub fn get(&self, id: Idx) -> Option<&T> { self.items.get(&id) }

    /// Returns a mutable reference to the item in the [`Arena`] with the given
    /// `Idx`.
    #[must_use]
    pub fn get_mut(&mut self, id: Idx) -> Option<&mut T> {
        self.items.get_mut(&id)
    }

    /// Returns an iterator over the items in the [`Arena`].
    #[must_use]
    pub fn items(&self) -> impl ExactSizeIterator<Item = &T> {
        self.items.values()
    }

    /// Returns an mutable iterator over the items in the [`Arena`].
    pub fn items_mut(&mut self) -> impl ExactSizeIterator<Item = &mut T> {
        self.items.values_mut()
    }

    /// Returns an iterator over the items in the [`Arena`] with their `Idx`s.
    #[must_use]
    pub fn iter(&self) -> impl ExactSizeIterator<Item = (Idx, &T)> {
        self.items.iter().map(|(idx, i)| (*idx, i))
    }

    /// Returns an mutable iterator over the items in the [`Arena`] with their
    #[must_use]
    pub fn iter_mut(&mut self) -> impl ExactSizeIterator<Item = (Idx, &mut T)> {
        self.items.iter_mut().map(|(idx, i)| (*idx, i))
    }

    /// Returns an iterator over the `Idx`s of the items in the [`Arena`].
    #[must_use]
    pub fn ids(&self) -> impl ExactSizeIterator<Item = Idx> + '_ {
        self.items.keys().copied()
    }

    /// Removes the item in the [`Arena`] with the given `Idx` and returns it.
    #[must_use]
    pub fn remove(&mut self, id: Idx) -> Option<T> { self.items.remove(&id) }
}

impl<T, Idx: Key> Index<Idx> for Arena<T, Idx> {
    type Output = T;

    fn index(&self, id: Idx) -> &Self::Output { self.get(id).unwrap() }
}

impl<T, Idx: Key> IndexMut<Idx> for Arena<T, Idx> {
    fn index_mut(&mut self, id: Idx) -> &mut Self::Output {
        self.get_mut(id).unwrap()
    }
}

impl<T, Idx: Key> IntoIterator for Arena<T, Idx> {
    type IntoIter = std::collections::hash_map::IntoIter<Idx, T>;
    type Item = (Idx, T);

    fn into_iter(self) -> Self::IntoIter { self.items.into_iter() }
}

impl<T, Idx: Key> FromIterator<(Idx, T)> for Arena<T, Idx> {
    fn from_iter<U: IntoIterator<Item = (Idx, T)>>(iter: U) -> Self {
        let items: HashMap<Idx, T> = iter.into_iter().collect();
        let max = items.keys().copied().max().unwrap_or(Idx::from_index(0));
        Self { items, last_gen: max.into_index() }
    }
}

impl<'a, T, Idx: Key> IntoIterator for &'a Arena<T, Idx> {
    type IntoIter = std::collections::hash_map::Iter<'a, Idx, T>;
    type Item = (&'a Idx, &'a T);

    fn into_iter(self) -> Self::IntoIter { self.items.iter() }
}

impl<'a, T, Idx: Key> IntoIterator for &'a mut Arena<T, Idx> {
    type IntoIter = std::collections::hash_map::IterMut<'a, Idx, T>;
    type Item = (&'a Idx, &'a mut T);

    fn into_iter(self) -> Self::IntoIter { self.items.iter_mut() }
}
