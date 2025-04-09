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

/// A trait for generating unique IDs for items in the [`Arena`].
pub trait IDGenerator<T> {
    /// The type of ID used by the generator.
    type ID: Eq + Hash + Clone + Copy;

    /// Rebinds the current ID generator to a generator of a different arena
    /// type.
    type Rebind<U>: IDGenerator<U>;

    /// Generates a new ID for an item in the [`Arena`].
    fn next_id(&mut self, items: &HashMap<Self::ID, T>) -> Self::ID;

    /// Got invoked when an explicit ID is inserted into the [`Arena`]. This
    /// allows the generator to update its internal state if needed.
    fn explict_insert_with_id(
        &mut self,
        id: &Self::ID,
        items: &HashMap<Self::ID, T>,
    );

    /// Rebinds the current ID generator to a generator of a different arena
    /// type.
    fn rebind<U>(self) -> Self::Rebind<U>;

    /// Converts an ID from the different arena type to the current arena type.
    fn convert_rebound_id<U>(
        rebound: &mut Self::Rebind<U>,
        id: Self::ID,
    ) -> <Self::Rebind<U> as IDGenerator<U>>::ID;
}

/// A simple ID generator that uses a [`usize`] as the ID type and increments it
/// for each new item. This is the default ID generator used by the [`Arena`].
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
pub struct Serial(usize);

impl Serial {
    /// Creates a new [`Serial`] ID generator.
    #[must_use]
    pub const fn new() -> Self { Self(0) }
}

impl<T> IDGenerator<T> for Serial {
    type ID = ID<T>;

    type Rebind<U> = Self;

    fn next_id(&mut self, _: &HashMap<Self::ID, T>) -> Self::ID {
        let next_id = self.0;
        self.0 += 1;

        ID::new(next_id)
    }

    fn explict_insert_with_id(
        &mut self,
        id: &Self::ID,
        _: &HashMap<Self::ID, T>,
    ) {
        self.0 = std::cmp::max(self.0, id.index);
    }

    fn rebind<U>(self) -> Self::Rebind<U> { self }

    fn convert_rebound_id<U>(
        _: &mut Self::Rebind<U>,
        id: Self::ID,
    ) -> <Self::Rebind<U> as IDGenerator<U>>::ID {
        ID::new(id.index)
    }
}

/// Represents a collection of items of type `T` that can be referenced by an
/// [`ID`].
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(bound(
    serialize = "G: Serialize, G::ID: Serialize, T: Serialize",
    deserialize = "G: Deserialize<'de>, G::ID: Deserialize<'de>, T: \
                   Deserialize<'de>",
))]
pub struct Arena<T, G: IDGenerator<T> = Serial> {
    generator: G,
    items: HashMap<G::ID, T>,
}

impl<T, G: IDGenerator<T> + Default> Default for Arena<T, G> {
    fn default() -> Self {
        Self { items: HashMap::new(), generator: G::default() }
    }
}

impl<T, G: IDGenerator<T>> Arena<T, G> {
    /// Creates a new empty [`Arena`].
    #[must_use]
    pub fn new() -> Self
    where
        G: Default,
    {
        Self::default()
    }

    /// Returns the number of items in the [`Arena`].
    #[must_use]
    pub fn len(&self) -> usize { self.items.len() }

    /// Returns `true` if the [`Arena`] contains no items.
    #[must_use]
    pub fn is_empty(&self) -> bool { self.items.is_empty() }

    /// Inserts a new item into the [`Arena`] and returns its `Idx`.
    pub fn insert(&mut self, item: T) -> G::ID {
        let next_id = self.generator.next_id(&self.items);
        assert!(self.items.insert(next_id, item).is_none());

        next_id
    }

    /// Inserts a new item into the [`Arena`] by constructing it with the given
    /// function with the ID of the item as the argument.
    pub fn insert_with(&mut self, f: impl FnOnce(G::ID) -> T) -> G::ID {
        let next_id = self.generator.next_id(&self.items);
        let value = f(next_id);
        assert!(self.items.insert(next_id, value).is_none());

        next_id
    }

    /// Retains only the items in the [`Arena`] that satisfy the given
    /// predicate.
    pub fn retain(&mut self, mut f: impl FnMut(G::ID, &mut T) -> bool) {
        self.items.retain(|id, item| f(*id, item));
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
    pub fn insert_with_id(&mut self, id: G::ID, item: T) -> Result<(), T> {
        match self.items.entry(id) {
            Entry::Occupied(_) => Err(item),
            Entry::Vacant(entry) => {
                entry.insert(item);
                self.generator.explict_insert_with_id(&id, &self.items);

                Ok(())
            }
        }
    }

    /// Maps the items in the [`Arena`] to another type using the given
    /// function. The mapped items will have the same `Idx`s as the original
    /// items.
    pub fn map<U: 'static>(
        mut self,
        mut f: impl FnMut(T) -> U,
    ) -> Arena<U, G::Rebind<U>> {
        let mut rebound_gen: G::Rebind<U> = self.generator.rebind();

        let items = self
            .items
            .drain()
            .map(|(id, item)| {
                (G::convert_rebound_id(&mut rebound_gen, id), f(item))
            })
            .collect();

        Arena { items, generator: rebound_gen }
    }

    /// Returns a reference to the item in the [`Arena`] with the given `Idx`.
    #[must_use]
    pub fn get(&self, id: G::ID) -> Option<&T> { self.items.get(&id) }

    /// Returns a mutable reference to the item in the [`Arena`] with the given
    /// `Idx`.
    #[must_use]
    pub fn get_mut(&mut self, id: G::ID) -> Option<&mut T> {
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
    pub fn iter(&self) -> impl ExactSizeIterator<Item = (G::ID, &T)> {
        self.items.iter().map(|(idx, i)| (*idx, i))
    }

    /// Returns an mutable iterator over the items in the [`Arena`] with their
    #[must_use]
    pub fn iter_mut(
        &mut self,
    ) -> impl ExactSizeIterator<Item = (G::ID, &mut T)> {
        self.items.iter_mut().map(|(idx, i)| (*idx, i))
    }

    /// Returns an iterator over the `Idx`s of the items in the [`Arena`].
    #[must_use]
    pub fn ids(&self) -> impl ExactSizeIterator<Item = G::ID> + '_ {
        self.items.keys().copied()
    }

    /// Removes the item in the [`Arena`] with the given `Idx` and returns it.
    #[must_use]
    pub fn remove(&mut self, id: G::ID) -> Option<T> { self.items.remove(&id) }
}

impl<T, G: IDGenerator<T>> Index<G::ID> for Arena<T, G> {
    type Output = T;

    fn index(&self, id: G::ID) -> &Self::Output { self.get(id).unwrap() }
}

impl<T, G: IDGenerator<T>> IndexMut<G::ID> for Arena<T, G> {
    fn index_mut(&mut self, id: G::ID) -> &mut Self::Output {
        self.get_mut(id).unwrap()
    }
}

impl<T, G: IDGenerator<T>> IntoIterator for Arena<T, G> {
    type IntoIter = std::collections::hash_map::IntoIter<G::ID, T>;
    type Item = (G::ID, T);

    fn into_iter(self) -> Self::IntoIter { self.items.into_iter() }
}

impl<'a, T, G: IDGenerator<T>> IntoIterator for &'a Arena<T, G> {
    type IntoIter = std::collections::hash_map::Iter<'a, G::ID, T>;
    type Item = (&'a G::ID, &'a T);

    fn into_iter(self) -> Self::IntoIter { self.items.iter() }
}

impl<'a, T, G: IDGenerator<T>> IntoIterator for &'a mut Arena<T, G> {
    type IntoIter = std::collections::hash_map::IterMut<'a, G::ID, T>;
    type Item = (&'a G::ID, &'a mut T);

    fn into_iter(self) -> Self::IntoIter { self.items.iter_mut() }
}
