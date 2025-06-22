//! Contains the definition of [`Arena`] and [`ID`].
//!
//! [`Arena`] is a data structure that allows storing items of type `T` and
//! referencing them by your own custom index type. This is useful for providing
//! more type safety when working with various containers of different types.

use std::{
    collections::hash_map::Entry,
    fmt::Debug,
    marker::PhantomData,
    ops::{Index, IndexMut},
};

use pernixc_hash::HashMap;
use pernixc_serialize::{
    de::Deserializer, ser::Serializer, Deserialize, Serialize,
};
use pernixc_stable_hash::StableHash;
use state::{Generator, Rebind, State};

pub mod state;

mod arbitrary;

/// Represents an unique identifier to a particular entry in the [`Arena`] of
/// type `T`.
pub struct ID<T: ?Sized> {
    index: u64,

    _marker: PhantomData<Box<T>>,
}

impl<T: ?Sized> ID<T> {
    /// Returns the index of the [`ID`].
    #[must_use]
    pub const fn index(&self) -> u64 { self.index }
}

unsafe impl<T> Send for ID<T> {}
unsafe impl<T> Sync for ID<T> {}

impl<T> Default for ID<T> {
    fn default() -> Self { Self { index: 0, _marker: PhantomData } }
}

impl<T> Debug for ID<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ID({})", self.index)
    }
}

impl<T> ID<T> {
    /// Creates a new [`ID`] with the given index.
    #[must_use]
    pub const fn new(index: u64) -> Self {
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

impl<T> StableHash for ID<T> {
    fn stable_hash<H: pernixc_stable_hash::StableHasher + ?Sized>(
        &self,
        state: &mut H,
    ) {
        self.index.stable_hash(state);
    }
}

impl<S: Serializer<E>, E, T> Serialize<S, E> for ID<T> {
    fn serialize(
        &self,
        serializer: &mut S,
        extension: &mut E,
    ) -> Result<(), S::Error> {
        self.index.serialize(serializer, extension)
    }
}

impl<D: Deserializer<E>, E, T> Deserialize<D, E> for ID<T> {
    fn deserialize(
        deserializer: &mut D,
        extension: &mut E,
    ) -> Result<Self, D::Error> {
        u64::deserialize(deserializer, extension).map(Self::new)
    }
}

/// Represents a collection of items of type `T` that can be referenced by an
/// [`ID`].
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(ser_bound(G: Serialize<__S, __E>, G::ID: Serialize<__S, __E>, T: Serialize<__S, __E>))]
#[serde(de_bound(G: Deserialize<__D, __E>, G::ID: Deserialize<__D, __E>, T: Deserialize<__D, __E>))]
pub struct Arena<T, G: State<T> = state::Default> {
    generator: G,
    items: HashMap<G::ID, T>,
}

// skipcq: RS-W1111 this doesn't require G::ID to be `Default`
impl<T, G: State<T> + Default> Default for Arena<T, G> {
    fn default() -> Self {
        Self { items: HashMap::default(), generator: G::default() }
    }
}

impl<T: StableHash, G: State<T> + StableHash> StableHash for Arena<T, G>
where
    G::ID: StableHash,
{
    fn stable_hash<H: pernixc_stable_hash::StableHasher + ?Sized>(
        &self,
        state: &mut H,
    ) {
        self.generator.stable_hash(state);
        self.items.stable_hash(state);
    }
}

impl<T, G: State<T>> Arena<T, G> {
    /// Creates a new empty [`Arena`].
    #[must_use]
    pub fn new() -> Self
    where
        G: Default,
    {
        Self::default()
    }

    /// Creates a new empty [`Arena`] with the given ID generator.
    #[must_use]
    pub fn new_with(generator: G) -> Self {
        Self { items: HashMap::default(), generator }
    }

    /// Returns the number of items in the [`Arena`].
    #[must_use]
    pub fn len(&self) -> usize { self.items.len() }

    /// Returns `true` if the [`Arena`] contains no items.
    #[must_use]
    pub fn is_empty(&self) -> bool { self.items.is_empty() }

    /// Inserts a new item into the [`Arena`] and returns its ID.
    pub fn insert(&mut self, item: T) -> G::ID
    where
        G: Generator<T>,
    {
        let next_id = self.generator.next_id(&self.items, &item);
        assert!(self.items.insert(next_id, item).is_none());

        next_id
    }

    /// Retains only the items in the [`Arena`] that satisfy the given
    /// predicate.
    pub fn retain(&mut self, mut f: impl FnMut(G::ID, &mut T) -> bool) {
        self.items.retain(|id, item| f(*id, item));
    }

    /// Inserts a new item into the [`Arena`] with explicit ID.
    ///
    /// If the ID is already occupied, the item is reutrned back.
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
    /// function. The mapped items will have the same IDs as the original
    /// items.
    pub fn map<U: 'static>(
        mut self,
        mut f: impl FnMut(T) -> U,
    ) -> Arena<U, G::Result>
    where
        G: Rebind<T, U>,
    {
        let mut rebound_gen: G::Result = self.generator.rebind();

        let items = self
            .items
            .drain()
            .map(|(id, item)| {
                (G::convert_rebound_id(&mut rebound_gen, id), f(item))
            })
            .collect();

        Arena { items, generator: rebound_gen }
    }

    /// Returns a reference to the item in the [`Arena`] with the given ID.
    #[must_use]
    pub fn get(&self, id: G::ID) -> Option<&T> { self.items.get(&id) }

    /// Returns a mutable reference to the item in the [`Arena`] with the given
    /// ID.
    #[must_use]
    pub fn get_mut(&mut self, id: G::ID) -> Option<&mut T> {
        self.items.get_mut(&id)
    }

    /// Returns an iterator over the items in the [`Arena`].
    #[must_use]
    pub fn items(&self) -> impl ExactSizeIterator<Item = &T> {
        self.items.values()
    }

    /// Checks if the [`Arena`] contains an item with the given ID.
    #[must_use]
    pub fn contains_id(&self, id: G::ID) -> bool {
        self.items.contains_key(&id)
    }

    /// Returns an mutable iterator over the items in the [`Arena`].
    pub fn items_mut(&mut self) -> impl ExactSizeIterator<Item = &mut T> {
        self.items.values_mut()
    }

    /// Returns an iterator over the items in the [`Arena`] with their IDs.
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

    /// Returns an iterator over the IDs of the items in the [`Arena`].
    #[must_use]
    pub fn ids(&self) -> impl ExactSizeIterator<Item = G::ID> + '_ {
        self.items.keys().copied()
    }

    /// Removes the item in the [`Arena`] with the given ID and returns it.
    #[must_use]
    pub fn remove(&mut self, id: G::ID) -> Option<T> { self.items.remove(&id) }
}

impl<T, G: State<T>> Index<G::ID> for Arena<T, G> {
    type Output = T;

    fn index(&self, id: G::ID) -> &Self::Output { self.get(id).unwrap() }
}

impl<T, G: State<T>> IndexMut<G::ID> for Arena<T, G> {
    fn index_mut(&mut self, id: G::ID) -> &mut Self::Output {
        self.get_mut(id).unwrap()
    }
}

impl<T, G: State<T>> IntoIterator for Arena<T, G> {
    type IntoIter = std::collections::hash_map::IntoIter<G::ID, T>;
    type Item = (G::ID, T);

    fn into_iter(self) -> Self::IntoIter { self.items.into_iter() }
}

impl<'a, T, G: State<T>> IntoIterator for &'a Arena<T, G> {
    type IntoIter = std::collections::hash_map::Iter<'a, G::ID, T>;
    type Item = (&'a G::ID, &'a T);

    fn into_iter(self) -> Self::IntoIter { self.items.iter() }
}

impl<'a, T, G: State<T>> IntoIterator for &'a mut Arena<T, G> {
    type IntoIter = std::collections::hash_map::IterMut<'a, G::ID, T>;
    type Item = (&'a G::ID, &'a mut T);

    fn into_iter(self) -> Self::IntoIter { self.items.iter_mut() }
}
