//! Contains the definition of the [`Arena`] type.

use std::collections::HashMap;

use derive_more::{Deref, DerefMut};
use getset::CopyGetters;

/// A unique identifier for a symbol in the arena.
#[derive(Debug)]
pub struct ID<T> {
    id: usize,
    _marker: std::marker::PhantomData<T>,
}

impl<T> Clone for ID<T> {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            _marker: std::marker::PhantomData,
        }
    }
}

impl<T> Copy for ID<T> {}

impl<T> PartialEq for ID<T> {
    fn eq(&self, other: &Self) -> bool { self.id == other.id }
}

impl<T> Eq for ID<T> {}

impl<T> PartialOrd for ID<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

impl<T> Ord for ID<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering { self.id.cmp(&other.id) }
}

impl<T> std::hash::Hash for ID<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) { self.id.hash(state) }
}

/// A symbol in the arena, consisting of an [`ID`] and a value of type `T`.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Deref, DerefMut, CopyGetters)]
pub struct Symbol<T> {
    /// The unique identifier of the symbol.
    #[get_copy = "pub"]
    id: ID<T>,

    #[deref]
    #[deref_mut]
    symbol: T,
}

/// Represents a collection of symbols of type `T`
///
/// Each new symbol added to the arena is assigned with a unique [`ID`].
#[derive(Debug, Clone, Default)]
pub struct Arena<T> {
    items: HashMap<ID<T>, Symbol<T>>,
    current_id: usize,
}

impl<T> Arena<T> {
    /// Creates a new, empty arena.
    #[must_use]
    pub fn new() -> Self {
        Self {
            items: HashMap::new(),
            current_id: 0,
        }
    }

    /// Adds a symbol to the arena and returns its ID.
    pub fn insert(&mut self, item: T) -> ID<T> {
        let new_id = ID {
            id: self.current_id,
            _marker: std::marker::PhantomData,
        };
        self.current_id += 1;

        assert!(self
            .items
            .insert(new_id, Symbol {
                id: new_id,
                symbol: item,
            })
            .is_none());

        new_id
    }

    /// Returns the number of items in the arena.
    #[must_use]
    pub fn len(&self) -> usize { self.items.len() }

    /// Returns `true` if the arena contains no items.
    #[must_use]
    pub fn is_empty(&self) -> bool { self.items.is_empty() }

    /// Returns an iterator over the symbols in the arena.
    #[must_use]
    pub fn values(&self) -> std::collections::hash_map::Values<'_, ID<T>, Symbol<T>> {
        self.items.values()
    }

    /// Returns a mutable iterator over the symbols in the arena.
    #[must_use]
    pub fn values_mut(&mut self) -> std::collections::hash_map::ValuesMut<'_, ID<T>, Symbol<T>> {
        self.items.values_mut()
    }

    /// Returns an iterator over the IDs of the symbols in the arena.
    pub fn ids(&self) -> impl Iterator<Item = ID<T>> + '_ { self.items.keys().copied() }

    /// Removes a symbol from the arena and returns it.
    pub fn remove(&mut self, id: ID<T>) -> Option<Symbol<T>> { self.items.remove(&id) }

    /// Returns a reference to the item in the arena with the given ID.
    #[must_use]
    pub fn get(&self, id: ID<T>) -> Option<&Symbol<T>> { self.items.get(&id) }

    /// Returns a mutable reference to the item in the arena with the given ID.
    #[must_use]
    pub fn get_mut(&mut self, id: ID<T>) -> Option<&mut Symbol<T>> { self.items.get_mut(&id) }
}

impl<T> std::ops::Index<ID<T>> for Arena<T> {
    type Output = Symbol<T>;

    /// Returns a reference to the item in the arena with the given ID.
    fn index(&self, id: ID<T>) -> &Self::Output { &self.items[&id] }
}

impl<T> std::ops::IndexMut<ID<T>> for Arena<T> {
    /// Returns a mutable reference to the item in the arena with the given ID.
    fn index_mut(&mut self, id: ID<T>) -> &mut Self::Output { self.items.get_mut(&id).unwrap() }
}

impl<T> IntoIterator for Arena<T> {
    type IntoIter = std::collections::hash_map::IntoIter<ID<T>, Symbol<T>>;
    type Item = (ID<T>, Symbol<T>);

    /// Returns an iterator over the items in the arena.
    fn into_iter(self) -> Self::IntoIter { self.items.into_iter() }
}

impl<'a, T> IntoIterator for &'a Arena<T> {
    type IntoIter = std::collections::hash_map::Iter<'a, ID<T>, Symbol<T>>;
    type Item = (&'a ID<T>, &'a Symbol<T>);

    /// Returns an iterator over the items in the arena.
    fn into_iter(self) -> Self::IntoIter { self.items.iter() }
}

impl<'a, T> IntoIterator for &'a mut Arena<T> {
    type IntoIter = std::collections::hash_map::IterMut<'a, ID<T>, Symbol<T>>;
    type Item = (&'a ID<T>, &'a mut Symbol<T>);

    /// Returns a mutable iterator over the items in the arena.
    fn into_iter(self) -> Self::IntoIter { self.items.iter_mut() }
}
