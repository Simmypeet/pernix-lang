//! A container for storing items of type `T` with unique identifiers.
//!
//! This module provides an implementation of an [`Arena`], which is a container for storing items
//! of type `T` with unique identifiers. The [`Arena`]s implemented as a vector of [`Symbol<T>`]
//! structs, where each [`Symbol<T>`] struct contains a value of type `T`, and a unique identifier.
//! The unique identifier is represented by an [`ID<T>`] struct, which contains an index into the
//! vector and a marker for the type `T`.

use derive_more::{Deref, DerefMut};
use getset::CopyGetters;

/// A unique identifier for a symbol in the arena.
///
/// The [`ID`] is only unique within the [`Arena`] it belongs to. Two [`Arena`]s can have two
/// different symbols with the same ID.
///
/// This a newtype wrapper around an index into the [`Arena`]'s internal vector. The extra type
/// parameter required by this struct is used to prevent indexing into the wrong [`Arena`] type.
#[derive(Debug)]
pub struct ID<T> {
    index: usize,
    _marker: std::marker::PhantomData<T>,
}

impl<T> Clone for ID<T> {
    fn clone(&self) -> Self {
        Self {
            index: self.index,
            _marker: std::marker::PhantomData,
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
/// The [`Arena`] internally stores the symbols in a vector. This enforces more type safety than
/// just using a `Vec<T>` as each [`Arena`] type has its own [`ID`] type.
///
/// The [`Arena`] doesn't allow removing items from it as this would invalidate all the IDs that
/// have been handed out.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Arena<T> {
    items: Vec<Symbol<T>>,
}

/// Represents an error that occurs when trying to access a symbol in the [`Arena`] with an invalid
/// ID.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, thiserror::Error)]
#[error("couldn't access the symbol with the given ID.")]
pub struct Error;

impl<T> Arena<T> {
    /// Creates a new, empty arena.
    #[must_use]
    pub fn new() -> Self { Self { items: Vec::new() } }

    /// Adds a symbol to the arena and returns its ID.
    pub fn push(&mut self, item: T) -> ID<T> {
        let index = self.items.len();
        self.items.push(Symbol {
            id: ID {
                index,
                _marker: std::marker::PhantomData,
            },
            symbol: item,
        });
        ID {
            index,
            _marker: std::marker::PhantomData,
        }
    }

    /// Returns the number of items in the arena.
    #[must_use]
    pub fn len(&self) -> usize { self.items.len() }

    /// Returns `true` if the arena contains no items.
    #[must_use]
    pub fn is_empty(&self) -> bool { self.items.is_empty() }

    /// Returns an iterator over the [`ID`]s of the symbols in the arena.
    pub fn ids(&self) -> impl Iterator<Item = ID<T>> {
        (0..self.items.len()).map(|index| ID {
            index,
            _marker: std::marker::PhantomData,
        })
    }

    /// Returns an iterator over the symbols in the arena.
    pub fn iter(&self) -> std::slice::Iter<'_, Symbol<T>> { self.items.iter() }

    /// Returns a mutable iterator over the symbols in the arena.
    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, Symbol<T>> { self.items.iter_mut() }

    /// Returns a reference to the item in the arena with the given ID.
    #[must_use]
    pub fn get(&self, id: ID<T>) -> Option<&Symbol<T>> { self.items.get(id.index) }

    /// Returns a mutable reference to the item in the arena with the given ID.
    #[must_use]
    pub fn get_mut(&mut self, id: ID<T>) -> Option<&mut Symbol<T>> { self.items.get_mut(id.index) }

    /// Returns a reference to the item in the arena with the given ID.
    ///
    /// # Errors
    /// Returns an [`Error`] if the ID is invalid.
    pub fn get_as_ok(&self, id: ID<T>) -> Result<&Symbol<T>, Error> { self.get(id).ok_or(Error) }

    /// Returns a mutable reference to the item in the arena with the given ID.
    ///
    /// # Errors
    /// Returns an [`Error`] if the ID is invalid.
    pub fn get_mut_as_ok(&mut self, id: ID<T>) -> Result<&mut Symbol<T>, Error> {
        self.get_mut(id).ok_or(Error)
    }
}

impl<T> std::ops::Index<ID<T>> for Arena<T> {
    type Output = Symbol<T>;

    /// Returns a reference to the item in the arena with the given ID.
    fn index(&self, id: ID<T>) -> &Self::Output { &self.items[id.index] }
}

impl<T> std::ops::IndexMut<ID<T>> for Arena<T> {
    /// Returns a mutable reference to the item in the arena with the given ID.
    fn index_mut(&mut self, id: ID<T>) -> &mut Self::Output { &mut self.items[id.index] }
}

impl<T> IntoIterator for Arena<T> {
    type IntoIter = std::vec::IntoIter<Self::Item>;
    type Item = Symbol<T>;

    /// Returns an iterator over the items in the arena.
    fn into_iter(self) -> Self::IntoIter { self.items.into_iter() }
}

impl<'a, T> IntoIterator for &'a Arena<T> {
    type IntoIter = std::slice::Iter<'a, Symbol<T>>;
    type Item = &'a Symbol<T>;

    /// Returns an iterator over the items in the arena.
    fn into_iter(self) -> Self::IntoIter { self.items.iter() }
}

impl<'a, T> IntoIterator for &'a mut Arena<T> {
    type IntoIter = std::slice::IterMut<'a, Symbol<T>>;
    type Item = &'a mut Symbol<T>;

    /// Returns a mutable iterator over the items in the arena.
    fn into_iter(self) -> Self::IntoIter { self.items.iter_mut() }
}
