//! Contains the definition of the [`Arena`] type.

use std::{borrow::Borrow, cmp::Eq, collections::HashMap, hash::Hash};

use derive_more::{Deref, DerefMut};
use getset::CopyGetters;

/// A unique identifier for a symbol in the arena.
///
/// The underlying value is the index of the symbol in the [`Arena`]'s internal vector.
/// The reason for this is that to avoid ambiguity between multiple arenas, the [`ID`] type
/// is generic over the type of the symbol it represents.
#[derive(Debug)]
pub struct ID<T> {
    id: usize,
    _marker: std::marker::PhantomData<T>,
}

impl<T> Default for ID<T> {
    fn default() -> Self {
        Self {
            id: 0,
            _marker: std::marker::PhantomData,
        }
    }
}

impl<T> ID<T> {
    /// Returns the underlying integer ID.
    #[must_use]
    pub fn id(&self) -> usize { self.id }

    /// Creates a new [`ID`] with the given integer ID.
    #[must_use]
    pub fn new(id: usize) -> Self {
        Self {
            id,
            _marker: std::marker::PhantomData,
        }
    }
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

impl<T> Hash for ID<T> {
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Arena<T> {
    items: Vec<Symbol<T>>,
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self {
            items: Vec::default(),
        }
    }
}

impl<T> Arena<T> {
    /// Creates a new, empty arena.
    #[must_use]
    pub fn new() -> Self { Self { items: Vec::new() } }

    /// Adds a symbol to the arena and returns its ID.
    pub fn push(&mut self, item: T) -> ID<T> {
        let new_id = ID {
            id: self.items.len(),
            _marker: std::marker::PhantomData,
        };

        self.items.push(Symbol {
            id: new_id,
            symbol: item,
        });

        new_id
    }

    /// Returns the number of items in the arena.
    #[must_use]
    pub fn len(&self) -> usize { self.items.len() }

    /// Returns `true` if the arena contains no items.
    #[must_use]
    pub fn is_empty(&self) -> bool { self.items.is_empty() }

    /// Returns an iterator over the symbols in the arena.
    pub fn iter(&self) -> std::slice::Iter<'_, Symbol<T>> { self.items.iter() }

    /// Returns a mutable iterator over the symbols in the arena.
    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, Symbol<T>> { self.items.iter_mut() }

    /// Returns a reference to the item in the arena with the given ID.
    #[must_use]
    pub fn get(&self, id: ID<T>) -> Option<&Symbol<T>> { self.items.get(id.id) }

    /// Returns a mutable reference to the item in the arena with the given ID.
    #[must_use]
    pub fn get_mut(&mut self, id: ID<T>) -> Option<&mut Symbol<T>> { self.items.get_mut(id.id) }
}

impl<T> std::ops::Index<ID<T>> for Arena<T> {
    type Output = Symbol<T>;

    /// Returns a reference to the item in the arena with the given ID.
    fn index(&self, id: ID<T>) -> &Self::Output { &self.items[id.id] }
}

impl<T> std::ops::IndexMut<ID<T>> for Arena<T> {
    /// Returns a mutable reference to the item in the arena with the given ID.
    fn index_mut(&mut self, id: ID<T>) -> &mut Self::Output { self.items.get_mut(id.id).unwrap() }
}

impl<T> IntoIterator for Arena<T> {
    type IntoIter = std::vec::IntoIter<Symbol<T>>;
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

    /// Returns an iterator over the items in the arena.
    fn into_iter(self) -> Self::IntoIter { self.items.iter_mut() }
}

/// A container for symbols that can be accessed by its name or ID.,
///
/// The [`NamedMap`] is a wrapper around an [`Arena`] that allows for accessing its symbols by name.
/// However, accessing symbols by ID provides better lookup performance and should be preferred.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NamedMap<T> {
    ids_by_name: HashMap<String, ID<T>>,
    items: Arena<T>,
}

impl<T> Default for NamedMap<T> {
    fn default() -> Self { Self::new() }
}

impl<T> NamedMap<T> {
    /// Creates a new, empty [`NamedMap`].
    #[must_use]
    pub fn new() -> Self {
        Self {
            ids_by_name: HashMap::new(),
            items: Arena::new(),
        }
    }

    /// Adds a symbol to the [`NamedMap`] and returns its ID.
    ///
    /// # Errors
    /// Returns [`Err`] to the newly inserted symbol if the name is already taken.
    /// Retutns [`Ok`] to the newly inserted symbol if the name is not taken.
    pub fn insert(&mut self, name: String, item: T) -> Result<ID<T>, ID<T>> {
        let new_id = self.items.push(item);
        match self.ids_by_name.insert(name, new_id) {
            Some(_) => Err(new_id),
            None => Ok(new_id),
        }
    }

    /// Returns the number of items in the [`NamedMap`].
    #[must_use]
    pub fn len(&self) -> usize { self.items.len() }

    /// Returns `true` if the [`NamedMap`] contains no items.
    #[must_use]
    pub fn is_empty(&self) -> bool { self.items.is_empty() }

    /// Returns a reference to the item in the [`NamedMap`] with the given ID.
    #[must_use]
    pub fn get(&self, id: ID<T>) -> Option<&Symbol<T>> { self.items.get(id) }

    /// Returns a mutable reference to the item in the [`NamedMap`] with the given ID.
    #[must_use]
    pub fn get_mut(&mut self, id: ID<T>) -> Option<&mut Symbol<T>> { self.items.get_mut(id) }

    /// Returns a reference to the item in the [`NamedMap`] with the given name.
    #[must_use]
    pub fn get_by_name<Q: ?Sized + Hash + Eq>(&self, name: &Q) -> Option<&Symbol<T>>
    where
        String: Borrow<Q>,
    {
        let id = self.ids_by_name.get(name)?;
        self.items.get(*id)
    }

    /// Returns a mutable reference to the item in the [`NamedMap`] with the given name.
    #[must_use]
    pub fn get_mut_by_name<Q: ?Sized + Hash + Eq>(&mut self, name: &Q) -> Option<&mut Symbol<T>>
    where
        String: Borrow<Q>,
    {
        let id = self.ids_by_name.get(name)?;
        self.items.get_mut(*id)
    }

    /// Returns an iterator over the items in the [`NamedMap`].
    pub fn iter(&self) -> std::slice::Iter<'_, Symbol<T>> { self.items.iter() }

    /// Returns a mutable iterator over the items in the [`NamedMap`].
    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, Symbol<T>> { self.items.iter_mut() }

    /// Gets the ID of the item in the [`NamedMap`] with the given name.
    #[must_use]
    pub fn name_to_id<Q: ?Sized + Hash + Eq>(&self, name: &Q) -> Option<ID<T>>
    where
        String: Borrow<Q>,
    {
        self.ids_by_name.get(name).copied()
    }
}

impl<T> std::ops::Index<ID<T>> for NamedMap<T> {
    type Output = Symbol<T>;

    fn index(&self, id: ID<T>) -> &Self::Output { &self.items[id] }
}

impl<T> std::ops::IndexMut<ID<T>> for NamedMap<T> {
    fn index_mut(&mut self, id: ID<T>) -> &mut Self::Output { &mut self.items[id] }
}

impl<T, Q: ?Sized + Eq + Hash> std::ops::Index<&Q> for NamedMap<T>
where
    String: Borrow<Q>,
{
    type Output = Symbol<T>;

    fn index(&self, name: &Q) -> &Self::Output {
        let id = self.ids_by_name.get(name).unwrap();
        &self.items[*id]
    }
}

impl<T, Q: ?Sized + Eq + Hash> std::ops::IndexMut<&Q> for NamedMap<T>
where
    String: Borrow<Q>,
{
    fn index_mut(&mut self, index: &Q) -> &mut Self::Output {
        let id = self.ids_by_name.get(index).unwrap();
        &mut self.items[*id]
    }
}
