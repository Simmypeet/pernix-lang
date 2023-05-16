//! Contains definitions for the arena and symbol.

use std::{
    collections::HashMap,
    fmt::Debug,
    hash::Hash,
    sync::atomic::{AtomicU64, Ordering},
};

use derive_more::{Deref, DerefMut};
use getset::CopyGetters;

/// Is a unique identifier that guarantees uniqueness across the entire program.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Uid(u64);

impl Uid {
    /// Creates a new [`Uid`] with a unique value.
    pub fn fresh() -> Self {
        static ATOMIC_COUNTER: AtomicU64 = AtomicU64::new(0);
        Self(ATOMIC_COUNTER.fetch_add(1, Ordering::Relaxed))
    }
}

/// Is a trait that allows generating unique identifiers for a type.
pub trait UniqueIdentifier:
    'static + Send + Sync + Debug + Clone + Copy + PartialEq + Eq + PartialOrd + Ord + Hash
{
    /// Creates a new [`UniqueIdentifier`] with a unique value.
    fn fresh() -> Self;
}

/// Represents a type that uses a [`UniqueIdentifier`] to identify itself uniquely.
pub trait Data {
    /// The type of the [`UniqueIdentifier`] used to identify the type.
    type ID: UniqueIdentifier;
}

/// Is a struct containing a [`UniqueIdentifier`] and a [`Data`] type.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Deref, DerefMut, CopyGetters)]
pub struct Symbol<T: Data> {
    /// Gets the [`UniqueIdentifier`] of the [`Symbol`].
    #[get_copy = "pub"]
    id: T::ID,

    #[deref]
    #[deref_mut]
    data: T,
}

impl<T: Data> Symbol<T> {
    /// Creates a new [`Symbol`] with a fresh [`UniqueIdentifier`].
    pub fn new(data: T) -> Self {
        Self {
            id: T::ID::fresh(),
            data,
        }
    }

    /// Dissolves the [`Symbol`] into its [`UniqueIdentifier`] and [`Data`] type.
    #[must_use]
    pub fn dissolve(self) -> (T::ID, T) { (self.id, self.data) }
}

/// Is a container that stores symbols identified by a [`UniqueIdentifier`].
///
/// This container is generally used to construct a graph of symbols. Semantic analysis phase
/// generally uses lots of graphs to represent the program. The symbols would use the
/// [`UniqueIdentifier`] to store the references to other symbols.
#[derive(Debug, Deref)]
pub struct Arena<T: Data> {
    symbols_by_id: HashMap<T::ID, Symbol<T>>,
}

impl<T: Data> Default for Arena<T> {
    fn default() -> Self { Self::new() }
}

impl<T: Data> Arena<T> {
    /// Creates a new empty [`Arena`].
    #[must_use]
    pub fn new() -> Self {
        Self {
            symbols_by_id: HashMap::new(),
        }
    }

    /// Inserts a new symbol into the [`Arena`].
    pub fn insert(&mut self, data: T) -> T::ID {
        let symbol = Symbol::new(data);
        let id = symbol.id;
        self.symbols_by_id.insert(id, symbol);
        id
    }

    /// Maps the [`Arena`] to a new [`Arena`] with a different [`Data`] type.
    pub fn map<U: Data<ID = T::ID>>(self, mut f: impl FnMut(T) -> U) -> Arena<U> {
        let mut arena = Arena::new();
        for (id, symbol) in self.symbols_by_id {
            arena.symbols_by_id.insert(id, Symbol {
                id,
                data: f(symbol.data),
            });
        }
        arena
    }

    /// Removes a symbol from the [`Arena`] with the given [`UniqueIdentifier`].
    ///
    /// Returns the symbol of the given [`UniqueIdentifier`] if it exists.
    ///
    /// # Errors
    /// - If the ID wasn't created by this [`Arena`].
    pub fn remove(&mut self, id: T::ID) -> Result<Symbol<T>, InvalidIDError> {
        self.symbols_by_id.remove(&id).ok_or(InvalidIDError)
    }

    /// Returns a reference to the symbol of the given [`UniqueIdentifier`].
    ///
    /// # Errors
    /// - If the ID wasn't created by this [`Arena`].
    pub fn get(&self, id: T::ID) -> Result<&Symbol<T>, InvalidIDError> {
        self.symbols_by_id.get(&id).ok_or(InvalidIDError)
    }

    /// Returns a mutable reference to the symbol of the given [`UniqueIdentifier`].
    ///
    /// # Errors
    /// - If the ID wasn't created by this [`Arena`].
    pub fn get_mut(&mut self, id: T::ID) -> Result<&mut Symbol<T>, InvalidIDError> {
        self.symbols_by_id.get_mut(&id).ok_or(InvalidIDError)
    }

    /// Returns a mutable iterator over the symbols in the [`Arena`].
    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut Symbol<T>> {
        self.symbols_by_id.values_mut()
    }
}

impl<T: Data> IntoIterator for Arena<T> {
    type IntoIter = std::collections::hash_map::IntoIter<T::ID, Symbol<T>>;
    type Item = (T::ID, Symbol<T>);

    fn into_iter(self) -> Self::IntoIter { self.symbols_by_id.into_iter() }
}

/// Signifies that the given ID to the container is invalid or wasn't created by that particular
/// container.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[error("The given ID is invalid or wasn't created by this container.")]
pub struct InvalidIDError;

/// Is a macro that generates a new ID type and implements the [`UniqueIdentifier`]
#[macro_export]
macro_rules! create_id_type {
    ($name:ident) => {
        $crate::arena::__private::paste! {
            #[doc = concat!("Is a unique identifier for a [`", stringify!($name), "`].")]
            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            pub struct [< $name ID >]($crate::arena::Uid);

            impl $crate::arena::UniqueIdentifier for [< $name ID >] {
                fn fresh() -> Self { Self(pernixc_system::arena::Uid::fresh()) }
            }
        }
    };
    ($name: ident, $doc: literal) => {
        $crate::arena::__private::paste! {
            #[doc = $doc]
            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            pub struct [< $name ID >]($crate::arena::Uid);

        impl $crate::arena::UniqueIdentifier for [< $name ID >] {
            fn fresh() -> Self { Self(pernixc_system::arena::Uid::fresh()) }
            }
        }
    };
}

/// Is a macro generating a [`Data`] trait impl for a given type and its ID.
#[macro_export]
macro_rules! create_symbol {
    {
        $(#[$outer:meta])*
        $vis:vis $ty:ident $name:ident $(< $($lt:lifetime),* $($gt:ident $(: $tb:tt)?),* >)?
        { $($t:tt)* }
    } => {
        $crate::arena::__private::paste! {
            $(#[$outer])*
            $vis $ty [< $name >] $(< $($lt),* $($gt $( : $tb)?),* >)? { $($t)* }

            $crate::arena::create_id_type!($name);

            impl $(< $($lt),* $($gt $(: $tb)?),* >)? $crate::arena::Data for $name $(< $($lt),* $($gt),* >)? {
                type ID = [< $name ID >];
            }

            #[doc = concat!("Is a symbol with data of type [`", stringify!($name), "`].")]
            pub type [< $name Symbol >]$(< $($lt),* $($gt),* >)? = $crate::arena::Symbol<$name $(< $($lt),* $($gt),* >)?>;
        }
    };
}

#[doc(hidden)]
pub mod __private {
    pub use paste::paste;
}

pub use create_id_type;
pub use create_symbol;
pub use paste::paste;
use thiserror::Error;
