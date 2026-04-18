//! Contains the definition of [`Inference`] type.

use std::{fmt::Debug, marker::PhantomData};

use qbice::{Decode, Encode, StableHash};

/// A new type wrapper for representing inference tyep variable when building
/// the IR.
///
/// Since the language only allows type inference in the function body, this
/// inference instance should only exist in the function body.
#[derive(Encode, Decode)]
pub struct Variable<T: ?Sized> {
    index: u64,

    _marker: PhantomData<Box<T>>,
}

impl<T: ?Sized> Variable<T> {
    /// Returns the index of the [`ID`].
    #[must_use]
    pub const fn index(&self) -> u64 { self.index }
}

unsafe impl<T> Send for Variable<T> {}
unsafe impl<T> Sync for Variable<T> {}

impl<T> Default for Variable<T> {
    fn default() -> Self { Self { index: 0, _marker: PhantomData } }
}

impl<T> Debug for Variable<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ID({})", self.index)
    }
}

impl<T> Variable<T> {
    /// Creates a new [`ID`] with the given index.
    #[must_use]
    pub const fn new(index: u64) -> Self {
        Self { index, _marker: PhantomData }
    }
}

impl<T> Clone for Variable<T> {
    fn clone(&self) -> Self { *self }
}

impl<T> Copy for Variable<T> {}

impl<T> PartialEq for Variable<T> {
    fn eq(&self, other: &Self) -> bool { self.index == other.index }
}

impl<T> Eq for Variable<T> {}

impl<T> PartialOrd for Variable<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for Variable<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.index.cmp(&other.index)
    }
}

impl<T> std::hash::Hash for Variable<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.index.hash(state);
    }
}

impl<T> StableHash for Variable<T> {
    fn stable_hash<H: qbice::stable_hash::StableHasher + ?Sized>(
        &self,
        state: &mut H,
    ) {
        self.index.stable_hash(state);
    }
}
