//! Contains the definition of [`State`]

use std::{collections::HashMap, hash::Hash};

use serde::{Deserialize, Serialize};

use crate::ID;

/// Implemented by all the state that will be stoed alongside the
/// [`crate::Arena`]
pub trait State<T> {
    /// The type of the ID used for identifying items in the arena.
    type ID: Eq + Hash + Copy;

    /// Got invoked when an explicit ID is inserted into the [`Arena`]. This
    /// allows the generator to update its internal state if needed.
    fn explict_insert_with_id(
        &mut self,
        id: &Self::ID,
        items: &HashMap<Self::ID, T>,
    );
}

/// A trait for generating unique IDs for items in the [`crate::Arena`].
pub trait Generator<T>: State<T> {
    /// Generates a new ID for an item in the [`crate::Arena`].
    fn next_id(&mut self, items: &HashMap<Self::ID, T>, value: &T) -> Self::ID;
}

/// A trait for rebinding an [`crate::Arena`]'s state to a different arena type.
pub trait Rebind<T, U>: State<T> {
    /// The result of the rebind operation, which is a new state for the new
    /// arena
    type Result: State<U>;

    /// Rebinds the current ID generator to a generator of a different arena
    /// type.
    fn rebind(self) -> Self::Result;

    /// Converts an ID from the different arena type to the current arena type.
    fn convert_rebound_id(
        rebound: &mut Self::Result,
        id: Self::ID,
    ) -> <Self::Result as State<U>>::ID;
}

/// Struct implementing the [`State`] trait that uses [`ID`] as the ID type.
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
pub struct Default;

impl<T> State<T> for Default {
    type ID = ID<T>;

    fn explict_insert_with_id(
        &mut self,
        _: &Self::ID,
        _: &HashMap<Self::ID, T>,
    ) {
        // No-op for default state
    }
}

/// A simple ID generator that uses a [`usize`] as the ID type and increments it
/// for each new item. This is the default ID generator used by the
/// [`crate::Arena`].
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
pub struct Serial(u64);

impl Serial {
    /// Creates a new [`Serial`] ID generator.
    #[must_use]
    pub const fn new() -> Self { Self(0) }
}

impl<T> State<T> for Serial {
    type ID = ID<T>;

    fn explict_insert_with_id(
        &mut self,
        id: &Self::ID,
        _: &HashMap<Self::ID, T>,
    ) {
        self.0 = std::cmp::max(self.0, id.index);
    }
}

impl<T> Generator<T> for Serial {
    fn next_id(&mut self, _: &HashMap<Self::ID, T>, _: &T) -> Self::ID {
        let next_id = self.0;
        self.0 += 1;

        ID::new(next_id)
    }
}

impl<T, U> Rebind<T, U> for Serial {
    type Result = Self;

    fn rebind(self) -> Self::Result { self }

    fn convert_rebound_id(_: &mut Self::Result, id: Self::ID) -> ID<U> {
        ID::new(id.index)
    }
}
