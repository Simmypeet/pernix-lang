//! Contains the definition of [`Table`]

use std::{
    fmt::{self, Debug},
    marker::PhantomData,
    ops::DerefMut,
};

use derive_more::{Deref, DerefMut};

use self::representation::{Container, NoContainer, Representation};

pub mod evaluate;
pub mod representation;
pub mod resolution;

/// Contaains the display object that requires the table.
///
/// Primarily used for implementing [`std::fmt::Display`] trait.
#[derive(Debug, Clone, Copy)]
#[allow(clippy::self_named_module_files)]
pub struct DisplayObject<'a, D: ?Sized, T: State> {
    /// The table in which the display object will refer to.
    pub table: &'a Table<T>,

    /// The display object that requires the table.
    pub display: &'a D,
}

impl<'a, Error: Display<T> + ?Sized, T: State> fmt::Display
    for DisplayObject<'a, Error, T>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display.fmt(self.table, f)
    }
}

/// Similar to [`std::fmt::Display`] but with the table in which the error
/// occurred.
pub trait Display<T: State> {
    #[allow(missing_docs, clippy::missing_errors_doc)]
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

/// A struct which implements [`State`] used to signify that the table is built
/// with some errors and is not suitable for the next phase (i.e. code
/// generation).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Suboptimal(());

impl State for Suboptimal {
    type Container = NoContainer;
}

/// The state object used for indicating no extra state.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct NoState;

/// The state object used for building the table.
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
    Deref,
    DerefMut,
)]
pub struct Building<
    T: Container = NoContainer,
    S: Debug + Sized + Send + Sync + 'static = NoState,
> {
    _phantom: PhantomData<T>,

    #[deref]
    #[deref_mut]
    inner_state: S,
}

impl<T: Container, S: Debug + Sized + Send + Sync + 'static> Building<T, S> {
    /// Creates a new building state object with the given inner state.
    pub fn new(state: S) -> Self {
        Self { _phantom: PhantomData, inner_state: state }
    }
}

impl<T: Container, S: Debug + Sized + Send + Sync + 'static> State
    for Building<T, S>
{
    type Container = T;
}

/// A struct which implements [`State`] used to signify that the table is built
/// successfully and is ready to be used for the next phase (i.e. code
/// generation).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Success(() /* Preventing arbitrary instantiation */);

impl State for Success {
    type Container = NoContainer;
}

/// Represents a state object for the [`Table`].
///
/// This is used to distinguish between the states of the symbols in the table.
#[doc(hidden)]
pub trait State: Debug + Sized + 'static + Send + Sync {
    /// The container type used to wrap the symbols in the table.
    type Container: Container;
}

/// Contains all the symbols and information defined in the target.
#[derive(Debug, derive_more::Deref)]
pub struct Table<T: State> {
    #[deref]
    representation: Representation<T::Container>,

    state: T,
}

impl<T: Container, S: Debug + Sized + Send + Sync + 'static> DerefMut
    for Table<Building<T, S>>
{
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.representation }
}

pub use representation::BuildTableError;

#[cfg(test)]
mod tests;
