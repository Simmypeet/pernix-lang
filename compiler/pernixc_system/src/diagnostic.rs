//!  Contains the definition of [`Handler`] trait and its implementations.

use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};

use derive_more::{Deref, DerefMut};

/// Represents a trait responsible for handling compilation diagnostics in the compiler.
pub trait Handler<T>: Send + Sync {
    /// Recieves an error and handles it.
    fn recieve(&self, error: T);
}

/// Is a struct that implements [`Handler`] trait by storing all errors in a vector.
#[derive(Debug, Deref, DerefMut)]
pub struct Storage<T: Send + Sync> {
    errors: RwLock<Vec<T>>,
}

impl<T: Send + Sync> Storage<T> {
    /// Creates a new empty [`Storage`]
    #[must_use]
    pub fn new() -> Self {
        Self {
            errors: RwLock::new(Vec::new()),
        }
    }

    /// Consumes the [`Storage`] and returns the underlying vector of errors.
    pub fn into_vec(self) -> Vec<T> { self.errors.into_inner().unwrap() }

    /// Returns a reference to the underlying vector of errors.
    pub fn as_vec(&self) -> RwLockReadGuard<Vec<T>> { self.errors.read().unwrap() }

    /// Returns a mutable reference to the underlying vector of errors.
    pub fn as_vec_mut(&self) -> RwLockWriteGuard<Vec<T>> { self.errors.write().unwrap() }
}

impl<T: Send + Sync> Default for Storage<T> {
    fn default() -> Self { Self::new() }
}

impl<T: Send + Sync, U> Handler<U> for Storage<T>
where
    U: Into<T>,
{
    fn recieve(&self, error: U) { self.errors.write().unwrap().push(error.into()); }
}

/// Is a struct that implements [`Handler`] trait by doing nothing with the errors.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Dummy;

impl<T> Handler<T> for Dummy {
    fn recieve(&self, _error: T) {}
}
