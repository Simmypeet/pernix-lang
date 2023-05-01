//! Contains a definition of [`ErrorHandler`] trait -- a trait responsible for handling compilation
//! errors and warnings in the compiler.
//!
//! Also contains ssome implementations of [`ErrorHandler`] trait.

use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};

/// Represents a trait responsible for handling compilation errors and warnings in the compiler.
pub trait ErrorHandler<T>: 'static + Send + Sync {
    /// Recieves an error and handles it.
    fn recieve(&self, error: T);
}

/// Is a struct that implements [`ErrorHandler`] trait by storing all errors in a vector.
#[derive(Debug)]
pub struct ErrorVec<T: 'static + Send + Sync> {
    errors: RwLock<Vec<T>>,
}

impl<T: 'static + Send + Sync> ErrorVec<T> {
    /// Creates a new empty [`ErrorVec`]
    #[must_use]
    pub fn new() -> Self {
        Self {
            errors: RwLock::new(Vec::new()),
        }
    }

    /// Consumes the [`ErrorVec`] and returns the underlying vector of errors.
    pub fn into_vec(self) -> Vec<T> { self.errors.into_inner().unwrap() }

    /// Returns a reference to the underlying vector of errors.
    pub fn as_vec(&self) -> RwLockReadGuard<Vec<T>> { self.errors.read().unwrap() }

    /// Returns a mutable reference to the underlying vector of errors.
    pub fn as_vec_mut(&self) -> RwLockWriteGuard<Vec<T>> { self.errors.write().unwrap() }
}

impl<T: 'static + Send + Sync> Default for ErrorVec<T> {
    fn default() -> Self { Self::new() }
}

impl<T: 'static + Send + Sync, U> ErrorHandler<U> for ErrorVec<T>
where
    U: Into<T>,
{
    fn recieve(&self, error: U) { self.errors.write().unwrap().push(error.into()); }
}

/// Is a struct that implements [`ErrorHandler`] trait by doing nothing with the errors.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Dummy;

impl<T> ErrorHandler<T> for Dummy {
    fn recieve(&self, _error: T) {}
}
