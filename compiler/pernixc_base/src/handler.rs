//! A module for handling diagnostics in the compiler.

use std::sync::atomic::AtomicUsize;

use derive_more::{Deref, DerefMut};
use parking_lot::{RwLock, RwLockReadGuard, RwLockWriteGuard};

/// Represents a trait responsible for handling diagnostics in the compiler.
pub trait Handler<T>: Send + Sync {
    /// Receives an error and handles it.
    fn receive(&self, error: T);
}

/// Is a struct that implements [`Handler`] trait by storing all errors in a
/// vector.
#[derive(Debug, Deref, DerefMut)]
pub struct Storage<T: Send + Sync> {
    errors: RwLock<Vec<T>>,
}

impl<T: Send + Sync> Storage<T> {
    /// Creates a new empty [`Storage`]
    #[must_use]
    pub const fn new() -> Self { Self { errors: RwLock::new(Vec::new()) } }

    /// Consumes the [`Storage`] and returns the underlying vector of errors.
    pub fn into_vec(self) -> Vec<T> { self.errors.into_inner() }

    /// Returns a reference to the underlying vector of errors.
    pub fn as_vec(&self) -> RwLockReadGuard<Vec<T>> { self.errors.read() }

    /// Returns a mutable reference to the underlying vector of errors.
    pub fn as_vec_mut(&self) -> RwLockWriteGuard<Vec<T>> { self.errors.write() }

    /// Propagates all diagnostics to the given handler.
    ///
    /// The diagnostics within the storage are cleared and moved to the handler.
    pub fn propagate<U: From<T>, H: ?Sized + Handler<U>>(&self, handler: &H) {
        let errors = std::mem::take(&mut *self.as_vec_mut());

        for error in errors {
            handler.receive(error.into());
        }
    }

    /// Clears all diagnostics.
    pub fn clear(&self) { self.errors.write().clear(); }
}

impl<T: Send + Sync> Default for Storage<T> {
    fn default() -> Self { Self::new() }
}
impl<T: Send + Sync, U> Handler<U> for Storage<T>
where
    U: Into<T>,
{
    fn receive(&self, error: U) { self.errors.write().push(error.into()); }
}

/// Is a struct that implements [`Handler`] trait by doing nothing with the
/// errors.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Dummy;

impl<T> Handler<T> for Dummy {
    fn receive(&self, _error: T) {}
}

/// Is a struct that implements [`Handler`] trait by counting the number of
/// diagnostics received.
#[derive(Debug, Default)]
pub struct Counter {
    counter: AtomicUsize,
}

impl Counter {
    /// Returns the number of diagnostics received.
    #[must_use]
    pub fn count(&self) -> usize {
        self.counter.load(std::sync::atomic::Ordering::Relaxed)
    }

    /// Resets the counter to zero.
    pub fn reset(&self) {
        self.counter.store(0, std::sync::atomic::Ordering::Relaxed);
    }
}

impl<T> Handler<T> for Counter {
    fn receive(&self, _error: T) {
        self.counter.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    }
}

/// Handles a diagnostic by panicking and printing the error.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Panic;

impl<T: std::fmt::Debug> Handler<T> for Panic {
    fn receive(&self, error: T) {
        panic!("{:?}", error);
    }
}
