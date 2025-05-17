//! Implements a query system powering the incremental compilation and semantic
//! analysis

use std::hash::Hash;

pub mod map;

/// A trait representing a key that can be used to store and retrieve values
/// inside a [`map::Map`].
pub trait Key: 'static + Send + Sync + Eq + Hash {
    /// The corresponding value type for this key
    type Value: 'static + Send + Sync + Clone;
}
