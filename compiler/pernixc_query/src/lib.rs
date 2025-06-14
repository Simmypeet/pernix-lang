//! Implements a query system powering the incremental compilation and semantic
//! analysis

use getset::{CopyGetters, Getters};
pub use key::Key;
pub use pernixc_query_derive::{Key, Value};
pub use pernixc_stable_type_id::Identifiable;

#[doc(hidden)]
pub mod __internal {
    pub use std::result::Result;

    pub use pernixc_query_derive::{Key, Value};
    pub use serde::{Deserialize, Serialize};

    pub use crate::key::Key;
}

// so that this crate can use derive macro
extern crate self as pernixc_query;

pub mod database;
pub mod key;
pub mod runtime;
pub mod serde;

/// The central data structure for the Pernix compiler storing all the semantic
/// information about the program.
#[derive(Debug, Default, Getters, CopyGetters)]
pub struct Engine {
    /// The main database instance used for storing and retrieving
    /// compiler-related data.
    ///
    /// This field provides access to the underlying query database, which
    /// manages incremental compilation state and caches results for
    /// efficient recompilation.
    pub database: database::Database,

    /// The runtime environment for the query system, containing executors and
    /// serialization components.
    pub runtime: runtime::Runtime,
}

#[cfg(test)]
mod test;
