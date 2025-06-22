//! Contains the runtime components for the query system.

pub mod executor;
pub mod persistence;
pub mod serde;

/// Represents the runtime environment for the query system.
///
/// The `Runtime` struct serves as a container for essential components
/// required to execute and serialize queries in the system.
///
/// # Fields
/// - `executor`: Manages the execution of queries through a registry of
///   executors.
/// - `serde`: Handles serialization and deserialization of query data.
#[derive(Debug, Default)]
pub struct Runtime {
    /// Registry for managing query executors.
    pub executor: executor::Registry,

    /// Optional persistence layer for storing and retrieving query data.
    ///
    /// Typically, if the incremental directory flag is passed, this field
    /// will be populated with a `Persistence` instance that manages the
    /// serialization and deserialization of the query database.
    pub persistence: Option<persistence::Persistence>,
}
