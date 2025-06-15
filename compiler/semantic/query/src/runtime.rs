//! Contains the runtime components for the query system.

pub mod executor;

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
}
