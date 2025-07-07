//! A module for loading source files and building the initial engine state.

use std::{fmt::Debug, hash::Hash, path::Path, sync::Arc};

use pernixc_query::Identifiable;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

/// Query for loading source files content from the file system.
///
/// This query is im-pure and should be re-evaluated every time it's loaded from
/// the persistence layer to trigger the re-verification of the query that
/// depends on it.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Identifiable, StableHash,
)]
pub struct Key<P, T> {
    /// The path to load the source file.
    pub path: P,

    /// The target name that requested the source file.
    pub target_name: T,
}

/// The string formatted error from the [`std::io::Error`] when loading
/// the source file.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    Identifiable,
    StableHash,
)]
pub struct LoadSourceFileError(pub Arc<str>);

impl<
        P: AsRef<Path>
            + Debug
            + StableHash
            + Identifiable
            + Hash
            + Eq
            + Clone
            + Send
            + Sync
            + 'static,
        T: AsRef<str>
            + Debug
            + StableHash
            + Identifiable
            + Hash
            + Eq
            + Clone
            + Send
            + Sync
            + 'static,
    > pernixc_query::Key for Key<P, T>
{
    /// The [`Ok`] value represents the source file content, while the [`Err`]
    /// is the string to report the error.
    type Value = Result<Arc<String>, LoadSourceFileError>;
}

/// The executor used by the [`Key`] to load the source file
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Executor;

fn load_source_file(path: &Path) -> Result<Arc<String>, LoadSourceFileError> {
    std::fs::read_to_string(path)
        .map_err(|x| LoadSourceFileError(Arc::from(x.to_string())))
        .map(Arc::new)
}

impl<
        P: AsRef<Path>
            + Debug
            + StableHash
            + Identifiable
            + Hash
            + Eq
            + Clone
            + Send
            + Sync
            + 'static,
        T: AsRef<str>
            + Debug
            + StableHash
            + Identifiable
            + Hash
            + Eq
            + Clone
            + Send
            + Sync
            + 'static,
    > pernixc_query::runtime::executor::Executor<Key<P, T>> for Executor
{
    fn execute(
        &self,
        _: &pernixc_query::TrackedEngine,
        key: &Key<P, T>,
    ) -> Result<
        Result<Arc<String>, LoadSourceFileError>,
        pernixc_query::runtime::executor::CyclicError,
    > {
        Ok(load_source_file(key.path.as_ref()))
    }
}
