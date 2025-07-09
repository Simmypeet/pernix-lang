//! Provides a query to retrieve the source file paths mapping from
//! [`ID<SourceFile>`] to [`Arc<Path>`] in a particular target's module tree.

use std::{path::Path, sync::Arc};

use pernixc_arena::ID;
use pernixc_hash::HashMap;
use pernixc_source_file::SourceFile;
use pernixc_target::TargetID;

use crate::load_source_file::LoadSourceFileError;

/// A query key for retrieving a map from the [`ID<SourceFile>`] to the
/// [`Arc<Path>`] which the source file belongs to.
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
    pernixc_query::Key,
    pernixc_serialize::Serialize,
    pernixc_serialize::Deserialize,
    pernixc_stable_hash::StableHash,
)]
#[value(Result<Arc<HashMap<ID<SourceFile>, Arc<Path>>>, LoadSourceFileError>)]
pub struct Key(pub TargetID);

/// An executor for the [`Key`] query that retrieves the source file paths from
/// the module tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Executor;

impl pernixc_query::runtime::executor::Executor<Key> for Executor {
    fn execute(
        &self,
        engine: &pernixc_query::TrackedEngine,
        key: &Key,
    ) -> Result<
        Result<Arc<HashMap<ID<SourceFile>, Arc<Path>>>, LoadSourceFileError>,
        pernixc_query::runtime::executor::CyclicError,
    > {
        let module_tree = match engine.query(&crate::Key(key.0)).unwrap() {
            Ok(module_tree) => module_tree,
            Err(error) => return Ok(Err(error)),
        };

        Ok(Ok(module_tree.source_file_paths_by_id))
    }
}
