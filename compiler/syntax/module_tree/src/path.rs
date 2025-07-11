#![allow(clippy::type_complexity)]

//! Provides a query to retrieve the source file paths mapping from
//! [`GlobalSourceID`] to [`Arc<Path>`] in a particular target's module tree.

use std::{path::Path, sync::Arc};

use pernixc_source_file::GlobalSourceID;

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
#[value(Arc<Path>)]
#[extend(method(get_source_file_path), no_cyclic)]
pub struct Key(pub GlobalSourceID);

/// An executor for the [`Key`] query that retrieves the source file paths from
/// the module tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Executor;

impl pernixc_query::runtime::executor::Executor<Key> for Executor {
    fn execute(
        &self,
        engine: &pernixc_query::TrackedEngine,
        key: &Key,
    ) -> Result<Arc<Path>, pernixc_query::runtime::executor::CyclicError> {
        let module_tree =
            match engine.query(&crate::Key(key.0.target_id)).unwrap() {
                Ok(module_tree) => module_tree,
                Err(error) => panic!("the `GlobalSourceID` error {error}"),
            };

        Ok(module_tree
            .source_file_paths_by_id
            .get(&key.0.id)
            .unwrap_or_else(|| {
                panic!("`GlobalSourceID` {:?} not found", key.0.id)
            })
            .clone())
    }
}
