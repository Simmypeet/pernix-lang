//! Module for parsing a token tree from the source code string.

use std::{fmt::Debug, hash::Hash, path::Path, sync::Arc};

use flexstr::SharedStr;
use pernixc_handler::Storage;
use pernixc_query::{runtime::executor::CyclicError, Identifiable};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::GlobalSourceID;
use pernixc_stable_hash::StableHash;

use crate::source_file::LoadSourceFileError;

/// Query for parsing a token tree from the given source file path.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    Identifiable,
)]
pub struct Key {
    /// The path to load the source file.
    pub path: Arc<Path>,

    /// The target name that requested the source file.
    pub target_name: SharedStr,

    /// The ID to the source file in the global source map.
    pub global_source_id: GlobalSourceID,
}

/// A result from loading a source file and parse it to a [`TokenTree`] with its
/// errors.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct TokenTree {
    /// The parsed token tree from the source code.
    pub token_tree: Arc<pernixc_lexical::tree::Tree>,

    /// The list of errors that occurred while parsing the source code.
    pub errors: Arc<[pernixc_lexical::error::Error]>,
}

impl pernixc_query::Key for Key {
    type Value = Result<TokenTree, LoadSourceFileError>;
}

/// An executor for parsing a token tree from the source code string.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct Executor;

impl pernixc_query::runtime::executor::Executor<Key> for Executor {
    fn execute(
        &self,
        tracked_engine: &pernixc_query::TrackedEngine,
        key: &Key,
    ) -> Result<Result<TokenTree, LoadSourceFileError>, CyclicError> {
        // load the source file
        let source_code =
            match tracked_engine.query(&crate::source_file::Key {
                path: key.path.clone(),
                target_name: key.target_name.clone(),
            })? {
                Ok(source_code) => source_code,
                Err(error) => return Ok(Err(error)),
            };

        let storage = Storage::<pernixc_lexical::error::Error>::default();
        let tree = pernixc_lexical::tree::Tree::from_source(
            &source_code,
            key.global_source_id,
            &storage,
        );

        Ok(Ok(TokenTree {
            token_tree: Arc::new(tree),
            errors: Arc::from(storage.into_vec()),
        }))
    }
}
