//! Module for parsing a token tree from the source code string.

use std::{fmt::Debug, hash::Hash, path::Path, sync::Arc};

use pernixc_handler::Storage;
use pernixc_query::runtime::executor::CyclicError;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::GlobalSourceID;
use pernixc_stable_hash::StableHash;
use pernixc_target::TargetID;

use crate::{get_source_file_path, load_source_file::LoadSourceFileError};

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
    pernixc_query::Key,
)]
#[value(Result<TokenTree, LoadSourceFileError>)]
pub struct Parse {
    /// The path to load the source file.
    pub path: Arc<Path>,

    /// The target ID that requested the source file parsing.
    pub target_id: TargetID,

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

/// An executor for parsing a token tree from the source code string.
#[derive(Debug, Clone, Copy, Default)]
pub struct ParseExecutor;

impl pernixc_query::runtime::executor::Executor<Parse> for ParseExecutor {
    fn execute(
        &self,
        tracked_engine: &pernixc_query::TrackedEngine,
        key: &Parse,
    ) -> Result<Result<TokenTree, LoadSourceFileError>, CyclicError> {
        // load the source file
        let source_file =
            match tracked_engine.query(&crate::load_source_file::Key {
                path: key.path.clone(),
                target_id: key.target_id,
            })? {
                Ok(source_code) => source_code,
                Err(error) => return Ok(Err(error)),
            };

        let storage = Storage::<pernixc_lexical::error::Error>::default();
        let tree = pernixc_lexical::tree::Tree::from_source(
            source_file.content(),
            key.global_source_id,
            &storage,
        );

        Ok(Ok(TokenTree {
            token_tree: Arc::new(tree),
            errors: Arc::from(storage.into_vec()),
        }))
    }
}

/// A key for the retrieving a [`TokenTree`] that has been parsed from a
/// [`ModuleTree`] building process.
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
    pernixc_query::Key,
)]
#[value(Arc<pernixc_lexical::tree::Tree>)]
#[extend(method(get_source_file_token_tree), no_cyclic)]
pub struct Key(pub GlobalSourceID);

/// An executor for the [`Key`] query that retrieves the token tree from the
/// module tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct KeyExecutor;

impl pernixc_query::runtime::executor::Executor<Key> for KeyExecutor {
    fn execute(
        &self,
        engine: &pernixc_query::TrackedEngine,
        key: &Key,
    ) -> Result<
        Arc<pernixc_lexical::tree::Tree>,
        pernixc_query::runtime::executor::CyclicError,
    > {
        let path = engine.get_source_file_path(key.0);

        let token_tree = engine
            .query(&Parse {
                path,
                target_id: key.0.target_id,
                global_source_id: key.0,
            })
            .unwrap();

        Ok(token_tree.unwrap().token_tree)
    }
}
