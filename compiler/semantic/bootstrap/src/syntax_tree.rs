//! Module for parsing a token tree from the source code string.

use std::{fmt::Debug, hash::Hash, path::Path, sync::Arc};

use pernixc_parser::abstract_tree::AbstractTree;
use pernixc_query::runtime::executor::CyclicError;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::GlobalSourceID;
use pernixc_stable_hash::StableHash;
use pernixc_target::TargetID;

use crate::source_file::LoadSourceFileError;

/// Query for parsing a [`pernixc_syntax::item::module::Module`] from the given
/// source file path.
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
#[value(Result<SyntaxTree, LoadSourceFileError>)]
pub struct Key {
    /// The path to load the source file.
    pub path: Arc<Path>,

    /// The target ID that requested the source file parsing.
    pub target_id: TargetID,

    /// The ID to the source file in the global source map.
    pub global_source_id: GlobalSourceID,
}

/// A result from loading a source file and parse it to a
/// [`pernixc_syntax::item::module::Module`] with its errors.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct SyntaxTree {
    /// The parsed syntax tree from the source code.
    pub syntax_tree: Option<pernixc_syntax::item::module::Content>,

    /// The list of errors that occurred while parsing the source code.
    pub errors: Arc<[pernixc_parser::error::Error]>,
}

/// An executor for parsing a [`pernixc_syntax::item::module::Module`] from the
/// source file.
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
    ) -> Result<Result<SyntaxTree, LoadSourceFileError>, CyclicError> {
        // load the token tree
        let token_tree =
            match tracked_engine.query(&crate::token_tree::Key {
                path: key.path.clone(),
                target_id: key.target_id,
                global_source_id: key.global_source_id,
            })? {
                Ok(source_code) => source_code,
                Err(error) => return Ok(Err(error)),
            };

        let (module, errors) = pernixc_syntax::item::module::Content::parse(
            &token_tree.token_tree,
        );

        Ok(Ok(SyntaxTree { syntax_tree: module, errors: Arc::from(errors) }))
    }
}
