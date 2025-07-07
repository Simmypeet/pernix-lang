//! Module for parsing a token tree from the source code string.

use std::{fmt::Debug, hash::Hash, path::Path, sync::Arc};

use pernixc_parser::abstract_tree::AbstractTree;
use pernixc_query::{runtime::executor::CyclicError, Identifiable};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::GlobalSourceID;
use pernixc_stable_hash::StableHash;

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
    Default,
    StableHash,
    Serialize,
    Deserialize,
    Identifiable,
)]
pub struct Key<P, T> {
    /// The path to load the source file.
    pub path: P,

    /// The target name that requested the source file.
    pub target_name: T,

    /// The ID to the source file in the global source map.
    pub global_source_id: GlobalSourceID,
}

/// A result from loading a source file and parse it to a
/// [`pernixc_syntax::item::module::Module`] with its errors.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct FileSyntaxTree {
    /// The parsed syntax tree from the source code.
    pub syntax_tree: Option<pernixc_syntax::item::module::Module>,

    /// The list of errors that occurred while parsing the source code.
    pub errors: Arc<[pernixc_parser::error::Error]>,
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
    > pernixc_query::Key for Key<P, T>
{
    type Value = Result<FileSyntaxTree, LoadSourceFileError>;
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
        tracked_engine: &pernixc_query::TrackedEngine,
        key: &Key<P, T>,
    ) -> Result<Result<FileSyntaxTree, LoadSourceFileError>, CyclicError> {
        // load the token tree
        let token_tree =
            match tracked_engine.query(&crate::token_tree::Key {
                path: key.path.clone(),
                target_name: key.target_name.clone(),
                global_source_id: key.global_source_id,
            })? {
                Ok(source_code) => source_code,
                Err(error) => return Ok(Err(error)),
            };

        let (module, errors) =
            pernixc_syntax::item::module::Module::parse(&token_tree.token_tree);

        Ok(Ok(FileSyntaxTree {
            syntax_tree: module,
            errors: Arc::from(errors),
        }))
    }
}
