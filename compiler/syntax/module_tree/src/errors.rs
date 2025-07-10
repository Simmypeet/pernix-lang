//! Contains the definition of the [`Errors`] type.

use std::sync::Arc;

use pernixc_query::{TrackedEngine, Value};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::TargetID;

use crate::{load_source_file::LoadSourceFileError, ModuleTree, Parse};

/// List of errors that occurred while building the module tree.
#[derive(
    Debug, Clone, PartialEq, Eq, Serialize, Deserialize, StableHash, Value,
)]
#[id(TargetID)]
#[value(Result<Errors, LoadSourceFileError>)]
pub struct Errors {
    /// The list of errors that occurred while parsing the token tree.
    pub token_tree: Arc<[Arc<[pernixc_lexical::error::Error]>]>,

    /// The list of errors that occurred while parsing the syntax tree.
    pub syntax_tree: Arc<[Arc<[pernixc_parser::error::Error]>]>,

    /// The list of branching errors that occurred while branching the module
    /// tree.
    pub branching: Arc<[crate::Error]>,
}

/// An executor for collecting errors from the module tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Executor;

impl pernixc_query::runtime::executor::Executor<Key> for Executor {
    fn execute(
        &self,
        engine: &pernixc_query::TrackedEngine,
        key: &Key,
    ) -> Result<
        Result<Errors, LoadSourceFileError>,
        pernixc_query::runtime::executor::CyclicError,
    > {
        let parse = match engine.query(&crate::Key(key.0)).unwrap() {
            Ok(module_tree) => module_tree,
            Err(error) => return Ok(Err(error)),
        };

        let mut token_tree_errors = Vec::new();
        let mut syntax_tree_errors = Vec::new();

        Errors::collect(
            &mut token_tree_errors,
            &mut syntax_tree_errors,
            engine,
            &parse.root_module_tree,
            &parse,
            key.0,
        );

        Ok(Ok(Errors {
            token_tree: token_tree_errors.into(),
            syntax_tree: syntax_tree_errors.into(),
            branching: parse.branching_errors.clone(),
        }))
    }
}

impl Errors {
    fn collect(
        token_tree_errors: &mut Vec<Arc<[pernixc_lexical::error::Error]>>,
        syntax_tree_errors: &mut Vec<Arc<[pernixc_parser::error::Error]>>,
        engine: &TrackedEngine,
        module_tree: &ModuleTree,
        parse: &Parse,
        target_id: TargetID,
    ) {
        if let Some(created_from_source_id) = module_tree.created_from_source_id
        {
            let Ok(token_tree) = engine
                .query(&crate::token_tree::Parse {
                    path: parse
                        .source_file_paths_by_id
                        .get(&created_from_source_id)
                        .cloned()
                        .unwrap(),
                    target_id,
                    global_source_id: target_id
                        .make_global(created_from_source_id),
                })
                .unwrap()
            else {
                return;
            };

            token_tree_errors.push(token_tree.errors);

            let Ok(syntax_tree) = engine
                .query(&crate::syntax_tree::Key {
                    path: parse
                        .source_file_paths_by_id
                        .get(&created_from_source_id)
                        .cloned()
                        .unwrap(),
                    target_id,
                    global_source_id: target_id
                        .make_global(created_from_source_id),
                })
                .unwrap()
            else {
                return;
            };

            syntax_tree_errors.push(syntax_tree.errors);
        }

        for submodule in module_tree.submodules_by_name.values() {
            Self::collect(
                token_tree_errors,
                syntax_tree_errors,
                engine,
                submodule,
                parse,
                target_id,
            );
        }
    }
}
