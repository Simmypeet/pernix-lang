//! Contains the definition of the [`Errors`] type.

use std::sync::Arc;

use pernixc_arena::ID;
use pernixc_diagnostic::{Diagnostic, Report};
use pernixc_query::{TrackedEngine, Value};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::{ByteIndex, SourceFile};
use pernixc_stable_hash::StableHash;
use pernixc_target::TargetID;
use rayon::iter::{
    IntoParallelIterator, IntoParallelRefIterator, ParallelIterator,
};

use crate::{
    load_source_file::LoadSourceFileError, source_map::SourceMap,
    token_tree::get_source_file_token_tree, ModuleTree,
};

/// List of errors that occurred while building the module tree.
#[derive(
    Debug, Clone, PartialEq, Eq, Serialize, Deserialize, StableHash, Value,
)]
#[id(TargetID)]
#[value(Result<Errors, LoadSourceFileError>)]
#[allow(clippy::type_complexity)]
pub struct Errors {
    /// The list of lexical and parser errors that occurred while parsing the
    /// token tree.
    pub syntactic_errors: Arc<
        [(
            Arc<[pernixc_lexical::error::Error]>,
            Arc<[pernixc_parser::error::Error]>,
        )],
    >,

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

        let mut module_tree_with_source_file = Vec::new();
        collect_source_files(
            &mut module_tree_with_source_file,
            &parse.root_module_tree,
        );

        let syntactic_errors = module_tree_with_source_file
            .into_par_iter()
            .flat_map(|source_id| {
                let Ok(token_tree) = engine
                    .query(&crate::token_tree::Parse {
                        path: parse
                            .source_file_paths_by_id
                            .get(&source_id)
                            .cloned()
                            .unwrap(),
                        target_id: key.0,
                        global_source_id: key.0.make_global(source_id),
                    })
                    .unwrap()
                else {
                    return None;
                };

                let Ok(syntax_tree) = engine
                    .query(&crate::syntax_tree::Key {
                        path: parse
                            .source_file_paths_by_id
                            .get(&source_id)
                            .cloned()
                            .unwrap(),
                        target_id: key.0,
                        global_source_id: key.0.make_global(source_id),
                    })
                    .unwrap()
                else {
                    return None;
                };

                Some((token_tree.errors, syntax_tree.errors))
            })
            .collect::<Vec<_>>();

        Ok(Ok(Errors {
            branching: parse.branching_errors.clone(),
            syntactic_errors: syntactic_errors.into(),
        }))
    }
}

fn collect_source_files(
    module_tree_with_source_file: &mut Vec<ID<SourceFile>>,
    module_tree: &ModuleTree,
) {
    if let Some(source_file) = module_tree.created_from_source_id {
        module_tree_with_source_file.push(source_file);
    }

    for submodule in module_tree.submodules_by_name.values() {
        collect_source_files(module_tree_with_source_file, submodule);
    }
}

/// A list of errors that has been rendered in a form of [`Diagnostic`] from the
/// [`Errors`]
#[derive(
    Debug, Clone, PartialEq, Eq, Serialize, Deserialize, StableHash, Value,
)]
#[id(TargetID)]
#[key(RenderedKey)]
#[value(Result<Rendered, LoadSourceFileError>)]
#[extend(method(get_module_tree_rendered_errors), no_cyclic)]
pub struct Rendered(pub Arc<[Diagnostic<ByteIndex>]>);

/// An executor for rendering errors from the module tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct RenderedExecutor;

impl pernixc_query::runtime::executor::Executor<RenderedKey>
    for RenderedExecutor
{
    fn execute(
        &self,
        engine: &TrackedEngine,
        key: &RenderedKey,
    ) -> Result<
        Result<Rendered, LoadSourceFileError>,
        pernixc_query::runtime::executor::CyclicError,
    > {
        let errors = match engine.query(&Key(key.0)).unwrap() {
            Ok(errors) => errors,
            Err(err) => return Ok(Err(err)),
        };

        let source_map = SourceMap(engine);

        let errors = errors
            .syntactic_errors
            .as_ref()
            .par_iter()
            .flat_map(|x| {
                let (lexical_errors, parser_errors) = x;

                lexical_errors
                    .par_iter()
                    .map(|error| error.report(&source_map))
                    .chain(parser_errors.par_iter().map(|error| {
                        let token_tree =
                            engine.get_source_file_token_tree(error.source_id);
                        error.report(&token_tree)
                    }))
            })
            .chain(
                errors
                    .branching
                    .par_iter()
                    .map(|error| error.report(source_map.0)),
            )
            .collect::<Vec<_>>();

        Ok(Ok(Rendered(errors.into())))
    }
}
