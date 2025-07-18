//! Contains the definition of the [`Errors`] type.

use std::sync::Arc;

use pernixc_diagnostic::{Diagnostic, Report};
use pernixc_query::{TrackedEngine, Value};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
use pernixc_target::TargetID;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

use crate::{
    load::Error, source_map::SourceMap, token_tree::get_source_file_token_tree,
    RecursiveFileRequest,
};

/// Contains all the errors that occurred building the syntax tree in a
/// particular file.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct Syntactic {
    /// The list of lexical errors that occurred while parsing the
    pub lexicals: Arc<[pernixc_lexical::error::Error]>,

    /// The list of parser errors that occurred while parsing the
    pub parsers: Arc<[pernixc_parser::error::Error]>,
}

/// List of errors that occurred while building the module tree.
#[derive(
    Debug, Clone, PartialEq, Eq, Serialize, Deserialize, StableHash, Value,
)]
#[id(TargetID)]
#[value(Result<Errors, Error>)]
#[allow(clippy::type_complexity)]
pub struct Errors {
    /// The list of lexical and parser errors that occurred while parsing the
    /// token tree.
    pub syntactic_errors: Arc<[Syntactic]>,

    /// The recursive request error, if any.
    ///
    /// This can only appar in the root of the file tree
    pub recursive_request_error: Option<Arc<RecursiveFileRequest>>,
}

/// An executor for collecting errors from the module tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Executor;

impl pernixc_query::runtime::executor::Executor<Key> for Executor {
    fn execute(
        &self,
        engine: &pernixc_query::TrackedEngine,
        &Key(target_id): &Key,
    ) -> Result<
        Result<Errors, Error>,
        pernixc_query::runtime::executor::CyclicError,
    > {
        let file_map = match engine.query(&crate::MapKey(target_id))? {
            Ok(file_map) => file_map,
            Err(err) => return Ok(Err(err)),
        };

        let recursive_request_error = engine
            .query(&crate::Key::Root(target_id))?
            .ok()
            .and_then(|x| x.recursive_request_error.clone());

        let syntactic_errors = file_map
            .par_iter()
            .filter_map(|x| {
                let Ok(token_tree) = engine
                    .query(&crate::token_tree::ErrorKey {
                        path: x.value().clone(),
                        target_id,
                    })
                    .unwrap()
                else {
                    return None;
                };

                let Ok(syntax_tree) = engine
                    .query(&crate::syntax_tree::ErrorKey {
                        path: x.value().clone(),
                        target_id,
                    })
                    .unwrap()
                else {
                    return None;
                };

                Some(Syntactic { lexicals: token_tree, parsers: syntax_tree })
            })
            .collect::<Vec<_>>();

        Ok(Ok(Errors {
            syntactic_errors: syntactic_errors.into(),
            recursive_request_error,
        }))
    }
}

/// A list of errors that has been rendered in a form of [`Diagnostic`] from the
/// [`Errors`]
#[derive(
    Debug, Clone, PartialEq, Eq, Serialize, Deserialize, StableHash, Value,
)]
#[id(TargetID)]
#[key(RenderedKey)]
#[value(Result<Rendered, Error>)]
#[extend(method(get_file_tree_rendered_errors), no_cyclic)]
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
        Result<Rendered, Error>,
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
                let Syntactic { lexicals, parsers } = x;

                lexicals
                    .par_iter()
                    .map(|error| error.report(&source_map))
                    .chain(parsers.par_iter().map(|error| {
                        let token_tree =
                            engine.get_source_file_token_tree(error.source_id);
                        error.report(&token_tree)
                    }))
            })
            .chain(
                errors
                    .recursive_request_error
                    .as_ref()
                    .map(|x| x.report(engine)),
            )
            .collect::<Vec<_>>();

        Ok(Ok(Rendered(errors.into())))
    }
}
