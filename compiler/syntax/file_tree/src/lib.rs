//! Manages a module tree creation and syntactic information of the compilation
//! target.

use std::{
    hash::{BuildHasher, BuildHasherDefault},
    path::{Path, PathBuf},
    sync::Arc,
};

use pernixc_arena::ID;
use pernixc_diagnostic::Report;
use pernixc_extend::extend;
use pernixc_hash::{DashMap, ReadOnlyView};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{
    runtime::{
        executor,
        executor::CyclicError,
        persistence::{serde::DynamicRegistry, Persistence},
    },
    TrackedEngine,
};
use pernixc_serialize::{
    de::Deserializer, ser::Serializer, Deserialize, Serialize,
};
use pernixc_source_file::{ByteIndex, GlobalSourceID, SourceFile};
use pernixc_stable_hash::StableHash;
use pernixc_syntax::Passable;
use pernixc_target::{get_invocation_arguments, get_target_seed, TargetID};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

use crate::{
    load::Error, source_map::to_absolute_span, syntax_tree::ModuleContent,
};

pub mod errors;
pub mod load;
pub mod source_map;
pub mod syntax_tree;
pub mod token_tree;

/// Registers all the required executors to run the queries.
pub fn register_executors(executor: &mut executor::Registry) {
    executor.register(Arc::new(load::Executor));
    executor.register(Arc::new(source_map::Executor));

    executor.register(Arc::new(token_tree::ParseExecutor));
    executor.register(Arc::new(token_tree::KeyExecutor));
    executor.register(Arc::new(token_tree::ErrorExecutor));

    executor.register(Arc::new(syntax_tree::Executor));
    executor.register(Arc::new(syntax_tree::ErrorExecutor));

    executor.register(Arc::new(errors::Executor));
    executor.register(Arc::new(errors::RenderedExecutor));

    executor.register(Arc::new(Executor));
    executor.register(Arc::new(MapExecutor));
    executor.register(Arc::new(PathExecutor));
}

/// Registers all the necessary runtime information for the query engine.
pub fn register_serde<
    S: Serializer<Registry>,
    D: Deserializer<Registry>,
    Registry: DynamicRegistry<S, D> + Send + Sync,
>(
    serde_registry: &mut Registry,
) where
    S::Error: Send + Sync,
{
    serde_registry.register::<load::Key>();

    serde_registry.register::<source_map::Key>();

    serde_registry.register::<token_tree::Parse>();
    serde_registry.register::<token_tree::Key>();
    serde_registry.register::<token_tree::ErrorKey>();

    serde_registry.register::<syntax_tree::Key>();
    serde_registry.register::<syntax_tree::ErrorKey>();

    serde_registry.register::<errors::Key>();
    serde_registry.register::<errors::RenderedKey>();

    serde_registry.register::<Key>();
    serde_registry.register::<MapKey>();
    serde_registry.register::<PathKey>();
}

/// Registers the keys that should be skipped during serialization and
/// deserialization in the query engine's persistence layer
pub fn skip_persistence(persistence: &mut Persistence) {
    persistence.skip_cache_value::<load::Key>();
    persistence.skip_cache_value::<source_map::Key>();
}

/// A query for retrieving a node in the file tree.
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
    StableHash,
    pernixc_query::Key,
)]
#[value(Result<Arc<ChildFiles>, Error>)]
pub enum Key {
    /// A root node for the module tree.
    Root(TargetID),

    /// Load the submodule from the source file.
    File {
        /// The path to the source file that must be loaded
        path: Arc<Path>,

        /// The target that requested the source file loading
        target_id: TargetID,
    },
}

/// The `public module someFile` ended up requesting the current file itself.
/// This only happens at the root of the file tree, where the file name is the
/// same as the submodule name.
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
)]
pub struct RecursiveFileRequest {
    /// The span of the submodule declaration in the root file.
    pub submodule_span: RelativeSpan,

    /// The path to the source file that was requested.
    pub path: PathBuf,
}

impl Report<&TrackedEngine<'_>> for RecursiveFileRequest {
    type Location = ByteIndex;

    fn report(
        &self,
        engine: &TrackedEngine<'_>,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        pernixc_diagnostic::Diagnostic {
            span: Some((
                engine.to_absolute_span(&self.submodule_span),
                Some(
                    "this module declaration causes recursive loading"
                        .to_string(),
                ),
            )),
            message: format!(
                "the module declaration ened up loading the current file `{}`",
                self.path.display()
            ),
            severity: pernixc_diagnostic::Severity::Error,
            help_message: Some(format!(
                "try changing the module name from `{}` to something else \
                 that is not the same as the file name",
                self.path.file_stem().unwrap_or_default().to_string_lossy()
            )),
            related: Vec::new(),
        }
    }
}

/// Represents the collection of files that current file declares as its
/// children.
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
    StableHash,
)]
pub struct ChildFiles {
    /// The list of child files in the file tree.
    pub files: Arc<[Arc<Path>]>,

    /// The recursive request error, if any.
    ///
    /// This can only appar in the root of the file tree
    pub recursive_request_error: Option<Arc<RecursiveFileRequest>>,
}

#[allow(dead_code)]
fn parse_node_from_module_content(
    content: Option<ModuleContent>,
    current_path: &Path,
    is_root: bool,
) -> (Vec<Arc<Path>>, Option<Arc<RecursiveFileRequest>>) {
    // empty file/module
    let Some(content) = content else {
        return (vec![], None);
    };

    let mut child_files = vec![];
    let mut recursive_request_error = None;

    for member in content.members() {
        let Passable::Line(pernixc_syntax::item::module::Member::Module(
            submodule,
        )) = member
        else {
            continue;
        };

        let next_signature = submodule.signature();

        // extract the identifier from the signature
        let Some(next_identifier) = next_signature
            .as_ref()
            .and_then(pernixc_syntax::item::module::Signature::identifier)
        else {
            continue;
        };

        let content = submodule.inline_body();

        let mut next_path = if is_root {
            current_path.parent().map_or_else(
                || PathBuf::from(next_identifier.kind.0.as_str()),
                |x| x.join(next_identifier.kind.0.as_str()),
            )
        } else {
            let mut next_path = current_path.to_path_buf();

            // replace the file stem that has `.pnx` extension with the folder
            // of the same name and append the submodule name with `.pnx`
            // extension
            next_path.set_extension("");
            next_path.push(next_identifier.kind.0.as_str());

            next_path
        };
        next_path.set_extension("pnx");

        if let Some(module) = content {
            child_files.append(
                &mut parse_node_from_module_content(
                    module.content().map(ModuleContent),
                    &next_path,
                    false,
                )
                .0,
            );
        } else {
            if is_root {
                let root_file_name = current_path
                    .file_stem()
                    .unwrap_or_default()
                    .to_string_lossy();

                if next_identifier.kind.0 == root_file_name.as_ref() {
                    recursive_request_error =
                        Some(Arc::new(RecursiveFileRequest {
                            submodule_span: next_identifier.span,
                            path: next_path,
                        }));

                    // don't add the file to the list of child files, will cause
                    // loop
                    continue;
                }
            }

            child_files.push(Arc::from(next_path));
        }
    }

    (child_files, recursive_request_error)
}

#[pernixc_query::executor(key(Key), name(Executor))]
pub fn node_executor(
    key: &Key,
    engine: &TrackedEngine,
) -> Result<Result<Arc<ChildFiles>, Error>, CyclicError> {
    let (path, target_id, is_root) = match key {
        Key::Root(target_id) => {
            let invocation_argument =
                engine.get_invocation_arguments(*target_id);

            (
                Arc::from(invocation_argument.command.input().file.clone()),
                *target_id,
                true,
            )
        }

        Key::File { path, target_id } => (path.clone(), *target_id, false),
    };

    let syntax_tree = match engine
        .query(&syntax_tree::Key { path: path.clone(), target_id })?
    {
        Ok(syntax_tree) => syntax_tree,
        Err(error) => return Ok(Err(Error(Arc::from(error.to_string())))),
    };

    let result =
        parse_node_from_module_content(syntax_tree.syntax_tree, &path, is_root);

    Ok(Ok(Arc::new(ChildFiles {
        files: Arc::from(result.0),
        recursive_request_error: result.1,
    })))
}

/// A query for mapping [`GlobalSourceID`] to the source file paths in a
/// particular target's file tree.
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
#[value(Result<Arc<ReadOnlyView<ID<SourceFile>, Arc<Path>>>, Error>)]
pub struct MapKey(pub TargetID);

fn scan_file_tree(
    engine: &TrackedEngine,
    source_file_paths_by_id: &DashMap<ID<SourceFile>, Arc<Path>>,
    key: &Key,
) -> Result<(), CyclicError> {
    let (file_path, target_id) = match key {
        Key::Root(target_id) => {
            let invocation_arguments =
                engine.get_invocation_arguments(*target_id);

            (
                Arc::from(invocation_arguments.command.input().file.clone()),
                *target_id,
            )
        }
        Key::File { path, target_id } => (path.clone(), *target_id),
    };

    assert!(source_file_paths_by_id
        .insert(
            engine.calculate_path_id(&file_path, target_id),
            file_path.clone()
        )
        .is_none());

    let Ok(child_files) = engine.query(key)? else {
        return Ok(());
    };

    child_files.files.par_iter().try_for_each(|child_file| {
        scan_file_tree(engine, source_file_paths_by_id, &Key::File {
            path: child_file.clone(),
            target_id,
        })?;

        Ok::<(), CyclicError>(())
    })?;

    Ok(())
}

#[pernixc_query::executor(key(MapKey), name(MapExecutor))]
#[allow(clippy::type_complexity)]
pub fn map_executor(
    &MapKey(target_id): &MapKey,
    engine: &TrackedEngine,
) -> Result<
    Result<Arc<ReadOnlyView<ID<SourceFile>, Arc<Path>>>, Error>,
    CyclicError,
> {
    let source_file_paths_by_id = DashMap::default();
    scan_file_tree(engine, &source_file_paths_by_id, &Key::Root(target_id))?;

    Ok(Ok(Arc::new(source_file_paths_by_id.into_read_only())))
}

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
pub struct PathKey(pub GlobalSourceID);

#[pernixc_query::executor(key(PathKey), name(PathExecutor))]
pub fn path_executor(
    &PathKey(global_source_id): &PathKey,
    engine: &TrackedEngine,
) -> Result<Arc<Path>, CyclicError> {
    let path_map = match engine.query(&MapKey(global_source_id.target_id))? {
        Ok(path_map) => path_map,
        Err(error) => {
            panic!(
                "Failed to query the file tree map for target ID {:?}: {}",
                global_source_id.target_id, error
            );
        }
    };

    Ok(path_map
        .get(&global_source_id.id)
        .unwrap_or_else(|| {
            panic!(
                "No path found for global source ID {global_source_id:?} in \
                 the file tree map"
            )
        })
        .clone())
}
