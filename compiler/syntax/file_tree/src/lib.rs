//! Manages a module tree creation and syntactic information of the compilation
//! target.

use std::{ collections::hash_map::Entry,
    hash::{BuildHasher, BuildHasherDefault},
    path::{Path, PathBuf},
    sync::Arc,
};

use flexstr::SharedStr;
use pernixc_arena::ID;
use pernixc_extend::extend;
use pernixc_hash::HashMap;
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
use pernixc_source_file::{GlobalSourceID, SourceFile};
use pernixc_stable_hash::StableHash;
use pernixc_syntax::Passable;
use pernixc_target::{get_invocation_arguments, TargetID};

use crate::{
    load_source_file::LoadSourceFileError, syntax_tree::ModuleContent,
};

pub mod errors;
pub mod file_tree;
pub mod load_source_file;
pub mod source_map;
pub mod syntax_tree;
pub mod token_tree;

/// Registers all the required executors to run the queries.
pub fn register_executors(executor: &mut executor::Registry) {
    executor.register(Arc::new(load_source_file::Executor));
    executor.register(Arc::new(source_map::Executor));

    executor.register(Arc::new(token_tree::ParseExecutor));
    executor.register(Arc::new(token_tree::KeyExecutor));

    executor.register(Arc::new(syntax_tree::Executor));

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
    serde_registry.register::<load_source_file::Key>();

    serde_registry.register::<source_map::Key>();

    serde_registry.register::<token_tree::Parse>();
    serde_registry.register::<token_tree::Key>();

    serde_registry.register::<syntax_tree::Key>();

    serde_registry.register::<errors::Key>();
    serde_registry.register::<errors::RenderedKey>();

    serde_registry.register::<Key>();
    serde_registry.register::<MapKey>();
    serde_registry.register::<PathKey>();
}

/// Registers the keys that should be skipped during serialization and
/// deserialization in the query engine's persistence layer
pub fn skip_persistence(persistence: &mut Persistence) {
    persistence.skip_cache_value::<load_source_file::Key>();
    persistence.skip_cache_value::<source_map::Key>();
}

/// Enumeration of either a submodule defined inline in the current
/// module file, or a submodule that is defined in a separate source file.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, StableHash)]
pub enum Submodule {
    /// A submodule that is defined inline in the current module file.
    Node(Node),

    /// A submodule is defined in a separate source file.
    SourceFile {
        /// The path to the source that it must load
        path: Arc<Path>,

        /// The target that requested the source file loading
        target_id: TargetID,
    },
}

/// Represents a node in the module tree.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, StableHash)]
pub struct Node {
    /// A map of submodules defined in the current node.
    pub submodules_by_name: Arc<HashMap<SharedStr, Submodule>>,
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
#[value(Result<Node, LoadSourceFileError>)]
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

#[extend]
fn calculate_path_id(self: &TrackedEngine<'_>, path: &Path) -> ID<SourceFile> {
    ID::new(
        BuildHasherDefault::<siphasher::sip::SipHasher24>::default()
            .hash_one((self.random_seed(), path)),
    )
}

#[allow(dead_code)]
fn parse_node_from_module_content(
    content: Option<ModuleContent>,
    current_path: &Path,
    is_root: bool,
    target_id: TargetID,
) -> Node {
    // empty file/module
    let Some(content) = content else {
        return Node { submodules_by_name: Arc::new(HashMap::default()) };
    };

    let mut sub_modules_by_name = HashMap::<SharedStr, Submodule>::default();

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

        let module_content = match content {
            Some(module) => Submodule::Node(parse_node_from_module_content(
                module.content().map(ModuleContent),
                &next_path,
                false,
                target_id,
            )),
            None => {
                Submodule::SourceFile { path: Arc::from(next_path), target_id }
            }
        };

        if let Entry::Vacant(entry) =
            sub_modules_by_name.entry(next_identifier.kind.0.clone())
        {
            entry.insert(module_content);
        }
    }

    Node { submodules_by_name: Arc::new(sub_modules_by_name) }
}

#[pernixc_query::executor(key(Key), name(Executor))]
pub fn node_executor(
    key: &Key,
    engine: &TrackedEngine,
) -> Result<Result<Node, LoadSourceFileError>, CyclicError> {
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

    // NOTE: the source file ID is derived from the path hash, so it is
    // stable across the compilation run (same session).
    //
    // There's slight chance of hash collision though
    let source_id = engine.calculate_path_id(&path);

    let syntax_tree = match engine.query(&syntax_tree::Key {
        path: path.clone(),
        target_id,
        global_source_id: target_id.make_global(source_id),
    })? {
        Ok(syntax_tree) => syntax_tree,
        Err(error) => {
            return Ok(Err(LoadSourceFileError(Arc::from(error.to_string()))))
        }
    };

    Ok(Ok(parse_node_from_module_content(
        syntax_tree.syntax_tree,
        &path,
        is_root,
        target_id,
    )))
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
#[value(Result<Arc<HashMap<ID<SourceFile>, Arc<Path>>>, LoadSourceFileError>)]
pub struct MapKey(pub TargetID);

fn scan_file_tree(
    engine: &TrackedEngine,
    source_file_paths_by_id: &mut HashMap<ID<SourceFile>, Arc<Path>>,
    node: &Node,
) -> Result<(), CyclicError> {
    for (_, submodule) in node.submodules_by_name.iter() {
        // add the submodule name to the source file paths map
        if let Submodule::SourceFile { path, .. } = submodule {
            source_file_paths_by_id
                .insert(engine.calculate_path_id(path), path.clone());
        }

        // recursively scan the submodule node
        match submodule {
            Submodule::Node(node) => {
                scan_file_tree(engine, source_file_paths_by_id, node)?;
            }
            Submodule::SourceFile { path, target_id } => {
                let Ok(node) = engine.query(&Key::File {
                    path: path.clone(),
                    target_id: *target_id,
                })?
                else {
                    continue;
                };

                scan_file_tree(engine, source_file_paths_by_id, &node)?;
            }
        }
    }

    Ok(())
}

#[pernixc_query::executor(key(MapKey), name(MapExecutor))]
#[allow(clippy::type_complexity)]
pub fn map_executor(
    &MapKey(target_id): &MapKey,
    engine: &TrackedEngine,
) -> Result<
    Result<Arc<HashMap<ID<SourceFile>, Arc<Path>>>, LoadSourceFileError>,
    CyclicError,
> {
    let module_tree = match engine.query(&Key::Root(target_id))? {
        Ok(module_tree) => module_tree,
        Err(result) => return Ok(Err(result)),
    };

    let mut source_file_paths_by_id = HashMap::default();
    scan_file_tree(engine, &mut source_file_paths_by_id, &module_tree)?;

    Ok(Ok(Arc::new(source_file_paths_by_id)))
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
