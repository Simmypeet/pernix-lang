//! Manages a module tree creation and syntactic information of the compilation
//! target.

use std::{
    collections::hash_map::Entry,
    hash::{Hash, Hasher},
    path::{Path, PathBuf},
    sync::Arc,
};

use enum_as_inner::EnumAsInner;
use flexstr::SharedStr;
use fnv::FnvHasher;
use parking_lot::RwLock;
use pernixc_arena::ID;
use pernixc_diagnostic::{Diagnostic, Related, Report, Severity};
use pernixc_handler::{Handler, Storage};
use pernixc_hash::HashMap;
use pernixc_lexical::tree::RelativeLocation;
use pernixc_query::{
    runtime::{
        executor,
        persistence::{serde::DynamicRegistry, Persistence},
    },
    TrackedEngine,
};
use pernixc_serialize::{
    de::Deserializer, ser::Serializer, Deserialize, Serialize,
};
use pernixc_source_file::{GlobalSourceID, LocalSourceID, SourceFile, Span};
use pernixc_stable_hash::StableHash;
use pernixc_syntax::Passable;
use pernixc_target::{get_invocation_arguments, TargetID};
use rayon::iter::{
    IntoParallelIterator as _, IntoParallelRefIterator as _,
    ParallelIterator as _,
};

use crate::{
    load_source_file::LoadSourceFileError, syntax_tree::ModuleContent,
};

pub mod errors;
pub mod load_source_file;
pub mod path;
pub mod source_map;
pub mod syntax_tree;
pub mod token_tree;

/// Describes the relationship between two symbols in the hierarchy.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HierarchyRelationship {
    /// The first symbol is the parent of the second symbol.
    Parent,

    /// The first symbol is the child of the second symbol.
    Child,
    /// Both symbols are two equivalent symbols.  Equivalent,

    /// Both symbols are defined in different hierarchy scope.
    Unrelated,
}

/// Registers all the required executors to run the queries.
pub fn register_executors(executor: &mut executor::Registry) {
    executor.register(Arc::new(load_source_file::Executor));
    executor.register(Arc::new(source_map::Executor));
    executor.register(Arc::new(path::Executor));
    executor.register(Arc::new(token_tree::ParseExecutor));
    executor.register(Arc::new(token_tree::KeyExecutor));
    executor.register(Arc::new(syntax_tree::Executor));
    executor.register(Arc::new(errors::Executor));
    executor.register(Arc::new(Executor));
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
    serde_registry.register::<path::Key>();
    serde_registry.register::<token_tree::Parse>();
    serde_registry.register::<token_tree::Key>();
    serde_registry.register::<syntax_tree::Key>();
    serde_registry.register::<errors::Key>();
    serde_registry.register::<Key>();
}

/// Registers the keys that should be skipped during serialization and
/// deserialization in the query engine's persistence layer
pub fn skip_persistence(persistence: &mut Persistence) {
    persistence.skip_cache_value::<load_source_file::Key>();
    persistence.skip_cache_value::<source_map::Key>();
    persistence.skip_cache_value::<syntax_tree::Key>();
    persistence.skip_cache_value::<token_tree::Parse>();
    persistence.skip_cache_value::<Key>();
}

/// An enumeration of all the errors that can occur during the module tree
/// parsing.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    Serialize,
    Deserialize,
    StableHash,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Error {
    RootSubmoduleConflict(RootSubmoduleConflict),
    SourceFileLoadFail(SourceFileLoadFail),
    ModuleRedefinition(ModuleRedefinition),
}

impl<T> Report<T> for Error {
    type Location = RelativeLocation;

    fn report(&self, context: T) -> Diagnostic<RelativeLocation> {
        match self {
            Self::RootSubmoduleConflict(error) => error.report(context),
            Self::SourceFileLoadFail(error) => error.report(context),
            Self::ModuleRedefinition(error) => error.report(context),
        }
    }
}

/// The submodule of the root source file ends up pointing to the root source
/// file itself.
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
pub struct RootSubmoduleConflict {
    /// The root source file.
    pub root: GlobalSourceID,

    /// The submodule that points to the root source file.
    pub submodule_span: Span<RelativeLocation>,

    /// The path to the source file where the redefinition occurred.
    pub load_path: PathBuf,
}

impl<T> Report<T> for RootSubmoduleConflict {
    type Location = RelativeLocation;

    fn report(&self, _: T) -> Diagnostic<RelativeLocation> {
        Diagnostic {
            severity: Severity::Error,
            message: "the submodule of the root source file ends up pointing \
                      to the root source file itself"
                .to_string(),
            span: Some((
                self.submodule_span,
                Some(format!(
                    "this ends up loading the file `{}`, which is the current \
                     module itself",
                    self.load_path.display()
                )),
            )),
            help_message: Some(
                "consider renaming the submodule name to avoid this conflict"
                    .to_string(),
            ),
            related: vec![],
        }
    }
}

/// Failed to load a source file for the submodule.
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
pub struct SourceFileLoadFail {
    /// The string representation of the source error.
    pub source_error: String,

    /// The submodule that submodule stems from.
    pub submodule: Span<RelativeLocation>,

    /// The failed loading path.
    pub path: PathBuf,
}

impl<T> Report<T> for SourceFileLoadFail {
    type Location = RelativeLocation;

    fn report(&self, _: T) -> Diagnostic<Self::Location> {
        Diagnostic {
            span: Some((self.submodule, None)),
            message: "failed to load the source file for the submodule"
                .to_string(),
            severity: Severity::Error,
            help_message: Some(format!(
                "failed to load the source file for the submodule at `{}`: {}",
                self.path.display(),
                self.source_error
            )),
            related: vec![],
        }
    }
}

/// A module with the given name already exists.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct ModuleRedefinition {
    /// The span of the existing module.
    pub existing_module_span: Option<Span<RelativeLocation>>,

    /// The submodule that redefines the module.
    pub redefinition_submodule_span: Option<Span<RelativeLocation>>,
}

impl<T> Report<T> for ModuleRedefinition {
    type Location = RelativeLocation;

    fn report(&self, _: T) -> Diagnostic<RelativeLocation> {
        Diagnostic {
            severity: Severity::Error,
            message: "a module with the given name already exists".to_string(),

            span: self.redefinition_submodule_span.map(|x| {
                (
                    x,
                    Some(
                        "this module redefines an existing module".to_string(),
                    ),
                )
            }),

            help_message: Some(
                "can not have more than one module with the same name in the \
                 same scope"
                    .to_string(),
            ),
            related: self
                .existing_module_span
                .map(|span| Related {
                    span,
                    message: "existing module with the same name".to_string(),
                })
                .into_iter()
                .collect(),
        }
    }
}

/// The tree structure of the compilation module.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ModuleTree {
    /// The signature of the module, if it exists.
    pub signature: Option<pernixc_syntax::item::module::Signature>,

    /// The access modifier of the module, if it exists.
    pub access_modifier: Option<pernixc_syntax::AccessModifier>,

    /// The content of the module, which contains the members and other
    pub content: Option<ModuleContent>,

    /// If the module was created from opening a source file, this is the ID of
    /// the source file.
    pub created_from_source_id: Option<ID<SourceFile>>,

    /// The submodules of the module, indexed by their names.
    pub submodules_by_name: HashMap<SharedStr, Self>,
}

impl StableHash for ModuleTree {
    fn stable_hash<H: pernixc_stable_hash::StableHasher + ?Sized>(
        &self,
        state: &mut H,
    ) {
        self.signature.stable_hash(state);
        self.access_modifier.stable_hash(state);
        self.content.stable_hash(state);

        self.submodules_by_name.len().stable_hash(state);

        let sub_module_hash = self
            .submodules_by_name
            .par_iter()
            .map(|(k, v)| {
                state.sub_hash(&mut |h| {
                    k.stable_hash(h);
                    v.stable_hash(h);
                })
            })
            .reduce(H::Hash::default, pernixc_stable_hash::Value::wrapping_add);

        sub_module_hash.stable_hash(state);
    }
}

impl Drop for ModuleTree {
    fn drop(&mut self) {
        std::mem::take(&mut self.submodules_by_name)
            .into_par_iter()
            .for_each(drop);
    }
}

/// The result of parsing an entire module tree for a compilation target.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, StableHash)]
pub struct Parse {
    /// The root module tree starting from the root file path.
    pub root_module_tree: Arc<ModuleTree>,

    /// The errors occurred during bracnching the module tree.
    ///
    /// This doesn't include the errors that occurred during parsing
    /// the source files.
    pub branching_errors: Arc<[Error]>,

    /// A map between the source file ID to its path. This allows retrival of
    /// the source file using a numeric ID.
    pub source_file_paths_by_id:
        Arc<HashMap<pernixc_arena::ID<SourceFile>, Arc<Path>>>,
}

/// Query for parsing a token tree from the given source file path.
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
#[value(Result<Parse, LoadSourceFileError>)]
#[extend(method(get_module_tree), no_cyclic)]
pub struct Key(pub TargetID);

/// An executor for parsing a module tree for a compilation target.
#[derive(Debug, Clone, Copy, Default)]
pub struct Executor;

fn add_source_file(
    path: Arc<Path>,
    source_files_by_id: &mut HashMap<pernixc_arena::ID<SourceFile>, Arc<Path>>,
) -> LocalSourceID {
    let mut hasher = FnvHasher::default();
    path.hash(&mut hasher);

    let finalize_hash = |hasher: FnvHasher| {
        let mut attempt = 0;
        loop {
            // for some reason, FnvHasher doesn't implement `Clone` trait.
            // this is the work around to clone
            let mut final_hasher = FnvHasher::with_key(hasher.finish());
            attempt.hash(&mut final_hasher);

            let candidate_branch_id =
                pernixc_arena::ID::new(final_hasher.finish());

            // avoid hash collision
            if !source_files_by_id.contains_key(&candidate_branch_id) {
                return candidate_branch_id;
            }

            attempt += 1;
        }
    };

    let hash = finalize_hash(hasher);

    // insert the source file into the map
    assert!(source_files_by_id.insert(hash, path).is_none());

    hash
}

fn parse_module_tree(
    tracked_engine: &TrackedEngine,
    source_files_by_id: &RwLock<
        HashMap<pernixc_arena::ID<SourceFile>, Arc<Path>>,
    >,
    path: Arc<Path>,
    target_id: TargetID,
) -> Result<(crate::syntax_tree::SyntaxTree, ID<SourceFile>), LoadSourceFileError>
{
    tracked_engine
        .query(&crate::load_source_file::Key { path: path.clone(), target_id })
        .expect("should have no cyclic dependencies")?;

    let id = add_source_file(path.clone(), &mut source_files_by_id.write());

    let module_tree = tracked_engine
        .query(&crate::syntax_tree::Key {
            path,
            target_id,
            global_source_id: TargetID::Local.make_global(id),
        })
        .expect("should have no cyclic dependencies")
        .expect("should be able to load the source file");

    Ok((module_tree, id))
}

struct Context<'a> {
    root_directory: &'a Path,
    root_file_path: &'a Path,
    root_file_source_id: ID<SourceFile>,
    handler: &'a dyn Handler<Error>,
    source_files_by_id:
        &'a RwLock<HashMap<pernixc_arena::ID<SourceFile>, Arc<Path>>>,
    tracked_engine: &'a TrackedEngine<'a>,
    target_id: TargetID,
}

impl Context<'_> {
    #[allow(clippy::too_many_lines)]
    fn branch_module_tree(
        &self,
        created_from_source_id: Option<ID<SourceFile>>,
        content: Option<ModuleContent>,
        signature: Option<pernixc_syntax::item::module::Signature>,
        access_modifier: Option<pernixc_syntax::AccessModifier>,
        current_name_space: &[SharedStr],
    ) -> ModuleTree {
        let Some(content) = content else {
            return ModuleTree {
                signature,
                access_modifier,
                created_from_source_id,
                content,
                submodules_by_name: HashMap::default(),
            };
        };

        // generate a parallel processes to parse files
        let submodules_by_name = std::thread::scope(|scope| {
            let mut join_handles = Vec::new();

            for member in content.members() {
                let Passable::Line(
                    pernixc_syntax::item::module::Member::Module(submodule),
                ) = member
                else {
                    continue;
                };

                let next_signature = submodule.signature();
                let Some(next_identifier) = next_signature.as_ref().and_then(
                    pernixc_syntax::item::module::Signature::identifier,
                ) else {
                    continue;
                };

                let next_name = next_identifier.kind.0.clone();
                let next_access_modifier = submodule.access_modifier();
                let next_name_space = current_name_space
                    .iter()
                    .cloned()
                    .chain(std::iter::once(next_name.clone()))
                    .collect::<Vec<_>>();

                join_handles.push(scope.spawn(move || {
                    // Found inline module content, take the content right away
                    if let Some(member) = submodule.inline_body() {
                        let next_submodule = self.branch_module_tree(
                            None,
                            member.content().map(ModuleContent),
                            next_signature,
                            next_access_modifier,
                            &next_name_space,
                        );

                        Some((next_name, next_submodule))
                    }
                    // Otherwise, open a file and parse it
                    else {
                        let mut path = self.root_directory.to_path_buf();

                        path.extend(
                            next_name_space
                                .iter()
                                .skip(1)
                                .map(SharedStr::as_str),
                        );
                        path.set_extension("pnx");

                        if path == self.root_file_path {
                            // pointing to the root file itself
                            self.handler.receive(Error::RootSubmoduleConflict(
                                RootSubmoduleConflict {
                                    root: TargetID::Local
                                        .make_global(self.root_file_source_id),
                                    submodule_span: next_identifier.span,
                                    load_path: path.clone(),
                                },
                            ));
                            return None;
                        }

                        let (content, source_id) = match parse_module_tree(
                            self.tracked_engine,
                            self.source_files_by_id,
                            Arc::from(path.clone()),
                            self.target_id,
                        ) {
                            Ok(content) => content,
                            Err(error) => {
                                self.handler.receive(
                                    Error::SourceFileLoadFail(
                                        SourceFileLoadFail {
                                            source_error: error.0.to_string(),
                                            submodule: next_identifier.span,
                                            path,
                                        },
                                    ),
                                );
                                return None;
                            }
                        };

                        let next_submodule = self.branch_module_tree(
                            Some(source_id),
                            content.syntax_tree,
                            next_signature,
                            next_access_modifier,
                            &next_name_space,
                        );

                        Some((next_name, next_submodule))
                    }
                }));
            }

            let mut submodules_by_name =
                HashMap::<SharedStr, ModuleTree>::default();

            for (name, module_tree) in join_handles
                .into_iter()
                .filter_map(|x| x.join().unwrap())
                .map(|x| (x.0, x.1))
            {
                match submodules_by_name.entry(name) {
                    Entry::Occupied(occupied_entry) => {
                        self.handler.receive(Error::ModuleRedefinition(
                            ModuleRedefinition {
                                existing_module_span: occupied_entry
                                    .get()
                                    .signature
                                    .as_ref()
                                    .map(|x| x.inner_tree().span()),
                                redefinition_submodule_span: module_tree
                                    .signature
                                    .as_ref()
                                    .map(|x| x.inner_tree().span()),
                            },
                        ));
                    }
                    Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(module_tree);
                    }
                }
            }

            submodules_by_name
        });

        ModuleTree {
            signature,
            access_modifier,
            content: Some(content),
            submodules_by_name,
            created_from_source_id,
        }
    }
}

fn start(
    tracked_engine: &TrackedEngine,
    root_file_path: &Arc<Path>,
    target_name: SharedStr,
    target_id: TargetID,
) -> Result<Parse, LoadSourceFileError> {
    let source_files_by_id = RwLock::new(HashMap::default());

    let (module_content, root_file_source_id) = parse_module_tree(
        tracked_engine,
        &source_files_by_id,
        root_file_path.clone(),
        target_id,
    )?;

    let storage = Storage::<Error>::default();

    let context = Context {
        root_directory: &Arc::from(
            root_file_path.parent().unwrap_or(Path::new("")),
        ),
        root_file_path,
        root_file_source_id,
        handler: &storage,
        source_files_by_id: &source_files_by_id,
        tracked_engine,
        target_id,
    };

    let root_module_tree = context.branch_module_tree(
        Some(root_file_source_id),
        module_content.syntax_tree,
        None,
        None,
        &[target_name],
    );

    Ok(Parse {
        root_module_tree: Arc::new(root_module_tree),
        branching_errors: Arc::from(storage.into_vec()),
        source_file_paths_by_id: Arc::new(source_files_by_id.into_inner()),
    })
}

impl pernixc_query::runtime::executor::Executor<Key> for Executor {
    fn execute(
        &self,
        engine: &pernixc_query::TrackedEngine,
        key: &Key,
    ) -> Result<
        Result<Parse, LoadSourceFileError>,
        pernixc_query::runtime::executor::CyclicError,
    > {
        let arguments = engine.get_invocation_arguments(key.0);

        let arguments_input = arguments.command.input();

        let root_path: Arc<Path> = Arc::from(arguments_input.file.clone());

        let target_name = arguments_input.target_name.as_ref().map_or_else(
            || {
                root_path.file_stem().map_or_else(
                    || SharedStr::from_static("placeholder"),
                    |x| x.to_string_lossy().as_ref().into(),
                )
            },
            SharedStr::from,
        );

        match start(engine, &root_path, target_name, key.0) {
            Ok(result) => Ok(Ok(result)),
            Err(error) => Ok(Err(error)),
        }
    }
}
