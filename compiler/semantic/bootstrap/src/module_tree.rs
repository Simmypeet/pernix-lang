//! Module syntax query system for the Pernix compiler.

use std::{
    collections::hash_map::Entry,
    hash::{Hash as _, Hasher as _},
    path::{Path, PathBuf},
    sync::Arc,
};

use enum_as_inner::EnumAsInner;
use flexstr::{FlexStr, SharedStr};
use fnv::FnvHasher;
use parking_lot::RwLock;
use pernixc_diagnostic::{Diagnostic, Related, Report, Severity};
use pernixc_handler::{Handler, Storage};
use pernixc_hash::HashMap;
use pernixc_lexical::tree::RelativeLocation;
use pernixc_query::TrackedEngine;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::{GlobalSourceID, LocalSourceID, SourceFile, Span};
use pernixc_stable_hash::StableHash;
use pernixc_syntax::Passable;
use pernixc_target::TargetID;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

use crate::{
    arguments::get_invocation_arguments, source_file::LoadSourceFileError,
};

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

impl Report<()> for RootSubmoduleConflict {
    type Location = RelativeLocation;

    fn report(&self, (): ()) -> Diagnostic<RelativeLocation> {
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

impl Report<()> for SourceFileLoadFail {
    type Location = RelativeLocation;

    fn report(&self, (): ()) -> Diagnostic<Self::Location> {
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

impl Report<()> for ModuleRedefinition {
    type Location = RelativeLocation;

    fn report(&self, (): ()) -> Diagnostic<RelativeLocation> {
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
    pub content: Option<pernixc_syntax::item::module::Content>,

    /// The submodules of the module, indexed by their names.
    pub submodules_by_name: HashMap<SharedStr, Self>,
}

impl StableHash for ModuleTree {
    fn stable_hash<H: pernixc_stable_hash::StableHasher + ?Sized>(
        &self,
        state: &mut H,
    ) {
        let inner_tree = self.content.inner_tree();

        self.signature.stable_hash(state);
        self.access_modifier.stable_hash(state);
        inner_tree.ast_info.stable_hash(state);

        let (tree_hash, tree_count) = inner_tree
            .nodes
            .par_iter()
            .map(|x| {
                let sub_hash = state.sub_hash(&mut |h| {
                    x.stable_hash(h);
                });

                (sub_hash, 1)
            })
            .reduce(
                || (H::Hash::default(), 0),
                |(l_hash, l_count), (r_hash, r_count)| {
                    (l_hash.wrapping_add(r_hash), r_count + l_count)
                },
            );

        tree_hash.stable_hash(state);
        tree_count.stable_hash(state);

        self.submodules_by_name.len().stable_hash(state);

        let module_hash = self
            .submodules_by_name
            .par_iter()
            .map(|(k, v)| {
                state.sub_hash(&mut |h| {
                    k.stable_hash(h);
                    v.stable_hash(h);
                })
            })
            .reduce(H::Hash::default, pernixc_stable_hash::Value::wrapping_add);

        module_hash.stable_hash(state);
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
) -> Result<(crate::syntax_tree::SyntaxTree, GlobalSourceID), LoadSourceFileError>
{
    tracked_engine
        .query(&crate::source_file::Key { path: path.clone(), target_id })
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

    Ok((module_tree, TargetID::Local.make_global(id)))
}

struct Context<'a> {
    root_directory: &'a Path,
    root_file_path: &'a Path,
    root_file_source_id: GlobalSourceID,
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
        content: Option<pernixc_syntax::item::module::Content>,
        signature: Option<pernixc_syntax::item::module::Signature>,
        access_modifier: Option<pernixc_syntax::AccessModifier>,
        current_name_space: &[SharedStr],
    ) -> ModuleTree {
        let Some(content) = content else {
            return ModuleTree {
                signature,
                access_modifier,
                content,
                submodules_by_name: HashMap::default(),
            };
        };

        let next_submodules = RwLock::new(Vec::new());

        // generate a parallel processes to parse files
        rayon::scope(|scope| {
            let next_submodules = &next_submodules;

            for (index, member) in content.members().enumerate() {
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

                scope.spawn(move |_| {
                    // Found inline module content, take the content right away
                    if let Some(member) = submodule.inline_body() {
                        let next_submodule = self.branch_module_tree(
                            member.content(),
                            next_signature,
                            next_access_modifier,
                            &next_name_space,
                        );

                        next_submodules.write().push((
                            index,
                            next_name,
                            next_submodule,
                        ));
                    }
                    // Otherwise, open a file and parse it
                    else {
                        let mut path = self.root_directory.to_path_buf();

                        path.extend(
                            next_name_space.iter().skip(1).map(FlexStr::as_str),
                        );
                        path.set_extension("pnx");

                        if path == self.root_file_path {
                            // pointing to the root file itself
                            self.handler.receive(Error::RootSubmoduleConflict(
                                RootSubmoduleConflict {
                                    root: self.root_file_source_id,
                                    submodule_span: next_identifier.span,
                                    load_path: path.clone(),
                                },
                            ));
                            return;
                        }

                        let (content, _) = match parse_module_tree(
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
                                return;
                            }
                        };

                        let next_submodule = self.branch_module_tree(
                            content.syntax_tree,
                            next_signature,
                            next_access_modifier,
                            &next_name_space,
                        );

                        next_submodules.write().push((
                            index,
                            next_name,
                            next_submodule,
                        ));
                    }
                });
            }
        });

        let mut next_submodules = next_submodules.into_inner();
        next_submodules.sort_by_key(|x| x.0);

        let mut submodules_by_name =
            HashMap::<SharedStr, ModuleTree>::default();

        for (name, module_tree) in
            next_submodules.into_iter().map(|x| (x.1, x.2))
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
                                .map(|x| x.inner_tree().span()),
                        },
                    ));
                }
                Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(module_tree);
                }
            }
        }

        ModuleTree {
            signature,
            access_modifier,
            content: Some(content),
            submodules_by_name,
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

    let root_module_tree =
        context.branch_module_tree(module_content.syntax_tree, None, None, &[
            target_name,
        ]);

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
