//! Module syntax query system for the Pernix compiler.

use std::{
    collections::hash_map::Entry,
    path::{Path, PathBuf},
};

use enum_as_inner::EnumAsInner;
use flexstr::SharedStr;
use parking_lot::RwLock;
use pernixc_diagnostic::{Diagnostic, Related, Report, Severity};
use pernixc_handler::Storage;
use pernixc_hash::{DashMap, HashMap};
use pernixc_lexical::tree::RelativeLocation;
use pernixc_parser::abstract_tree::AbstractTree;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::{GlobalSourceID, SourceFile, SourceMap, Span};
use pernixc_syntax::Passable;
use pernixc_target::TargetID;

/// An enumeration of all the errors that can occur during the module tree
/// parsing.
#[derive(Debug, EnumAsInner, derive_more::From)]
#[allow(missing_docs)]
pub enum Error {
    Lexical(pernixc_lexical::error::Error),
    Syntax(pernixc_parser::error::Error),
    RootSubmoduleConflict(RootSubmoduleConflict),
    SourceFileLoadFail(SourceFileLoadFail),
    ModuleRedefinition(ModuleRedefinition),
}

/// The submodule of the root source file ends up pointing to the root source
/// file itself.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
#[derive(Debug)]
pub struct SourceFileLoadFail {
    /// The string representation of the source error.
    pub source_error: pernixc_source_file::Error,

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
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct Tree {
    /// The signature of the module, if it exists.
    pub signature: Option<pernixc_syntax::item::module::Signature>,

    /// The access modifier of the module, if it exists.
    pub access_modifier: Option<pernixc_syntax::AccessModifier>,

    /// The content of the module, which contains the members and other
    pub content: pernixc_syntax::item::module::Content,

    /// The submodules of the module, indexed by their names.
    pub submodules_by_name: HashMap<SharedStr, Self>,
}

#[derive(Debug, Clone)]
enum Input {
    Inline {
        module_content: pernixc_syntax::item::module::Content,
        module_name: SharedStr,
        signature: pernixc_syntax::item::module::Signature,
        access_modifier: Option<pernixc_syntax::AccessModifier>,
        in_source_id: GlobalSourceID,
    },
    File {
        source_id: GlobalSourceID,
        access_modifier: Option<pernixc_syntax::AccessModifier>,
        signature: Option<pernixc_syntax::item::module::Signature>,
        root: bool,
    },
}

/// A supertrait for all the handlers that are required to parse the module
/// tree syntax.
pub trait Handler:
    pernixc_handler::Handler<pernixc_parser::error::Error>
    + pernixc_handler::Handler<pernixc_lexical::error::Error>
    + pernixc_handler::Handler<SourceFileLoadFail>
    + pernixc_handler::Handler<RootSubmoduleConflict>
    + pernixc_handler::Handler<ModuleRedefinition>
{
}

impl<
        T: pernixc_handler::Handler<pernixc_parser::error::Error>
            + pernixc_handler::Handler<pernixc_lexical::error::Error>
            + pernixc_handler::Handler<SourceFileLoadFail>
            + pernixc_handler::Handler<RootSubmoduleConflict>
            + pernixc_handler::Handler<ModuleRedefinition>,
    > Handler for T
{
}

impl Tree {
    #[allow(clippy::too_many_arguments, clippy::needless_pass_by_value)]
    fn parse_submodule(
        source_map: &pernixc_source_file::SourceMap,
        submodule_current_directory: &Path,
        submodule: pernixc_syntax::item::module::Module,
        current_module_name: &str,
        is_root: bool,
        token_trees_by_source_id: &DashMap<
            GlobalSourceID,
            pernixc_lexical::tree::Tree,
        >,
        in_source_id: GlobalSourceID,
        handler: &dyn Handler,
    ) -> Option<(SharedStr, Self)> {
        let signature = submodule.signature()?;

        let ident_span_rel = signature.identifier()?.span;
        let ident_span_abs = {
            let token_tree =
                token_trees_by_source_id.get(&in_source_id).unwrap();
            token_tree.absolute_span_of(&ident_span_rel)
        };

        // get the current module name from the source map
        let submodule_name: SharedStr =
            source_map.get(ident_span_abs.source_id).unwrap().content()
                [ident_span_abs.range()]
            .into();

        let module_tree = if let Some(inline_body) = submodule.inline_body() {
            Self::parse_input(
                Input::Inline {
                    module_content: inline_body.content()?,
                    module_name: submodule_name.clone(),
                    signature,
                    in_source_id,
                    access_modifier: submodule.access_modifier(),
                },
                submodule_current_directory,
                source_map,
                token_trees_by_source_id,
                handler,
            )
        } else {
            // ended up pointing to the same file
            if is_root && current_module_name == submodule_name.as_str() {
                handler.receive(RootSubmoduleConflict {
                    root: ident_span_abs.source_id,
                    submodule_span: ident_span_rel,
                    load_path: submodule_current_directory
                        .join(submodule_name.as_str())
                        .with_extension("pnx"),
                });
                return None;
            }

            let mut source_file_path =
                submodule_current_directory.join(submodule_name.as_str());
            source_file_path.set_extension("pnx");

            // load the source file
            let source_id = match std::fs::File::open(&source_file_path)
                .map_err(Into::into)
                .and_then(|file| {
                    SourceFile::load(file, source_file_path.clone())
                }) {
                Ok(source_file) => TargetID::Local.make_global(
                    source_map.register(TargetID::Local, source_file),
                ),
                Err(source_error) => {
                    handler.receive(SourceFileLoadFail {
                        source_error,
                        submodule: ident_span_rel,
                        path: source_file_path,
                    });
                    return None;
                }
            };

            let submodule_current_directory = &submodule_current_directory;

            Self::parse_input(
                Input::File {
                    source_id,
                    access_modifier: submodule.access_modifier(),
                    signature: Some(signature),
                    root: false,
                },
                submodule_current_directory,
                source_map,
                token_trees_by_source_id,
                handler,
            )
        };

        Some((submodule_name, module_tree))
    }

    #[allow(clippy::too_many_lines)]
    fn parse_input(
        input: Input,
        current_directory: &Path,
        source_map: &SourceMap,
        token_trees_by_source_id: &DashMap<
            GlobalSourceID,
            pernixc_lexical::tree::Tree,
        >,
        handler: &dyn Handler,
    ) -> Self {
        let (
            content,
            source_file,
            current_module_name,
            is_root,
            signature,
            access_modifier,
        ) = match input {
            Input::Inline {
                module_content,
                module_name,
                in_source_id,
                signature,
                access_modifier,
            } => (
                module_content,
                in_source_id,
                module_name,
                false,
                Some(signature),
                access_modifier,
            ),

            Input::File { source_id, root, access_modifier, signature } => {
                let token_tree = pernixc_lexical::tree::Tree::from_source(
                    source_map.get(source_id).unwrap().content(),
                    source_id,
                    handler,
                );

                let (module_content, errors) =
                    pernixc_syntax::item::module::Content::parse(&token_tree);

                for error in errors {
                    handler.receive(error);
                }

                token_trees_by_source_id.insert(source_id, token_tree);

                (
                    module_content.unwrap_or_else(
                        pernixc_syntax::item::module::Content::default,
                    ),
                    source_id,
                    source_map
                        .get(source_id)
                        .unwrap()
                        .full_path()
                        .file_stem()
                        .unwrap()
                        .to_str()
                        .unwrap()
                        .into(),
                    root,
                    signature,
                    access_modifier,
                )
            }
        };

        let submodule_current_directory = if is_root {
            current_directory.to_path_buf()
        } else {
            current_directory.join(current_module_name.as_str())
        };

        let parsing_result = RwLock::new(Vec::new());

        let parsing_result_ref = &parsing_result;
        let submodule_current_directory_ref = &submodule_current_directory;
        let current_module_name_ref = &current_module_name;

        rayon::scope(|scope| {
            for (index, member) in content.members().enumerate() {
                let Passable::Line(
                    pernixc_syntax::item::module::Member::Module(submodule),
                ) = member
                else {
                    continue;
                };

                scope.spawn(move |_| {
                    let result = Self::parse_submodule(
                        source_map,
                        submodule_current_directory_ref,
                        submodule,
                        current_module_name_ref,
                        is_root,
                        token_trees_by_source_id,
                        source_file,
                        handler,
                    );

                    parsing_result_ref.write().push((index, result));
                });
            }
        });

        let mut submodules_by_name = HashMap::<SharedStr, Self>::default();
        let mut parsing_result = parsing_result.into_inner();
        parsing_result.sort_by_key(|x| x.0);

        for (name, module_tree) in
            parsing_result.into_iter().filter_map(|x| x.1)
        {
            match submodules_by_name.entry(name) {
                Entry::Occupied(occupied_entry) => {
                    handler.receive(ModuleRedefinition {
                        existing_module_span: occupied_entry
                            .get()
                            .signature
                            .as_ref()
                            .map(|x| x.inner_tree().span()),
                        redefinition_submodule_span: module_tree
                            .signature
                            .map(|x| x.inner_tree().span()),
                    });
                }
                Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(module_tree);
                }
            }
        }

        Self { signature, access_modifier, content, submodules_by_name }
    }
}

/// Parses the whole module tree for the target program from the given root
/// source file.
#[must_use]
pub fn parse(
    source_id: GlobalSourceID,
    source_map: &SourceMap,
    token_trees_by_source_id: &DashMap<
        GlobalSourceID,
        pernixc_lexical::tree::Tree,
    >,
) -> (Tree, Vec<Error>) {
    let storage = Storage::<Error>::default();

    let path = source_map
        .get(source_id)
        .unwrap()
        .full_path()
        .parent()
        .unwrap_or_else(|| Path::new(""))
        .to_path_buf();

    (
        Tree::parse_input(
            Input::File {
                source_id,
                access_modifier: None,
                root: true,
                signature: None,
            },
            &path,
            source_map,
            token_trees_by_source_id,
            &storage,
        ),
        storage.into_vec(),
    )
}
