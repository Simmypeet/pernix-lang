//! Module syntax query system for the Pernix compiler.

use std::{
    collections::{hash_map::Entry, HashMap},
    path::{Path, PathBuf},
};

use dashmap::DashMap;
use enum_as_inner::EnumAsInner;
use pernixc_diagnostic::{Diagnostic, Related, Report, Severity};
use pernixc_lexical::tree::RelativeLocation;
use pernixc_parser::abstract_tree::AbstractTree;
use pernixc_source_file::{GlobalSourceID, SourceFile, SourceMap, Span};
use pernixc_syntax::Passable;
use pernixc_target::TargetID;
use rayon::iter::{ParallelBridge, ParallelIterator};

/// An enumeration of all the errors that can occur during the module tree
/// parsing.
#[derive(Debug, EnumAsInner)]
pub enum Error {
    Lexical(pernixc_lexical::error::Error),
    Syntax(pernixc_parser::error::Error),
    RootSubmoduleConflict(RootSubmoduleConflict),
    SourceFileLoadFail(SourceFileLoadFail),
    ModuleRedefinition(ModuleRedefinition),
}

/// The submodule of the root source file ends up pointing to the root source
/// file itself.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RootSubmoduleConflict {
    /// The root source file.
    pub root: GlobalSourceID,

    /// The submodule that points to the root source file.
    pub submodule_span: Span<RelativeLocation>,
}

impl Report<()> for RootSubmoduleConflict {
    type Location = RelativeLocation;

    fn report(&self, (): ()) -> Diagnostic<RelativeLocation> {
        Diagnostic {
            severity: Severity::Error,
            message: "the submodule of the root source file ends up pointing \
                      to the root source file itself"
                .to_string(),
            span: Some((self.submodule_span.clone(), None)),
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
    /// The error that occurred while loading the source file.
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
            span: Some((self.submodule.clone(), None)),
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

#[derive(Debug, Clone)]
pub struct Parse {
    pub tree: Tree,
    pub token_trees_by_source_id: pernixc_lexical::tree::Tree,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tree {
    pub signature: Option<pernixc_syntax::item::module::Signature>,
    pub access_modifier: Option<pernixc_syntax::AccessModifier>,
    pub content: pernixc_syntax::item::module::Content,
    pub submodules_by_name: HashMap<String, Self>,
}

#[derive(Debug, Clone)]
enum Input {
    Inline {
        module_content: pernixc_syntax::item::module::Content,
        module_name: String,
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
    #[allow(clippy::too_many_arguments)]
    fn parse_submodule(
        source_map: &pernixc_source_file::SourceMap,
        submodule_current_directory: &Path,
        member: Passable<pernixc_syntax::item::module::Member>,
        current_module_name: &str,
        is_root: bool,
        token_trees_by_source_id: &DashMap<
            GlobalSourceID,
            pernixc_lexical::tree::Tree,
        >,
        in_source_id: GlobalSourceID,
        handler: &dyn Handler,
    ) -> Option<(String, Self)> {
        let line = member.into_line().ok()?;
        let submodule = line.into_module().ok()?;
        let signature = submodule.signature()?;

        let ident_span_rel = signature.identifier()?.span;
        let ident_span_abs = {
            let token_tree =
                token_trees_by_source_id.get(&in_source_id).unwrap();
            token_tree.absolute_span_of(&ident_span_rel)
        };

        // get the current module name from the source map
        let submodule_name =
            source_map.get(ident_span_abs.source_id).unwrap().content()
                [ident_span_abs.range()]
            .to_owned();

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
            if is_root && current_module_name == submodule_name {
                handler.receive(RootSubmoduleConflict {
                    root: ident_span_abs.source_id,
                    submodule_span: ident_span_rel,
                });
            }

            let mut source_file_path =
                submodule_current_directory.join(&submodule_name);
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
                        .to_string(),
                    root,
                    signature,
                    access_modifier,
                )
            }
        };

        let submodule_current_directory = if is_root {
            current_directory.to_path_buf()
        } else {
            current_directory.join(&current_module_name)
        };

        let module_trees_by_name = content
            .members()
            .par_bridge()
            .filter_map(|item| {
                Self::parse_submodule(
                    source_map,
                    &submodule_current_directory,
                    item,
                    &current_module_name,
                    is_root,
                    token_trees_by_source_id,
                    source_file,
                    handler,
                )
            })
            .collect::<Vec<_>>();

        let mut submodules_by_name = HashMap::<String, Self>::default();

        for (name, module_tree) in module_trees_by_name {
            match submodules_by_name.entry(name) {
                Entry::Occupied(occupied_entry) => {
                    handler.receive(ModuleRedefinition {
                        existing_module_span: module_tree
                            .signature
                            .and_then(|x| x.inner_tree().span()),
                        redefinition_submodule_span: occupied_entry
                            .get()
                            .signature
                            .as_ref()
                            .and_then(|x| x.inner_tree().span()),
                    });
                }
                Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(module_tree);
                }
            }
        }

        Self { signature, access_modifier, content, submodules_by_name }
    }

    /// Parses the whole module tree for the target program from the given root
    /// source file.
    pub fn parse(
        source_id: GlobalSourceID,
        source_map: &SourceMap,
        handler: &dyn Handler,
    ) -> Self {
        let token_trees_by_source_id = DashMap::new();

        Self::parse_input(
            Input::File {
                source_id,
                access_modifier: None,
                root: true,
                signature: None,
            },
            source_map
                .get(source_id)
                .unwrap()
                .full_path()
                .parent()
                .unwrap_or_else(|| Path::new("")),
            source_map,
            &token_trees_by_source_id,
            handler,
        )
    }
}

/// Parses the syntax of a module from its source ID and source map,
/// returning the parsed syntax and the associated token tree.
#[must_use]
pub fn from_source_id(
    source_id: GlobalSourceID,
    source_map: &pernixc_source_file::SourceMap,
) -> (Parse, pernixc_lexical::tree::Tree) {
    todo!()
}
