//! Contains all definition of [`Target`].

#![allow(missing_docs)]

use std::{
    collections::{hash_map::Entry, HashMap},
    convert::Into,
    path::{Path, PathBuf},
    sync::Arc,
};

use derive_more::From;
use drain_filter_polyfill::VecExt;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use parking_lot::RwLock;
use pernixc_base::{
    diagnostic::{Diagnostic, Related, Report},
    handler::Handler,
    log::Severity,
    source_file::{self, SourceFile, Span},
};
use pernixc_lexical::token_stream::{TokenStream, Tree};
use rayon::iter::{ParallelBridge, ParallelIterator};

use super::{
    item::{Module, ModuleContent, ModuleSignature},
    AccessModifier, SyntaxTree,
};
use crate::{
    error, state_machine::parse::Parse, syntax_tree::item::ModuleKind,
};

pub mod strategy;

/// Contains both the access modifier and the module signature.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ModuleSignatureWithAccessModifier {
    pub access_modifier: AccessModifier,
    pub signature: ModuleSignature,
}

/// Represents a syntax tree node for a module with its submodule children.
#[derive(Debug, Clone, PartialEq, Eq, Getters)]
pub struct ModuleTree {
    /// The signature syntax tree of this module.
    ///
    /// If this module is the root module, this field will be `None`.
    #[get = "pub"]
    signature: Option<ModuleSignatureWithAccessModifier>,

    /// Contains the content of this module.
    ///
    /// The [`ModuleContent::items`] field will not contain any
    /// [`super::item::Module`]s as they are stored in the
    /// [`Self::submodules_by_name`] field instead.
    #[get = "pub"]
    module_content: ModuleContent,

    /// Contains the submodules of this module.
    #[get = "pub"]
    submodules_by_name: HashMap<String, ModuleTree>,
}

impl ModuleTree {
    /// Dissolves the [`ModuleTree`] into its tuple of fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (
        Option<ModuleSignatureWithAccessModifier>,
        ModuleContent,
        HashMap<String, Self>,
    ) {
        (self.signature, self.module_content, self.submodules_by_name)
    }
}

/// Is a complete syntax tree representing the whole target program.
#[derive(Debug, Clone, PartialEq, Eq, Getters)]
pub struct Target {
    /// Contains the content of the root module.
    #[get = "pub"]
    module_tree: ModuleTree,

    /// The name of the target.
    #[get = "pub"]
    name: String,
}

impl Target {
    /// Dissolves the target into its module tree and target name.
    #[must_use]
    pub fn dissolve(self) -> (ModuleTree, String) {
        (self.module_tree, self.name)
    }
}

/// The submodule of the root source file ends up pointing to the root source
/// file itself.
#[derive(Debug, Clone)]
pub struct RootSubmoduleConflict {
    /// The root source file.
    pub root: Arc<SourceFile>,

    /// The submodule that points to the root source file.
    pub submodule_span: Span,
}

impl Report<()> for RootSubmoduleConflict {
    type Error = ();

    fn report(&self, (): ()) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            severity: Severity::Error,
            message: "the submodule of the root source file ends up pointing \
                      to the root source file itself"
                .to_string(),
            span: self.submodule_span.clone(),
            help_message: None,
            related: vec![],
        })
    }
}

/// Failed to load a source file for the submodule.
#[derive(Debug)]
pub struct SourceFileLoadFail {
    /// The error that occurred while loading the source file.
    pub source_error: source_file::Error,

    /// The submodule that submodule stems from.
    pub submodule: Module,

    /// The failed loading path.
    pub path: PathBuf,
}

impl Report<()> for SourceFileLoadFail {
    type Error = ();

    fn report(&self, (): ()) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            severity: Severity::Error,
            message: "failed to load a source file for the submodule"
                .to_string(),
            span: self.submodule.signature().identifier().span.clone(),
            help_message: Some(format!(
                "{}: {}",
                self.path.display(),
                self.source_error
            )),
            related: vec![],
        })
    }
}

/// A module with the given name already exists.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleRedefinition {
    /// The span of the existing module.
    pub existing_module_span: Span,

    /// The submodule that redefines the module.
    pub redefinition_submodule_span: Span,
}

impl Report<()> for ModuleRedefinition {
    type Error = ();

    fn report(&self, (): ()) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            severity: Severity::Error,
            message: "a module with the given name already exists".to_string(),
            span: self.redefinition_submodule_span.clone(),
            help_message: None,
            related: vec![Related {
                span: self.existing_module_span.clone(),
                message: "existing module".to_string(),
            }],
        })
    }
}

/// Contains all possible errors that can occur when parsing a module tree for
/// the target program.
#[derive(Debug, EnumAsInner, From)]
pub enum Error {
    ModuleRedefinition(ModuleRedefinition),
    RootSubmoduleConflict(RootSubmoduleConflict),
    SourceFileLoadFail(SourceFileLoadFail),
}

impl Report<()> for Error {
    type Error = ();

    fn report(&self, (): ()) -> Result<Diagnostic, Self::Error> {
        match self {
            Self::ModuleRedefinition(err) => err.report(()),
            Self::RootSubmoduleConflict(err) => err.report(()),
            Self::SourceFileLoadFail(err) => err.report(()),
        }
    }
}

#[derive(Debug, Clone)]
enum Input {
    Inline {
        module_content: ModuleContent,
        module_name: String,
        signature: ModuleSignatureWithAccessModifier,
    },
    File {
        source_file: Arc<SourceFile>,
        signature: Option<ModuleSignatureWithAccessModifier>,
    },
}

impl Target {
    #[allow(clippy::too_many_lines)]
    fn parse_input<
        T: Handler<error::Error>
            + Handler<pernixc_lexical::error::Error>
            + Handler<Error>,
    >(
        input: Input,
        current_directory: &Path,
        handler: &T,
    ) -> ModuleTree {
        let (mut module_content, source_file, current_module_name, signature) =
            match input {
                Input::Inline {
                    module_content,
                    module_name,
                    signature: access_modifier,
                } => (module_content, None, module_name, Some(access_modifier)),
                Input::File { source_file, signature: access_modifier } => {
                    let token_stream =
                        TokenStream::tokenize(source_file.clone(), handler);
                    let tree = Tree::new(&token_stream);

                    let module_content =
                        ModuleContent::parse.parse_syntax(&tree, handler);

                    (
                        module_content.unwrap_or(ModuleContent {
                            usings: Vec::new(),
                            items: Vec::new(),
                        }),
                        Some((access_modifier.is_none(), source_file.clone())),
                        source_file
                            .full_path()
                            .file_stem()
                            .unwrap()
                            .to_str()
                            .unwrap()
                            .to_string(),
                        access_modifier,
                    )
                }
            };

        let submodule_current_directory =
            if source_file.as_ref().map_or(false, |x| x.0) {
                current_directory.to_path_buf()
            } else {
                current_directory.join(&current_module_name)
            };

        let module_contents_by_name =
            RwLock::new(HashMap::<String, ModuleTree>::new());

        module_content.items.drain_filter(|x| x.is_module()).par_bridge().for_each(|item| {
            let submodule = item.into_module().unwrap();

            let module_name = submodule.signature().identifier().span.str().to_owned();
            let module_tree = if let ModuleKind::Inline(inline_module_content) = submodule.kind {
                let signature_with_access_modifier = ModuleSignatureWithAccessModifier {
                    access_modifier: submodule.access_modifier,
                    signature: submodule.signature,
                };
                Self::parse_input(
                    Input::Inline {
                        module_content: inline_module_content.tree,
                        module_name: signature_with_access_modifier
                            .signature
                            .identifier()
                            .span
                            .str()
                            .to_string(),
                        signature: signature_with_access_modifier,
                    },
                    &submodule_current_directory,
                    handler,
                )
            } else {
                match &source_file {
                    Some((true, source_file))
                        if current_module_name == submodule.signature.identifier().span.str() =>
                    {
                        handler.receive(Error::RootSubmoduleConflict(RootSubmoduleConflict {
                            root: source_file.clone(),
                            submodule_span: submodule.signature.identifier().span.clone(),
                        }));
                        return;
                    }
                    _ => {
                        let mut source_file_path = submodule_current_directory
                            .join(submodule.signature.identifier().span.str());
                        source_file_path.set_extension("pnx");

                        // load the source file
                        let source_file = match std::fs::File::open(&source_file_path)
                            .map_err(Into::into)
                            .and_then(|file| SourceFile::load(file, source_file_path.clone()))
                        {
                            Ok(source_file) => Arc::new(source_file),
                            Err(source_error) => {
                                handler.receive(Error::SourceFileLoadFail(SourceFileLoadFail {
                                    source_error,
                                    submodule,
                                    path: source_file_path,
                                }));
                                return;
                            }
                        };

                        let submodule_signature_with_access_modifier =
                            ModuleSignatureWithAccessModifier {
                                access_modifier: submodule.access_modifier,
                                signature: submodule.signature,
                            };
                        let submodule_current_directory = &submodule_current_directory;

                        Self::parse_input(
                            Input::File {
                                source_file,
                                signature: Some(submodule_signature_with_access_modifier),
                            },
                            submodule_current_directory,
                            handler,
                        )
                    }
                }
            };

            match module_contents_by_name.write().entry(module_name) {
                Entry::Occupied(entry) => {
                    handler.receive(Error::ModuleRedefinition(ModuleRedefinition {
                        existing_module_span: entry.get()
                            .signature()
                            .as_ref()
                            .unwrap()
                            .signature
                            .identifier()
                            .span
                            .clone(),
                        redefinition_submodule_span: module_tree
                            .signature()
                            .as_ref()
                            .unwrap()
                            .signature
                            .identifier()
                            .span
                            .clone(),
                    }));
                }

                Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(module_tree);
                }
            }
        });

        ModuleTree {
            signature,
            module_content,
            submodules_by_name: module_contents_by_name.into_inner(),
        }
    }

    /// Parses the whole module tree for the target program from the given root
    /// source file.
    pub fn parse<
        T: Handler<error::Error>
            + Handler<pernixc_lexical::error::Error>
            + Handler<Error>
            + Send
            + Sync,
    >(
        root_source_file: &Arc<SourceFile>,
        target_name: String,
        handler: &T,
    ) -> Self {
        let module_tree = Self::parse_input(
            Input::File {
                source_file: root_source_file.clone(),
                signature: None,
            },
            root_source_file
                .full_path()
                .parent()
                .unwrap_or_else(|| Path::new("")),
            handler,
        );

        Self { module_tree, name: target_name }
    }
}

#[cfg(test)]
mod test;
