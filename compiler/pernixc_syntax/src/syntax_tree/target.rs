use std::{
    collections::{hash_map::Entry, HashMap},
    convert::Into,
    path::{Path, PathBuf},
    sync::Arc,
    thread::ScopedJoinHandle,
};

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_base::{
    diagnostic::Handler,
    source_file::{self, SourceFile, Span},
};
use pernixc_lexical::token_stream::TokenStream;

use super::{
    item::{Module, ModuleContent, ModuleSignature},
    AccessModifier,
};
use crate::{error, parser::Parser, syntax_tree::item::ModuleKind};

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
    /// The [`ModuleContent::items`] field will not contain any [`super::item::Module`]s as they
    /// are stored in the [`Self::submodules_by_name`] field instead.
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
    pub fn dissolve(self) -> (ModuleTree, String) { (self.module_tree, self.name) }
}

/// The submodule of the root source file ends up pointing to the root source file itself.
#[derive(Debug, Clone)]
pub struct RootSubmoduleConflict {
    /// The root source file.
    pub root: Arc<SourceFile>,

    /// The submodule that points to the root source file.
    pub submodule_span: Span,
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

/// A module with the given name already exists.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleRedefinition {
    /// The span of the existing module.
    pub existing_module_span: Span,

    /// The submodule that redefines the module.
    pub redifinition_submodule_span: Span,
}

/// Contains all possible errors that can occur when parsing a module tree for the target program.
#[derive(Debug, EnumAsInner, From)]
pub enum Error {
    ModuleRedefinition(ModuleRedefinition),
    RootSubmoduleConflict(RootSubmoduleConflict),
    SourceFileLoadFail(SourceFileLoadFail),
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
            + Handler<Error>
            + Send
            + Sync,
    >(
        input: Input,
        current_directory: &Path,
        handler: &T,
    ) -> ModuleTree {
        let (mut module_content, source_file, current_module_name, signature) = match input {
            Input::Inline {
                module_content,
                module_name,
                signature: access_modifier,
            } => (module_content, None, module_name, Some(access_modifier)),
            Input::File {
                source_file,
                signature: access_modifier,
            } => {
                let token_stream = TokenStream::tokenize(&source_file, handler);
                let mut parser = Parser::new(&token_stream);

                (
                    parser.parse_module_content(handler),
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
        let mut index = 0;
        let submodule_current_directory = if source_file.as_ref().map_or(false, |x| x.0) {
            current_directory.to_path_buf()
        } else {
            current_directory.join(&current_module_name)
        };

        let submodules_by_name = std::thread::scope(|scope| {
            let mut module_contents_by_name =
                HashMap::<String, (ScopedJoinHandle<ModuleTree>, Span)>::new();

            while index < module_content.items.len() {
                if module_content.items[index].is_module() {
                    let submodule = module_content.items.remove(index).into_module().unwrap();

                    // check for redifinitions
                    let entry = match module_contents_by_name
                        .entry(submodule.signature().identifier().span.str().to_string())
                    {
                        Entry::Occupied(entry) => {
                            handler.receive(Error::ModuleRedefinition(ModuleRedefinition {
                                existing_module_span: entry.get().1.clone(),
                                redifinition_submodule_span: submodule
                                    .signature()
                                    .identifier()
                                    .span
                                    .clone(),
                            }));
                            index += 1;
                            continue;
                        }
                        Entry::Vacant(entry) => entry,
                    };

                    if let ModuleKind::Inline(inline_module_content) = submodule.kind {
                        let signature_wtih_access_modifier = ModuleSignatureWithAccessModifier {
                            access_modifier: submodule.access_modifier,
                            signature: submodule.signature,
                        };
                        let identifier_span = signature_wtih_access_modifier
                            .signature
                            .identifier()
                            .span
                            .clone();
                        let identifier_span_closure = identifier_span.clone();
                        let submodule_current_directory = &submodule_current_directory;

                        entry.insert((
                            scope.spawn(move || {
                                Self::parse_input(
                                    Input::Inline {
                                        module_content: inline_module_content.content,
                                        module_name: identifier_span_closure.str().to_string(),
                                        signature: signature_wtih_access_modifier,
                                    },
                                    submodule_current_directory,
                                    handler,
                                )
                            }),
                            identifier_span,
                        ));
                    } else {
                        match &source_file {
                            Some((true, source_file))
                                if current_module_name
                                    == submodule.signature.identifier().span.str() =>
                            {
                                handler.receive(Error::RootSubmoduleConflict(
                                    RootSubmoduleConflict {
                                        root: source_file.clone(),
                                        submodule_span: submodule
                                            .signature
                                            .identifier()
                                            .span
                                            .clone(),
                                    },
                                ));
                            }
                            _ => {
                                let mut source_file_path = submodule_current_directory
                                    .join(submodule.signature.identifier().span.str());
                                source_file_path.set_extension("pnx");

                                // load the source file
                                let source_file = match std::fs::File::open(&source_file_path)
                                    .map_err(Into::into)
                                    .and_then(|file| {
                                        SourceFile::load(file, source_file_path.clone())
                                    }) {
                                    Ok(source_file) => source_file,
                                    Err(source_error) => {
                                        handler.receive(Error::SourceFileLoadFail(
                                            SourceFileLoadFail {
                                                source_error,
                                                submodule,
                                                path: source_file_path,
                                            },
                                        ));
                                        index += 1;
                                        continue;
                                    }
                                };

                                let submodule_signature_with_access_modifier =
                                    ModuleSignatureWithAccessModifier {
                                        access_modifier: submodule.access_modifier,
                                        signature: submodule.signature,
                                    };
                                let identifier_span = submodule_signature_with_access_modifier
                                    .signature
                                    .identifier()
                                    .span
                                    .clone();
                                let submodule_current_directory = &submodule_current_directory;

                                entry.insert((
                                    scope.spawn(move || {
                                        Self::parse_input(
                                            Input::File {
                                                source_file,
                                                signature: Some(
                                                    submodule_signature_with_access_modifier,
                                                ),
                                            },
                                            submodule_current_directory,
                                            handler,
                                        )
                                    }),
                                    identifier_span,
                                ));
                            }
                        }
                    }
                } else {
                    index += 1;
                }
            }

            module_contents_by_name
                .into_iter()
                .map(|(name, (join_handle, _))| (name, join_handle.join().unwrap()))
                .collect()
        });

        ModuleTree {
            signature,
            module_content,
            submodules_by_name,
        }
    }

    /// Parses the whole module tree for the target program from the given root source file.
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

        Self {
            module_tree,
            name: target_name,
        }
    }
}

#[cfg(test)]
mod tests;
