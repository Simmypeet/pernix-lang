use std::{
    collections::{hash_map::Entry, HashMap},
    path::{Path, PathBuf},
    sync::Arc,
    thread::ScopedJoinHandle,
};

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_lexical::token_stream::TokenStream;
use pernixc_source::{SourceFile, Span};
use pernixc_system::diagnostic::Handler;

use super::{
    item::{Module, ModuleContent},
    AccessModifier,
};
use crate::{error, parser::Parser, syntax_tree::item::ModuleKind};

/// Represents a syntax tree node for a module with its submodule children.
#[derive(Debug, Clone, Getters)]
pub struct ModuleTree {
    /// The access modifier of this module.
    ///
    /// If this module is the root module, this field will be `None`.
    #[get = "pub"]
    access_modifier: Option<AccessModifier>,

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

/// Represents a parsing target containing a module tree representing the whole target program.
#[derive(Debug, Clone, Getters)]
pub struct Target {
    /// Contains the content of the root module.
    #[get = "pub"]
    module_tree: ModuleTree,

    /// The name of the target.
    #[get = "pub"]
    target_name: String,
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
    pub io_error: std::io::Error,

    /// The submodule that submodule stems from.
    pub submodule: Module,

    /// The failed loading path.
    pub path: PathBuf,
}

/// A module with the given name already exists.
#[derive(Debug, Clone)]
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
        access_modifier: AccessModifier,
    },
    File {
        source_file: Arc<SourceFile>,
        access_modifier: Option<AccessModifier>,
    },
}

impl Target {
    #[allow(clippy::too_many_lines)]
    fn parse_input<
        T: Handler<error::Error> + Handler<pernixc_lexical::error::Error> + Handler<Error>,
    >(
        input: Input,
        current_directory: &Path,
        handler: &T,
    ) -> ModuleTree {
        let (mut module_content, source_file, current_module_name, access_modifier) = match input {
            Input::Inline {
                module_content,
                module_name,
                access_modifier,
            } => (module_content, None, module_name, Some(access_modifier)),
            Input::File {
                source_file,
                access_modifier,
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
                        let submodule_name =
                            submodule.signature.identifier().span.str().to_string();
                        entry.insert((
                            scope.spawn(|| {
                                Self::parse_input(
                                    Input::Inline {
                                        module_content: inline_module_content.content,
                                        module_name: submodule_name,
                                        access_modifier: submodule.access_modifier,
                                    },
                                    &submodule_current_directory,
                                    handler,
                                )
                            }),
                            submodule.signature.identifier().span.clone(),
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
                                let source_file = match SourceFile::load(&source_file_path) {
                                    Ok(source_file) => source_file,
                                    Err(io_error) => {
                                        handler.receive(Error::SourceFileLoadFail(
                                            SourceFileLoadFail {
                                                io_error,
                                                submodule,
                                                path: source_file_path,
                                            },
                                        ));
                                        index += 1;
                                        continue;
                                    }
                                };

                                entry.insert((
                                    scope.spawn(|| {
                                        Self::parse_input(
                                            Input::File {
                                                source_file,
                                                access_modifier: Some(submodule.access_modifier),
                                            },
                                            &submodule_current_directory,
                                            handler,
                                        )
                                    }),
                                    submodule.signature.identifier().span.clone(),
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
            access_modifier,
            module_content,
            submodules_by_name,
        }
    }

    /// Parses the whole module tree for the target program from the given root source file.
    pub fn parse<
        T: Handler<error::Error> + Handler<pernixc_lexical::error::Error> + Handler<Error>,
    >(
        root_source_file: &Arc<SourceFile>,
        target_name: String,
        handler: &T,
    ) -> Self {
        let module_tree = Self::parse_input(
            Input::File {
                source_file: root_source_file.clone(),
                access_modifier: None,
            },
            root_source_file
                .full_path()
                .parent()
                .unwrap_or_else(|| Path::new("")),
            handler,
        );

        Self {
            module_tree,
            target_name,
        }
    }
}

#[cfg(test)]
mod tests;
