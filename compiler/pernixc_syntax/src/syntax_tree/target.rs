//! Contains the definition of [`Target`]

use std::{collections::HashSet, path::PathBuf, str::FromStr, sync::Arc};

use derive_more::{Deref, DerefMut, From};
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_lexical::{
    token::{Identifier, Keyword, KeywordKind, Punctuation, Token},
    token_stream::TokenStream,
};
use pernixc_print::LogSeverity;
use pernixc_source::{SourceElement, SourceFile, Span};
use pernixc_system::diagnostic::Dummy;

use super::AccessModifier;
use crate::{
    error::Error as SyntaxError,
    parser::Parser,
    syntax_tree::{item::Item, ScopeSeparator},
};

/// Represents a moulde path in used in `module` declarations and `using` statements.
///
/// Syntax Synopsis:
/// ``` txt
/// ModulePath:
///     Identifier ('::' Identifier)*
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct ModulePath {
    #[get = "pub"]
    first: Identifier,
    #[get = "pub"]
    rest: Vec<(ScopeSeparator, Identifier)>,
}

impl ModulePath {
    /// Returns an iterator over the path identifiers.
    pub fn paths(&self) -> impl Iterator<Item = &Identifier> {
        std::iter::once(&self.first).chain(self.rest.iter().map(|(_, id)| id))
    }
}

/// Represents a syntax tree node for a `module` using statement.
///
/// Syntax Synopsis:
/// ``` txt
/// Using:
///     'using' ModulePath ';'
///     ;
/// ```
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Using {
    pub using_keyword: Keyword,
    pub module_path: ModulePath,
    pub semicolon: Punctuation,
}

/// Represents a syntax tree node for a `module` declaration.
///
/// Syntax Synopsis:
/// ``` txt
/// Module:
///     AccessModifier 'module' Identifier ';'
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct Module {
    #[get = "pub"]
    access_modifier: AccessModifier,
    #[get = "pub"]
    module_keyword: Keyword,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for Using {
    fn span(&self) -> Span { self.using_keyword.span.join(&self.semicolon.span).unwrap() }
}

/// Contains all the syntax trees defined within a single file.
///
/// Syntax Synopsis:
/// ``` txt
/// File:
///     Using*
///     Module*
///     Item*
///     ;
/// ```
#[derive(Debug, Clone)]
pub struct File {
    /// List of using statements syntax tree.
    pub usings: Vec<Using>,

    /// List of using statements syntax tree.
    pub submodules: Vec<Submodule>,

    /// List of item syntax trees defined within the file.
    pub items: Vec<Item>,

    /// The source file from which the syntax tree was parsed.
    pub source_file: Arc<SourceFile>,
}

/// Composition of a [`Module`] declaration syntax tree and the [`File`] syntax tree representing
/// the contents of the module.
#[derive(Debug, Clone)]
pub struct Submodule {
    /// THe [`Module`] declaration syntax tree.
    pub module: Module,

    /// The [`File`] syntax tree representing the contents of the module.
    pub file: File,
}

/// The syntax tree containing the definition of the whole target's module tree.
#[derive(Debug, Deref, DerefMut, Getters)]
pub struct Target {
    #[deref]
    #[deref_mut]
    #[get = "pub"]
    root_file: File,

    /// The name of the target.
    #[get = "pub"]
    name: String,
}

impl<'a> Parser<'a> {
    /// Parses a [`ModulePath`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_module_path(
        &mut self,
        handler: &impl pernixc_system::diagnostic::Handler<SyntaxError>,
    ) -> Option<ModulePath> {
        let first_identifier = self.parse_identifier(handler)?;
        let mut rest = Vec::new();

        while let Some(scope_separator) = self.try_parse(|this| this.parse_scope_separator(&Dummy))
        {
            let identifier = self.parse_identifier(handler)?;
            rest.push((scope_separator, identifier));
        }

        Some(ModulePath {
            first: first_identifier,
            rest,
        })
    }

    /// Parses a [`Using`] declaration.
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_using(
        &mut self,
        handler: &impl pernixc_system::diagnostic::Handler<SyntaxError>,
    ) -> Option<Using> {
        let using_keyword = self.parse_keyword(KeywordKind::Using, handler)?;
        let module_path = self.parse_module_path(handler)?;
        let semicolon = self.parse_punctuation(';', true, handler)?;

        Some(Using {
            using_keyword,
            module_path,
            semicolon,
        })
    }

    /// Parses a [`Module`] declaration.
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_module(
        &mut self,
        handler: &impl pernixc_system::diagnostic::Handler<SyntaxError>,
    ) -> Option<Module> {
        let access_modifier = self.parse_access_modifier(handler)?;
        let module_keyword = self.parse_keyword(KeywordKind::Module, handler)?;
        let identifier = self.parse_identifier(handler)?;
        let semicolon = self.parse_punctuation(';', true, handler)?;

        Some(Module {
            access_modifier,
            module_keyword,
            identifier,
            semicolon,
        })
    }

    fn parse_usings(
        &mut self,
        handler: &impl pernixc_system::diagnostic::Handler<SyntaxError>,
    ) -> Vec<Using> {
        let mut usings = Vec::new();
        loop {
            let using_keyword = match self.stop_at_significant() {
                Some(Token::Keyword(using_keyword))
                    if using_keyword.keyword == KeywordKind::Using =>
                {
                    self.forward();
                    using_keyword
                }
                _ => break,
            };

            #[allow(clippy::collection_is_never_read)]
            let using: Option<Using> = (|| {
                let module_path = self.parse_module_path(handler)?;
                let semicolon = self.parse_punctuation(';', true, handler)?;
                Some(Using {
                    using_keyword,
                    module_path,
                    semicolon,
                })
            })();

            // try to stop after the semicolon or brace pair
            using.map_or_else(
                || {
                    self.stop_at(|token| {
                        matches!(token, Token::Punctuation(p) if p.punctuation == ';' ||  p.punctuation == '{')
                    });
                    self.forward();
                },
                |using| usings.push(using),
            );
        }

        usings
    }

    fn parse_modules(
        &mut self,
        handler: &impl pernixc_system::diagnostic::Handler<SyntaxError>,
    ) -> Vec<Module> {
        let mut modules = Vec::new();

        loop {
            let result = self.try_parse(|parser| {
                let access_modifier = parser.parse_access_modifier(&Dummy)?;
                let module_keyword = parser.parse_keyword(KeywordKind::Module, &Dummy)?;

                Some((access_modifier, module_keyword))
            });

            let Some((access_modifier, module_keyword)) = result else {
                break;
            };

            #[allow(clippy::collection_is_never_read)]
            let result: Option<(Identifier, Punctuation)> = (|| {
                let identifier = self.parse_identifier(handler)?;
                let semicolon = self.parse_punctuation(';', true, handler)?;

                Some((identifier, semicolon))
            })();

            // try to stop after the semicolon or brace pair
            result.map_or_else(
                || {
                    self.stop_at(|token| {
                        matches!(token, Token::Punctuation(p) if p.punctuation == ';' ||  p.punctuation == '{')
                    });
                    self.forward();
                },
                |(identifier, semicolon)| modules.push(Module {
                    access_modifier,
                    module_keyword,
                    identifier,
                    semicolon
                })
            );
        }

        modules
    }

    fn parse_file(
        &mut self,
        handler: &impl pernixc_system::diagnostic::Handler<SyntaxError>,
    ) -> (Vec<Using>, Vec<Module>, Vec<Item>) {
        let usings = self.parse_usings(handler);
        let modules = self.parse_modules(handler);
        let items = {
            let mut items = Vec::new();
            while !self.is_exhausted() {
                self.parse_item(handler).map_or_else(|| {
                    self.stop_at(|token| {
                        matches!(token, Token::Punctuation(p) if p.punctuation == ';' ||  p.punctuation == '{')
                    });
                    self.forward();
                }, |item| items.push(item));
            }
            items
        };

        (usings, modules, items)
    }
}

/// Is a super trait of [`pernixc_system::diagnostic::Handler`] that allows handling all errors
/// occurred while parsing the [`Target`]
pub trait Handler:
    pernixc_system::diagnostic::Handler<pernixc_lexical::error::Error>
    + pernixc_system::diagnostic::Handler<crate::error::Error>
    + pernixc_system::diagnostic::Handler<Error>
{
}

impl<
        T: pernixc_system::diagnostic::Handler<pernixc_lexical::error::Error>
            + pernixc_system::diagnostic::Handler<crate::error::Error>
            + pernixc_system::diagnostic::Handler<Error>,
    > Handler for T
{
}

/// Is an error that occurred while parsing the [`Target::parse()`].
#[derive(Debug, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Error {
    RootSubmoduleConflict(RootSubmoduleConflict),
    SourceFileLoadFail(SourceFileLoadFail),
    ModuleRedefinition(ModuleRedefinition),
}

impl Error {
    /// Prints the error message to the console.
    pub fn print(&self) {
        match self {
            Self::RootSubmoduleConflict(error) => error.print(),
            Self::SourceFileLoadFail(error) => error.print(),
            Self::ModuleRedefinition(error) => error.print(),
        }
    }
}

/// The submodule of the root source file ends up pointing to the root source file itself.
#[derive(Debug, Clone)]
pub struct RootSubmoduleConflict {
    /// The root source file.
    pub root: Arc<SourceFile>,

    /// The submodule that points to the root source file.
    pub submodule: Module,
}

impl RootSubmoduleConflict {
    /// Prints the error message to the console.
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            "the submodule's source file ended up having the same path as the parent module \
             itself.",
        );

        pernixc_print::print_source_code(&self.submodule.identifier.span, None);
    }
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

impl SourceFileLoadFail {
    /// Prints the error message to the console.
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            &format!(
                "failed to load the submodule's source file from the path `{}`.",
                self.path.display()
            ),
        );

        pernixc_print::print_source_code(&self.submodule.identifier.span, None);
    }
}

/// A module with the given name already exists.
#[derive(Debug, Clone)]
pub struct ModuleRedefinition {
    /// The submodule that is redefined.
    pub redifinition_submodule: Module,
}

impl ModuleRedefinition {
    /// Prints the error message to the console.
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            &format!(
                "the module `{}` was already defined.",
                self.redifinition_submodule.identifier.span.str()
            ),
        );

        pernixc_print::print_source_code(&self.redifinition_submodule.identifier.span, None);
    }
}

impl Target {
    /// Dissolves the target into the root source file and the name of the root module.
    #[must_use]
    pub fn dissolve(self) -> (File, String) { (self.root_file, self.name) }

    fn parse_single_file(
        source_file: Arc<SourceFile>,
        handler: &impl Handler,
        is_root: bool,
    ) -> File {
        let token_stream = TokenStream::tokenize(&source_file, handler);
        let mut parser = Parser::new(&token_stream);

        let (usings, modules, items) = parser.parse_file(handler);
        let submodule_files = std::thread::scope(|scope| {
            let mut submodules = Vec::new();
            let mut defined_submodules = HashSet::new();

            for module in &modules {
                if !defined_submodules.insert(module.identifier.span.str()) {
                    submodules.push(None);
                    handler.receive(Error::ModuleRedefinition(ModuleRedefinition {
                        redifinition_submodule: module.clone(),
                    }));
                    continue;
                }

                // get the file path of the module
                let submodule_path = if is_root {
                    let mut submodule_path = source_file.full_path().parent().map_or_else(
                        || {
                            PathBuf::from_str(module.identifier.span.str())
                                .expect("should not fail")
                        },
                        |parent| parent.join(module.identifier.span.str()),
                    );

                    // check if the submodule path is the same as the root file path
                    if submodule_path == *source_file.full_path() {
                        submodules.push(None);
                        handler.receive(Error::RootSubmoduleConflict(RootSubmoduleConflict {
                            root: source_file.clone(),
                            submodule: module.clone(),
                        }));
                        continue;
                    }

                    submodule_path.set_extension("pnx");

                    submodule_path
                } else {
                    source_file
                        .full_path()
                        .with_extension("")
                        .join(format!("{}.pnx", module.identifier.span.str()))
                };

                // submodule source file
                let submodule_source_file = match SourceFile::load(&submodule_path) {
                    Ok(submodule_source_file) => submodule_source_file,
                    Err(io_error) => {
                        submodules.push(None);
                        handler.receive(Error::SourceFileLoadFail(SourceFileLoadFail {
                            io_error,
                            path: submodule_path,
                            submodule: module.clone(),
                        }));
                        continue;
                    }
                };

                submodules.push(Some(scope.spawn(|| {
                    Self::parse_single_file(submodule_source_file, handler, false)
                })));
            }

            submodules
                .into_iter()
                .map(|submodule| submodule.map(|submodule| submodule.join().unwrap()))
                .collect::<Vec<_>>()
        });

        let mut submodules = Vec::new();
        for (module, file) in modules.into_iter().zip(submodule_files.into_iter()) {
            let Some(file) = file else {
                continue;
            };

            submodules.push(Submodule { module, file });
        }

        File {
            usings,
            submodules,
            items,
            source_file,
        }
    }

    /// Parses the whole target tree
    pub fn parse(module_root_file: Arc<SourceFile>, name: String, handler: &impl Handler) -> Self {
        Self {
            root_file: Self::parse_single_file(module_root_file, handler, true),
            name,
        }
    }
}
