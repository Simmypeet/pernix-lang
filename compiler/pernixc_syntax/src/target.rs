//! Contains the definition of [`Target`]

use std::sync::Arc;

use derive_more::{Deref, DerefMut};
use pernixc_lexical::token::{
    strategy::keyword_kind, Identifier, Keyword, KeywordKind, Punctuation, Token,
};
use pernixc_source::{SourceElement, SourceFile, Span, SpanError};
use pernixc_system::diagnostic::{Dummy, Handler};

use crate::{
    error::Error,
    parser::{Error as ParserError, Parser, Result as ParserResult},
    syntax_tree::{
        item::{AccessModifier, Item},
        ConnectedList, ScopeSeparator,
    },
};

/// Represents a moulde path in used in `module` declarations and `using` statements.
///
/// Syntax Synopsis:
/// ``` txt
/// ModulePath:
///     Identifier ('::' Identifier)*
///     ;
/// ```
pub type ModulePath = ConnectedList<Identifier, ScopeSeparator>;

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
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Module {
    pub access_modifier: AccessModifier,
    pub module_keyword: Keyword,
    pub identifier: Identifier,
    pub semicolon: Punctuation,
}

impl SourceElement for Using {
    fn span(&self) -> Result<Span, SpanError> { self.using_keyword.span.join(&self.semicolon.span) }
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
#[derive(Debug, Deref, DerefMut)]
pub struct Target {
    #[deref]
    #[deref_mut]
    file: File,
}

impl<'a> Parser<'a> {
    /// Parses a [`ModulePath`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_module_path(&mut self, handler: &impl Handler<Error>) -> ParserResult<ModulePath> {
        let first = self.parse_identifier(handler)?;
        let mut rest = Vec::new();

        while let Ok(scope_separator) = self.try_parse(|this| this.parse_scope_separator(&Dummy)) {
            let identifier = self.parse_identifier(handler)?;
            rest.push((scope_separator, identifier));
        }

        Ok(ConnectedList {
            first,
            rest,
            trailing_separator: None,
        })
    }

    /// Parses a [`Using`] declaration.
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_using(&mut self, handler: &impl Handler<Error>) -> ParserResult<Using> {
        let using_keyword = self.parse_keyword(KeywordKind::Using, handler)?;
        let module_path = self.parse_module_path(handler)?;
        let semicolon = self.parse_punctuation(';', true, handler)?;

        Ok(Using {
            using_keyword,
            module_path,
            semicolon,
        })
    }

    /// Parses a [`Module`] declaration.
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_module(&mut self, handler: &impl Handler<Error>) -> ParserResult<Module> {
        let access_modifier = self.parse_access_modifier(handler)?;
        let module_keyword = self.parse_keyword(KeywordKind::Module, handler)?;
        let identifier = self.parse_identifier(handler)?;
        let semicolon = self.parse_punctuation(';', true, handler)?;

        Ok(Module {
            access_modifier,
            module_keyword,
            identifier,
            semicolon,
        })
    }

    fn parse_usings(&mut self, handler: &impl Handler<Error>) -> Vec<Using> {
        let mut usings = Vec::new();
        loop {
            let using_keyword = match self.stop_at_significant() {
                Some(Token::Keyword(using_keyword))
                    if using_keyword.keyword == KeywordKind::Using =>
                {
                    using_keyword
                }
                _ => break,
            };

            let using: Result<Using, ParserError> = (|| {
                let module_path = self.parse_module_path(handler)?;
                let semicolon = self.parse_punctuation(';', true, handler)?;
                Ok(Using {
                    using_keyword,
                    module_path,
                    semicolon,
                })
            })();

            // try to stop after the semicolon or brace pair
            using.map_or_else(
                |_| {
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

    fn parse_modules(&mut self, handler: &impl Handler<Error>) -> Vec<Module> {
        let mut modules = Vec::new();

        loop {
            let result = self.try_parse(|parser| {
                let access_modifier = parser.parse_access_modifier(handler)?;
                let module_keyword = parser.parse_keyword(KeywordKind::Module, handler)?;

                Ok((access_modifier, module_keyword))
            });

            let Ok((access_modifier, module_keyword)) = result else {
                break;
            };

            let result: Result<(Identifier, Punctuation), ParserError> = (|| {
                let identifier = self.parse_identifier(handler)?;
                let semicolon = self.parse_punctuation(';', true, handler)?;

                Ok((identifier, semicolon))
            })();

            // try to stop after the semicolon or brace pair
            result.map_or_else(
                |_| {
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
        handler: &impl Handler<Error>,
    ) -> (Vec<Using>, Vec<Module>, Vec<Item>) {
        let usings = self.parse_usings(handler);
        let modules = self.parse_modules(handler);
        let items = {
            let mut items = Vec::new();
            while !self.is_exhausted() {
                self.parse_item(handler).map_or_else(|_| {
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
