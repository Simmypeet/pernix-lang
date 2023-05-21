//! Contains the syntax tree nodes for statements

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_lexical::token::{Identifier, Keyword, KeywordKind, Punctuation, Token};
use pernixc_source::Span;
use pernixc_system::error_handler::ErrorHandler;

use super::{
    expression::{Expression, Functional, Imperative},
    SourceElement, TypeAnnotation,
};
use crate::{error::Error, parser::Parser};

/// Represents a statement syntax tree node
///
/// Syntax Synopsis:
/// ``` text
/// Statement:
///     Declarative
///     | Expressive
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum Statement {
    Declarative(Declarative),
    Expressive(Expressive),
}

impl SourceElement for Statement {
    fn span(&self) -> Span {
        match self {
            Self::Declarative(declaration) => declaration.span(),
            Self::Expressive(expression) => expression.span(),
        }
    }
}

/// Represents a declarative statement syntax tree node
///
/// Declarative statements are statements that introduce a new symbol into the current scope.
///
/// Syntax Synopsis:
/// ``` text
/// Declarative:
///     VariableDeclaration
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]

pub enum Declarative {
    VariableDeclaration(VariableDeclaration),
}

impl SourceElement for Declarative {
    fn span(&self) -> Span {
        match self {
            Self::VariableDeclaration(declaration) => declaration.span(),
        }
    }
}

/// Represents a variable declaration syntax tree node
///
/// Syntax Synopsis:
/// ``` text
/// VariableDeclaration:
///     VariableTypeBindingSpecifier Identifier '=' Expression ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct VariableDeclaration {
    #[get = "pub"]
    pub(super) let_keyword: Keyword,
    #[get = "pub"]
    pub(super) mutable_keyword: Option<Keyword>,
    #[get = "pub"]
    pub(super) identifier: Identifier,
    #[get = "pub"]
    pub(super) type_annotation: Option<TypeAnnotation>,
    #[get = "pub"]
    pub(super) equals: Punctuation,
    #[get = "pub"]
    pub(super) expression: Expression,
    #[get = "pub"]
    pub(super) semicolon: Punctuation,
}

impl SourceElement for VariableDeclaration {
    fn span(&self) -> Span {
        self.let_keyword.span().join(&self.semicolon.span).unwrap()
    }
}

/// Represents an expressive statement syntax tree node
///
/// Expressive statements are statements that interest in the side effects of evaluating an
/// expression.
///
/// Syntax Synopsis:
/// ``` text
/// Expressive:
///     Semi
///     | Imperative
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum Expressive {
    Semi(Semi),
    Imperative(Imperative),
}

impl SourceElement for Expressive {
    fn span(&self) -> Span {
        match self {
            Self::Semi(expression) => expression.span(),
            Self::Imperative(expression) => expression.span(),
        }
    }
}

/// Represents a semi statement syntax tree node
///
/// Semi statements are statements that are a functional expression followed by a semicolon.
///
/// Syntax Synopsis:
/// ``` text
/// Semi:
///     Functional ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct Semi {
    #[get = "pub"]
    pub(super) expression: Functional,
    #[get = "pub"]
    pub(super) semicolon: Punctuation,
}

impl SourceElement for Semi {
    fn span(&self) -> Span {
        self.expression.span().join(&self.semicolon.span).unwrap()
    }
}

impl<'a> Parser<'a> {
    /// Parses a [Statement]
    pub fn parse_statement(&mut self, handler: &impl ErrorHandler<Error>) -> Option<Statement> {
        match self.peek_significant_token() {
            // Handles variable declaration statements
            Some(Token::Keyword(let_keyword)) if let_keyword.keyword == KeywordKind::Let => {
                // eat let keyword
                self.next_token();

                let mutable_keyword = match self.peek_significant_token() {
                    Some(Token::Keyword(mutable_keyword))
                        if mutable_keyword.keyword == KeywordKind::Mutable =>
                    {
                        // eat mutable keyword
                        self.next_token();

                        Some(mutable_keyword)
                    }
                    _ => None,
                };

                // expect identifier
                let identifier = self.expect_identifier(handler)?;

                // parse optional type annotation
                let type_annotation = match self.peek_significant_token() {
                    Some(Token::Punctuation(colon)) if colon.punctuation == ':' => {
                        // eat colon
                        self.next_token();

                        let type_specifier = self.parse_type_specifier(handler)?;

                        Some(TypeAnnotation {
                            colon,
                            type_specifier,
                        })
                    }
                    _ => None,
                };

                // expect equals
                let equals = self.expect_punctuation('=', handler)?;

                // parse expression
                let expression = self.parse_expression(handler)?;

                // expect semi-colon
                let semicolon = self.expect_punctuation(';', handler)?;

                Some(Statement::Declarative(
                    VariableDeclaration {
                        let_keyword,
                        mutable_keyword,
                        identifier,
                        type_annotation,
                        equals,
                        expression,
                        semicolon,
                    }
                    .into(),
                ))
            }
            _ => {
                let expression = self.parse_expression(handler)?;

                match expression {
                    Expression::Functional(expression) => {
                        // expect semi-colon
                        let semicolon = self.expect_punctuation(';', handler)?;

                        Some(Statement::Expressive(Expressive::Semi(Semi {
                            expression,
                            semicolon,
                        })))
                    }
                    Expression::Imperative(expression) => {
                        Some(Statement::Expressive(Expressive::Imperative(expression)))
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests;
