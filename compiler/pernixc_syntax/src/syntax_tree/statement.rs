//! Contains the syntax tree nodes for statements

use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_common::source_file::Span;
use pernixc_lexical::token::{Identifier, Keyword, KeywordKind, Punctuation, Token};

use super::{
    expression::{Expression, Functional, Imperative},
    SourceElement, TypeAnnotation,
};
use crate::parser::Parser;

/// Represents a statement syntax tree node
///
/// Syntax Synopsis:
/// ``` text
/// Statement:
///     Declarative
///     | Expressive
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct VariableDeclaration {
    pub let_keyword: Keyword,
    pub mutable_keyword: Option<Keyword>,
    pub identifier: Identifier,
    pub type_annotation: Option<TypeAnnotation>,
    pub equals: Punctuation,
    pub expression: Expression,
    pub semicolon: Punctuation,
}

impl SourceElement for VariableDeclaration {
    fn span(&self) -> Span {
        Span {
            start: self.let_keyword.span.start,
            end: self.semicolon.span.end,
        }
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Semi {
    pub expression: Functional,
    pub semicolon: Punctuation,
}

impl SourceElement for Semi {
    fn span(&self) -> Span {
        Span {
            start: self.expression.span().start,
            end: self.semicolon.span.end,
        }
    }
}

impl<'a> Parser<'a> {
    /// Parses a [Statement]
    pub fn parse_statement(&mut self) -> Option<Statement> {
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

                        Some(*mutable_keyword)
                    }
                    _ => None,
                };

                // expect identifier
                let identifier = self.expect_identifier()?;

                // parse optional type annotation
                let type_annotation = match self.peek_significant_token() {
                    Some(Token::Punctuation(punctuation)) if punctuation.punctuation == ':' => {
                        // eat colon
                        self.next_token();

                        let type_specifier = self.parse_type_specifier()?;

                        Some(TypeAnnotation {
                            colon: *punctuation,
                            type_specifier,
                        })
                    }
                    _ => None,
                };

                // expect equals
                let equals = self.expect_punctuation('=')?;

                // parse expression
                let expression = self.parse_expression()?;

                // expect semi-colon
                let semicolon = self.expect_punctuation(';')?;

                Some(Statement::Declarative(
                    VariableDeclaration {
                        let_keyword: *let_keyword,
                        mutable_keyword,
                        identifier: *identifier,
                        type_annotation,
                        equals: *equals,
                        expression,
                        semicolon: *semicolon,
                    }
                    .into(),
                ))
            }
            _ => {
                let expression = self.parse_expression()?;

                match expression {
                    Expression::Functional(expression) => {
                        // expect semi-colon
                        let semicolon = self.expect_punctuation(';')?;

                        Some(Statement::Expressive(Expressive::Semi(Semi {
                            expression,
                            semicolon: *semicolon,
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
