//! Contains the definitions of statement syntax tree.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_lexical::token::{Identifier, Keyword, KeywordKind, Punctuation, Token};
use pernixc_source::{SourceElement, Span, SpanError};
use pernixc_system::diagnostic::{Dummy, Handler};

use super::{
    expression::{Expression, Functional, Imperative, Terminator},
    TypeAnnotation,
};
use crate::{
    error::Error,
    parser::{Parser, Result as ParserResult},
};

pub mod input;

/// Represents a statement syntax tree node
///
/// Syntax Synopsis:
/// ``` text
/// Statement:
///     VariableDeclaration
///     | Expressive
/// ```
#[derive(Debug, Clone, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Statement {
    VariableDeclaration(VariableDeclaration),
    Expressive(Expressive),
}

impl SourceElement for Statement {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::VariableDeclaration(declaration) => declaration.span(),
            Self::Expressive(expression) => expression.span(),
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
#[derive(Debug, Clone)]
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
    fn span(&self) -> Result<Span, SpanError> {
        self.let_keyword.span()?.join(&self.semicolon.span)
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
#[derive(Debug, Clone, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Expressive {
    Semi(Semi),
    Imperative(Imperative),
}

impl SourceElement for Expressive {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::Semi(expression) => expression.span(),
            Self::Imperative(expression) => expression.span(),
        }
    }
}

/// Is an enumeration of all expressions that can be used in [`Semi`] statement.
///
/// Syntax Synopsis:
/// ``` text
/// SemiExpression:
///     Functional
///     | Terminator
///     ;
/// ```
#[derive(Debug, Clone, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum SemiExpression {
    Functional(Functional),
    Terminator(Terminator),
}

impl SourceElement for SemiExpression {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::Functional(expression) => expression.span(),
            Self::Terminator(expression) => expression.span(),
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
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Semi {
    pub expression: SemiExpression,
    pub semicolon: Punctuation,
}

impl SourceElement for Semi {
    fn span(&self) -> Result<Span, SpanError> { self.expression.span()?.join(&self.semicolon.span) }
}

impl<'a> Parser<'a> {
    /// Parses a [`Statement`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_statement(&mut self, handler: &impl Handler<Error>) -> ParserResult<Statement> {
        if matches!(self.stop_at_significant(), Some(Token::Keyword(k)) if k.keyword == KeywordKind::Let)
        {
            self.parse_variable_declaration(handler)
                .map(Statement::VariableDeclaration)
        } else {
            let semi_expression = match self.parse_expression(handler)? {
                Expression::Imperative(imperative) => {
                    return Ok(Statement::Expressive(Expressive::Imperative(imperative)));
                }
                Expression::Functional(functional) => SemiExpression::Functional(functional),
                Expression::Terminator(terminator) => SemiExpression::Terminator(terminator),
            };

            let semicolon = self.parse_punctuation(';', true, handler)?;
            Ok(Statement::Expressive(Expressive::Semi(Semi {
                expression: semi_expression,
                semicolon,
            })))
        }
    }

    /// Parses a [`VariableDeclaration`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_variable_declaration(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<VariableDeclaration> {
        let let_keyword = self.parse_keyword(KeywordKind::Let, handler)?;

        // parse optional mutable binding
        let mutable_keyword = self
            .try_parse(|parser| parser.parse_keyword(KeywordKind::Mutable, &Dummy))
            .ok();

        let identifier = self.parse_identifier(handler)?;

        // parse optional type annotation
        let type_annotation = if matches!(self.stop_at_significant(), Some(Token::Punctuation(p)) if p.punctuation == ':')
        {
            Some(self.parse_type_annotation(handler)?)
        } else {
            None
        };

        let equals = self.parse_punctuation('=', true, handler)?;
        let expression = self.parse_expression(handler)?;
        let semicolon = self.parse_punctuation(';', true, handler)?;

        Ok(VariableDeclaration {
            let_keyword,
            mutable_keyword,
            identifier,
            type_annotation,
            equals,
            expression,
            semicolon,
        })
    }
}

#[cfg(test)]
mod tests;
