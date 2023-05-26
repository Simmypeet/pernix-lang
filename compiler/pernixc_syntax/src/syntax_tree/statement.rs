use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_lexical::token::{Identifier, Keyword, KeywordKind, Punctuation, Token};
use pernixc_source::{SourceElement, Span, SpanError};
use pernixc_system::diagnostic::{Dummy, Handler};

use super::{
    expression::{Expression, Functional, Imperative},
    TypeAnnotation,
};
use crate::{
    error::Error,
    parser::{Parser, Result as ParserResult},
};

pub mod strategy;

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
    fn span(&self) -> Result<Span, SpanError> {
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
    fn span(&self) -> Result<Span, SpanError> {
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Semi {
    pub expression: Functional,
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
                .map(|x| Statement::Declarative(Declarative::VariableDeclaration(x)))
        } else {
            Ok(Statement::Expressive(
                match self.parse_expression(handler)? {
                    Expression::Functional(expression) => {
                        let semicolon = self.parse_punctuation(';', true, handler)?;
                        Expressive::Semi(Semi {
                            expression,
                            semicolon,
                        })
                    }
                    Expression::Imperative(expression) => Expressive::Imperative(expression),
                },
            ))
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
