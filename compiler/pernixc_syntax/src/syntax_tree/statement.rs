use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_lexical::token::{Keyword, KeywordKind, Punctuation, Token};
use pernixc_source::{SourceElement, Span};
use pernixc_system::diagnostic::Handler;

use super::{
    expression::{Expression, Functional, Imperative, Terminator},
    pattern::Irrefutable,
    ty::Type,
};
use crate::{error::Error, parser::Parser};

/// Syntax Synopsis:
/// ``` txt
/// Statement:
///     VariableDeclaration
///     | Expressive
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Statement {
    VariableDeclaration(VariableDeclaration),
    Expressive(Expressive),
}

impl SourceElement for Statement {
    fn span(&self) -> Span {
        match self {
            Self::VariableDeclaration(declaration) => declaration.span(),
            Self::Expressive(expression) => expression.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// TypeAnnotation:
///     ':' Type
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct TypeAnnotation {
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    ty: Type,
}

impl SourceElement for TypeAnnotation {
    fn span(&self) -> Span { self.colon.span().join(&self.ty.span()).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// VariableDeclaration:
///     'let' Irrefutable TypeAnnotation? '=' Expression ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[allow(missing_docs)]
pub struct VariableDeclaration {
    #[get = "pub"]
    let_keyword: Keyword,
    #[get = "pub"]
    irrefutable_pattern: Irrefutable,
    #[get = "pub"]
    type_annotation: Option<TypeAnnotation>,
    #[get = "pub"]
    equals: Punctuation,
    #[get = "pub"]
    expression: Expression,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for VariableDeclaration {
    fn span(&self) -> Span { self.let_keyword.span().join(&self.semicolon.span).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
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

/// Syntax Synopsis:
/// ``` txt
/// SemiExpression:
///     Functional
///     | Terminator
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum SemiExpression {
    Functional(Functional),
    Terminator(Terminator),
}

impl SourceElement for SemiExpression {
    fn span(&self) -> Span {
        match self {
            Self::Functional(expression) => expression.span(),
            Self::Terminator(expression) => expression.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Semi:
///     Functional ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[allow(missing_docs)]
pub struct Semi {
    #[get = "pub"]
    expression: SemiExpression,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for Semi {
    fn span(&self) -> Span { self.expression.span().join(&self.semicolon.span).unwrap() }
}

impl<'a> Parser<'a> {
    /// Parses a [`Statement`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_statement(&mut self, handler: &impl Handler<Error>) -> Option<Statement> {
        if matches!(self.stop_at_significant(), Some(Token::Keyword(k)) if k.keyword == KeywordKind::Let)
        {
            self.parse_variable_declaration(handler)
                .map(Statement::VariableDeclaration)
        } else {
            let semi_expression = match self.parse_expression(handler)? {
                Expression::Imperative(imperative) => {
                    return Some(Statement::Expressive(Expressive::Imperative(imperative)));
                }
                Expression::Functional(functional) => SemiExpression::Functional(functional),
                Expression::Terminator(terminator) => SemiExpression::Terminator(terminator),
            };

            let semicolon = self.parse_punctuation(';', true, handler)?;
            Some(Statement::Expressive(Expressive::Semi(Semi {
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
    ) -> Option<VariableDeclaration> {
        let let_keyword = self.parse_keyword(KeywordKind::Let, handler)?;

        let irrefutable_pattern = self.parse_irrefutable_pattern(handler)?;

        // parse optional type annotation
        let type_annotation = match self.stop_at_significant() {
            Some(Token::Punctuation(colon)) if colon.punctuation == ':' => {
                self.forward();
                let ty = self.parse_type(handler)?;
                Some(TypeAnnotation { colon, ty })
            }
            _ => None,
        };

        let equals = self.parse_punctuation('=', true, handler)?;
        let expression = self.parse_expression(handler)?;
        let semicolon = self.parse_punctuation(';', true, handler)?;

        Some(VariableDeclaration {
            let_keyword,
            irrefutable_pattern,
            type_annotation,
            equals,
            expression,
            semicolon,
        })
    }
}

#[cfg(test)]
pub(super) mod tests;
