//! Contains all definition of statement syntax trees.

#![allow(missing_docs)]

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_base::source_file::{SourceElement, Span};
use pernixc_lexical::token::{Keyword, KeywordKind, Punctuation};

use super::{
    expression::{Binary, Brace, Expression, Terminator},
    pattern::Irrefutable,
    r#type::Type,
    Parse,
};
use crate::parser::Syntax;

pub mod strategy;

/// Syntax Synopsis:
/// ``` txt
/// Statement:
///     VariableDeclaration
///     | Expressive
/// ```
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From,
)]
#[allow(missing_docs)]
pub enum Statement {
    VariableDeclaration(VariableDeclaration),
    Expressive(Expressive),
}

impl Parse for Statement {
    fn syntax() -> impl Syntax<Output = Self> {
        VariableDeclaration::syntax()
            .map(Statement::VariableDeclaration)
            .or_else(Expression::syntax().then_do(|parser, x, handler| {
                let semi_expression = match x {
                    Expression::Binary(binary) => {
                        SemiExpression::Binary(binary)
                    }
                    Expression::Terminator(terminator) => {
                        SemiExpression::Terminator(terminator)
                    }

                    Expression::Brace(brace) => {
                        return Ok(Statement::Expressive(Expressive::Brace(
                            brace,
                        )))
                    }
                };

                Ok(Statement::Expressive(Expressive::Semi(Semi {
                    expression: semi_expression,
                    semicolon: parser.parse(';', handler)?,
                })))
            }))
    }
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
    r#type: Type,
}

impl Parse for TypeAnnotation {
    fn syntax() -> impl Syntax<Output = Self> {
        (':', Type::syntax()).map(|(colon, r#type)| Self { colon, r#type })
    }
}

impl SourceElement for TypeAnnotation {
    fn span(&self) -> Span {
        self.colon.span().join(&self.r#type.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// VariableDeclaration:
///     'let' Irrefutable TypeAnnotation? = Expression ';'
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

impl Parse for VariableDeclaration {
    fn syntax() -> impl Syntax<Output = Self> {
        (
            KeywordKind::Let,
            Irrefutable::syntax(),
            TypeAnnotation::syntax().or_none(),
            '=',
            Expression::syntax(),
            ';',
        )
            .map(
                |(
                    let_keyword,
                    irrefutable_pattern,
                    type_annotation,
                    equals,
                    expression,
                    semicolon,
                )| {
                    Self {
                        let_keyword,
                        irrefutable_pattern,
                        type_annotation,
                        equals,
                        expression,
                        semicolon,
                    }
                },
            )
    }
}

impl SourceElement for VariableDeclaration {
    fn span(&self) -> Span {
        self.let_keyword.span().join(&self.semicolon.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Expressive:
///     Semi
///     | Imperative
///     ;
/// ```
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From,
)]
#[allow(missing_docs)]
pub enum Expressive {
    Semi(Semi),
    Brace(Brace),
}

impl SourceElement for Expressive {
    fn span(&self) -> Span {
        match self {
            Self::Semi(expression) => expression.span(),
            Self::Brace(expression) => expression.span(),
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
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From,
)]
#[allow(missing_docs)]
pub enum SemiExpression {
    Binary(Binary),
    Terminator(Terminator),
}

impl SourceElement for SemiExpression {
    fn span(&self) -> Span {
        match self {
            Self::Binary(expression) => expression.span(),
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
    fn span(&self) -> Span {
        self.expression.span().join(&self.semicolon.span).unwrap()
    }
}

#[cfg(test)]
mod test;
