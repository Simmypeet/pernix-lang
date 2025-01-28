//! Contains all definition of statement syntax trees.

#![allow(missing_docs)]

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_handler::Handler;
use pernixc_lexical::{
    token::{Keyword, KeywordKind, Punctuation},
    token_stream::Delimiter,
};
use pernixc_source_file::{SourceElement, Span};

use super::{
    expression::{Brace, Expression},
    pattern::Irrefutable,
    r#type::Type,
    EnclosedTree, Parse, ParseExt, SyntaxTree,
};
use crate::{
    error,
    state_machine::{
        parse::{self, Branch},
        StateMachine,
    },
};

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

impl SyntaxTree for Statement {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            VariableDeclaration::parse.map(Self::VariableDeclaration),
            |state_machine: &mut StateMachine<'_>,
             handler: &dyn Handler<error::Error>| {
                let expression =
                    Expression::parse.parse(state_machine, handler)?;

                match expression {
                    Expression::Binary(binary)
                        if binary.first().is_brace()
                            && binary.chain().is_empty() =>
                    {
                        Ok(Self::Expressive(Expressive::Brace(
                            binary.destruct().0.into_brace().unwrap(),
                        )))
                    }

                    expression => {
                        Ok(Self::Expressive(Expressive::Semi(Semi {
                            expression,
                            semicolon: ';'
                                .to_owned()
                                .parse(state_machine, handler)?,
                        })))
                    }
                }
            },
        )
            .branch()
            .parse(state_machine, handler)
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

impl SyntaxTree for TypeAnnotation {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (':'.to_owned(), Type::parse)
            .map(|(colon, r#type)| Self { colon, r#type })
            .parse(state_machine, handler)
    }
}

impl SourceElement for TypeAnnotation {
    fn span(&self) -> Span { self.colon.span().join(&self.r#type.span()) }
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

impl SyntaxTree for VariableDeclaration {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Let.to_owned(),
            Irrefutable::parse,
            TypeAnnotation::parse.or_none(),
            '='.to_owned(),
            Expression::parse,
            ';'.to_owned(),
        )
            .map(
                |(
                    let_keyword,
                    irrefutable_pattern,
                    type_annotation,
                    equals,
                    expression,
                    semicolon,
                )| Self {
                    let_keyword,
                    irrefutable_pattern,
                    type_annotation,
                    equals,
                    expression,
                    semicolon,
                },
            )
            .parse(state_machine, handler)
    }
}

impl SourceElement for VariableDeclaration {
    fn span(&self) -> Span {
        self.let_keyword.span().join(&self.semicolon.span)
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
/// Semi:
///     Functional ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[allow(missing_docs)]
pub struct Semi {
    #[get = "pub"]
    expression: Expression,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for Semi {
    fn span(&self) -> Span { self.expression.span().join(&self.semicolon.span) }
}

/// Syntax Synopsis:
/// ``` txt
/// Statements:
///     '{' Statement* '}'
///     ;
/// ```
pub type Statements = EnclosedTree<Vec<Statement>>;

impl SyntaxTree for Statements {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Statement::parse
            .keep_take_all()
            .enclosed_tree(Delimiter::Brace)
            .parse(state_machine, handler)
    }
}

#[cfg(test)]
mod test;
