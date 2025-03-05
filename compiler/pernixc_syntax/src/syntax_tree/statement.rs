//! Contains all definition of statement syntax trees.

#![allow(missing_docs)]

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_handler::Handler;
use pernixc_lexical::{
    token::{Keyword, KeywordKind, Punctuation},
    token_stream::DelimiterKind,
};
use pernixc_source_file::{SourceElement, Span};

use super::{
    expression::{brace::Brace, Expression},
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
                        if binary.first.is_brace()
                            && binary.chain.is_empty() =>
                    {
                        Ok(Self::Expressive(Expressive::Brace(
                            binary.first.into_brace().unwrap(),
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct TypeAnnotation {
    pub colon: Punctuation,
    pub r#type: Type,
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct VariableDeclaration {
    pub let_keyword: Keyword,
    pub irrefutable_pattern: Irrefutable,
    pub type_annotation: Option<TypeAnnotation>,
    pub equals: Punctuation,
    pub expression: Expression,
    pub semicolon: Punctuation,
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Semi {
    pub expression: Expression,
    pub semicolon: Punctuation,
}

impl SourceElement for Semi {
    fn span(&self) -> Span { self.expression.span().join(&self.semicolon.span) }
}

pub type Statements = EnclosedTree<Vec<Statement>>;

impl SyntaxTree for Statements {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Statement::parse
            .keep_take_all()
            .enclosed_tree(DelimiterKind::Brace)
            .parse(state_machine, handler)
    }
}
