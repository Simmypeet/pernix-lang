//! Contains all definition of statement syntax trees.

#![allow(missing_docs)]

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_handler::Handler;
use pernixc_lexical::token::{Keyword, KeywordKind, Punctuation};
use pernixc_source_file::{SourceElement, Span};

use super::{
    expression::Expression, pattern::Irrefutable, r#type::Type, Parse,
    SyntaxTree,
};
use crate::{
    error,
    state_machine::{
        parse::{self, Branch, Passable},
        StateMachine,
    },
};

pub mod strategy;

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From,
)]
#[allow(missing_docs)]
pub enum Statement {
    VariableDeclaration(VariableDeclaration),
    Expression(Expression),
}

impl SyntaxTree for Statement {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            VariableDeclaration::parse.map(Self::VariableDeclaration),
            Expression::parse.map(Self::Expression),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for Statement {
    fn span(&self) -> Span {
        match self {
            Self::VariableDeclaration(declaration) => declaration.span(),
            Self::Expression(expression) => expression.span(),
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
        )
            .map(
                |(
                    let_keyword,
                    irrefutable_pattern,
                    type_annotation,
                    equals,
                    expression,
                )| Self {
                    let_keyword,
                    irrefutable_pattern,
                    type_annotation,
                    equals,
                    expression,
                },
            )
            .parse(state_machine, handler)
    }
}

impl SourceElement for VariableDeclaration {
    fn span(&self) -> Span {
        self.let_keyword.span().join(&self.expression.span())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Statements {
    pub colon: Punctuation,
    pub statements: Vec<Passable<Statement>>,
}

impl SyntaxTree for Statements {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Statement::parse
            .indentation_item()
            .keep_take_all()
            .step_into_indentation()
            .map(|(colon, statements)| Self {
                colon: colon.clone(),
                statements,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Statements {
    fn span(&self) -> Span {
        let begin = self.colon.span();

        begin.join(
            &self
                .statements
                .last()
                .map_or_else(|| begin.clone(), SourceElement::span),
        )
    }
}
