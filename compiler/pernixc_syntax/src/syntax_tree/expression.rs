//! Contains all definition of expression syntax trees.

#![allow(missing_docs)]

use binary::Binary;
use enum_as_inner::EnumAsInner;
use pernixc_handler::Handler;
use pernixc_lexical::token::Punctuation;
use pernixc_source_file::{SourceElement, Span};
use terminator::Terminator;

use super::{Label, Parse, SyntaxTree};
use crate::{
    error,
    state_machine::{
        parse::{self, Branch},
        StateMachine,
    },
};

pub mod binary;
pub mod brace;
pub mod postfix;
pub mod prefix;
pub mod terminator;
pub mod unit;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Expression {
    Binary(Binary),
    Terminator(Terminator),
}

impl SyntaxTree for Expression {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            Binary::parse.map(Self::Binary),
            Terminator::parse.map(Self::Terminator),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for Expression {
    fn span(&self) -> Span {
        match self {
            Self::Binary(syn) => syn.span(),
            Self::Terminator(syn) => syn.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LabelSpecifier {
    pub label: Label,
    pub colon: Punctuation,
}

impl SyntaxTree for LabelSpecifier {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (Label::parse, ':'.to_owned())
            .map(|(label, colon)| Self { label, colon })
            .parse(state_machine, handler)
    }
}

impl SourceElement for LabelSpecifier {
    fn span(&self) -> Span { self.label.span().join(&self.colon.span) }
}
