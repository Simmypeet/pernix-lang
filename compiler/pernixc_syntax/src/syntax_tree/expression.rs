//! Contains all definition of expression syntax trees.

#![allow(missing_docs)]

use binary::Binary;
use enum_as_inner::EnumAsInner;
use pernixc_handler::Handler;
use pernixc_source_file::{SourceElement, Span};
use terminator::Terminator;

use super::{Parse, SyntaxTree};
use crate::{
    error,
    state_machine::{
        parse::{self, Branch},
        StateMachine,
    },
};

pub mod binary;
pub mod block;
pub mod postfix;
pub mod prefix;
pub mod terminator;
pub mod unit;

pub mod strategy;

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
