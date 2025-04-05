//! An extra module for parsing JSON-like data.

use enum_as_inner::EnumAsInner;
use pernixc_handler::Handler;
use pernixc_lexical::{
    token::{Keyword, KeywordKind, Punctuation, String},
    token_stream::DelimiterKind,
};
use pernixc_source_file::{SourceElement, GlobalSpan};

use super::{
    expression::unit::{Boolean, Numeric},
    EnclosedConnectedList, ParseExt, SyntaxTree,
};
use crate::{
    error, expect,
    state_machine::{
        parse::{self, Branch, Parse},
        StateMachine,
    },
};

pub type Array = EnclosedConnectedList<Box<Value>, Punctuation>;

impl SyntaxTree for Array {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Value::parse
            .map(Box::new)
            .enclosed_connected_list(','.to_owned(), DelimiterKind::Bracket)
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pair {
    pub key: String,
    pub colon: Punctuation,
    pub value: Box<Value>,
}

impl SyntaxTree for Pair {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (expect::String.to_owned(), ':'.to_owned(), Value::parse.map(Box::new))
            .map(|(key, colon, value)| Self { key, colon, value })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Pair {
    fn span(&self) -> GlobalSpan { self.key.span().join(&self.value.span()) }
}

pub type Map = EnclosedConnectedList<Pair, Punctuation>;

impl SyntaxTree for Map {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Pair::parse
            .enclosed_connected_list(','.to_owned(), DelimiterKind::Brace)
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Value {
    Null(Keyword),
    Boolean(Boolean),
    Numeric(Numeric),
    String(String),
    Array(Array),
    Map(Map),
}

impl SyntaxTree for Value {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Null.to_owned().map(Self::Null),
            Boolean::parse.map(Self::Boolean),
            Numeric::parse.map(Self::Numeric),
            expect::String.to_owned().map(Self::String),
            Array::parse.map(Self::Array),
            Map::parse.map(Self::Map),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for Value {
    fn span(&self) -> GlobalSpan {
        match self {
            Self::Null(kw) => kw.span(),
            Self::Boolean(b) => b.span(),
            Self::Numeric(n) => n.span(),
            Self::String(s) => s.span(),
            Self::Array(a) => a.span(),
            Self::Map(m) => m.span(),
        }
    }
}
