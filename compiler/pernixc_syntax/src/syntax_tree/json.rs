//! An extra module for parsing JSON-like data.

use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_handler::Handler;
use pernixc_lexical::{
    token::{Keyword, KeywordKind, Punctuation, String},
    token_stream::DelimiterKind,
};
use pernixc_source_file::{SourceElement, Span};

use super::{
    expression::{Boolean, Numeric},
    EnclosedConnectedList, ParseExt, SyntaxTree,
};
use crate::{
    error, expect,
    state_machine::{
        parse::{self, Branch, Parse},
        StateMachine,
    },
};

/// Syntax Synopsis:
///
/// ``` txt
/// Array:
///     '[' (Value ("," Value)* ","? )? ']'
///     ;
/// ```
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

/// Syntax Synopsis:
///
/// ```txt
/// Pair:
///     String ':' Value
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Pair {
    #[get = "pub"]
    key: String,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    value: Box<Value>,
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
    fn span(&self) -> Span { self.key.span().join(&self.value.span()) }
}

impl Pair {
    /// Dissolve the pair into its components.
    #[must_use]
    pub fn dissolve(self) -> (String, Punctuation, Box<Value>) {
        (self.key, self.colon, self.value)
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Map:
///     '{' (Pair ("," Pair)* ","?)? '}' ;
///     ;
/// ```
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

/// Syntax Synopsis:
///
/// ```txt
/// Value:
///     Null
///     | Boolean
///     | Numeric
///     | String
///     | Array
///     | Map
///     ;
/// ```
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
    fn span(&self) -> Span {
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
