//! An extra module for parsing JSON-like data.

use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_lexical::{
    token::{Keyword, KeywordKind, Punctuation, String},
    token_stream::Delimiter,
};

use super::{
    expression::{Boolean, Numeric},
    EnclosedConnectedList, ParseExt, SyntaxTree,
};
use crate::{
    error, expect,
    state_machine::{
        parse::{self, Parse},
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
            .enclosed_connected_list(','.to_owned(), Delimiter::Bracket)
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
        (expect::String.to_owned(), ';'.to_owned(), Value::parse.map(Box::new))
            .map(|(key, colon, value)| Self { key, colon, value })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Pair {
    fn span(&self) -> pernixc_base::source_file::Span {
        self.key.span().join(&self.value.span()).unwrap()
    }
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
            .enclosed_connected_list(','.to_owned(), Delimiter::Brace)
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
        KeywordKind::Null
            .to_owned()
            .map(Self::Null)
            .or_else(Boolean::parse.map(Self::Boolean))
            .or_else(Numeric::parse.map(Self::Numeric))
            .or_else(expect::String.to_owned().map(Self::String))
            .or_else(Array::parse.map(Self::Array))
            .or_else(Map::parse.map(Self::Map))
            .parse(state_machine, handler)
    }
}

impl SourceElement for Value {
    fn span(&self) -> pernixc_base::source_file::Span {
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
