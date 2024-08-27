//! An extra module for parsing JSON-like data.

use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_lexical::{
    token::{Keyword, KeywordKind, Punctuation, String, Token},
    token_stream::Delimiter,
};

use super::{
    expression::{Boolean, Numeric},
    ConnectedList,
};
use crate::{
    error::{Error, SyntaxKind},
    parser::{Parser, Reading},
};

/// Syntax Synopsis:
///
/// ``` txt
/// Array:
///     '[' (Value ("," Value)* ","? )? ']'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Array {
    #[get = "pub"]
    left_bracket: Punctuation,
    #[get = "pub"]
    values: Option<ConnectedList<Box<Value>, Punctuation>>,
    #[get = "pub"]
    right_bracket: Punctuation,
}

impl SourceElement for Array {
    fn span(&self) -> pernixc_base::source_file::Span {
        self.left_bracket.span().join(&self.right_bracket.span()).unwrap()
    }
}

impl Array {
    /// Dissolve the array into its components.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (
        Punctuation,
        Option<ConnectedList<Box<Value>, Punctuation>>,
        Punctuation,
    ) {
        (self.left_bracket, self.values, self.right_bracket)
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Map {
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    pairs: Option<ConnectedList<Box<Pair>, Punctuation>>,
    #[get = "pub"]
    right_brace: Punctuation,
}

impl SourceElement for Map {
    fn span(&self) -> pernixc_base::source_file::Span {
        self.left_brace.span().join(&self.right_brace.span()).unwrap()
    }
}

impl Map {
    /// Dissolve the map into its components.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (Punctuation, Option<ConnectedList<Box<Pair>, Punctuation>>, Punctuation)
    {
        (self.left_brace, self.pairs, self.right_brace)
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

impl<'a> Parser<'a> {
    /// Parse a JSON value.
    ///
    /// This parsing function is not 100% accurate syntax-wise, for example, it
    /// allows trailing commas in arrays and maps. However, it is good enough
    /// for parsing JSON-like data.
    pub fn parse_json_value(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<Value> {
        match self.stop_at_significant() {
            // parse null
            Reading::Unit(Token::Keyword(
                null_keyword @ Keyword { kind: KeywordKind::Null, .. },
            )) => {
                // eat the null keyword
                self.forward();

                Some(Value::Null(null_keyword))
            }

            // parse boolean
            Reading::Unit(Token::Keyword(
                boolean_keyword @ Keyword {
                    kind: KeywordKind::True | KeywordKind::False,
                    ..
                },
            )) => {
                // eat the boolean keyword
                self.forward();

                Some(Value::Boolean(match boolean_keyword.kind {
                    KeywordKind::True => Boolean::True(boolean_keyword),
                    KeywordKind::False => Boolean::False(boolean_keyword),
                    _ => unreachable!(),
                }))
            }

            // parse number
            Reading::Unit(Token::Numeric(_)) => {
                // eat the numeric token
                self.forward();

                Some(Value::Numeric(self.parse_numeric_literal()?))
            }

            // parse string
            Reading::Unit(Token::String(string)) => {
                // eat the string token
                self.forward();

                Some(Value::String(string))
            }

            // parse array
            Reading::IntoDelimited(Delimiter::Bracket, _) => {
                let list = self.parse_delimited_list(
                    Delimiter::Brace,
                    ',',
                    |parser| parser.parse_json_value(handler).map(Box::new),
                    handler,
                )?;

                Some(Value::Array(Array {
                    left_bracket: list.open,
                    values: list.list,
                    right_bracket: list.close,
                }))
            }

            // parse map
            Reading::IntoDelimited(Delimiter::Brace, _) => {
                let list = self.parse_delimited_list(
                    Delimiter::Brace,
                    ',',
                    |parser| {
                        let key = parser.parse_string(handler)?;
                        let colon =
                            parser.parse_punctuation(':', true, handler)?;
                        let value =
                            parser.parse_json_value(handler).map(Box::new)?;

                        Some(Box::new(Pair { key, colon, value }))
                    },
                    handler,
                )?;

                Some(Value::Map(Map {
                    left_brace: list.open,
                    pairs: list.list,
                    right_brace: list.close,
                }))
            }

            // unexpected token
            found => {
                handler.receive(Error {
                    expected: SyntaxKind::JsonValue,
                    alternatives: vec![],
                    found: self.reading_to_found(found),
                });

                None
            }
        }
    }
}
