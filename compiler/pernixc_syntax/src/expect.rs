//! Contains the [`Expect`] trait and its implementations for expecting certain
//! tokens from a token-stream/state-machine.

use pernixc_lexical::{
    token::{self, KeywordKind, Token},
    token_stream::{self, DelimiterKind, TokenKind},
};

/// Expecting the [`TokenKind`] to be an [`token::Identifier`] token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Identifier;

/// Expecting the [`TokenKind`] to be a [`token::Numeric`] literal token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Numeric;

/// Expecting the [`TokenKind`] to be a [`token::String`] literal token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct String;

/// Expecting the [`TokenKind`] to be a [`token::Character`] literal token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Character;

/// An enumeration of all possible expected tokens.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From,
)]
pub enum Expected {
    /// Expecting an identifier token.
    Identifier(Identifier),

    /// Expecting a numeric literal token.
    Numeric(Numeric),

    /// Expecting a string literal token.
    String(String),

    /// Expecting a character literal token.
    Character(Character),

    /// Expecting a punctuation token.
    Punctuation(char),

    /// Expecting a keyword token.
    Keyword(KeywordKind),

    /// Expecting a [`TokenKind::Delimited`] token.
    Delimited(DelimiterKind),
}

/// A trait for expecting a certain [`TokenKind`] from a token stream.
pub trait Expect: Into<Expected> {
    /// The output type that is expected from the token stream.
    type Output;

    /// Checks if the [`TokenKind`] is the expected token.
    fn expect<'a>(&self, token: &'a TokenKind) -> Option<&'a Self::Output>;
}

impl Expect for Identifier {
    type Output = token::Identifier;

    fn expect<'a>(&self, token: &'a TokenKind) -> Option<&'a Self::Output> {
        if let TokenKind::Token(Token::Identifier(ident)) = token {
            Some(ident)
        } else {
            None
        }
    }
}

impl Expect for Numeric {
    type Output = token::Numeric;

    fn expect<'a>(&self, token: &'a TokenKind) -> Option<&'a Self::Output> {
        if let TokenKind::Token(Token::Numeric(ident)) = token {
            Some(ident)
        } else {
            None
        }
    }
}

impl Expect for String {
    type Output = token::String;

    fn expect<'a>(&self, token: &'a TokenKind) -> Option<&'a Self::Output> {
        if let TokenKind::Token(Token::String(ident)) = token {
            Some(ident)
        } else {
            None
        }
    }
}

impl Expect for Character {
    type Output = token::Character;

    fn expect<'a>(&self, token: &'a TokenKind) -> Option<&'a Self::Output> {
        if let TokenKind::Token(Token::Character(ident)) = token {
            Some(ident)
        } else {
            None
        }
    }
}

impl Expect for char {
    type Output = token::Punctuation;

    fn expect<'a>(&self, token: &'a TokenKind) -> Option<&'a Self::Output> {
        if let TokenKind::Token(Token::Punctuation(punc)) = token {
            if punc.punctuation == *self {
                Some(punc)
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl Expect for KeywordKind {
    type Output = token::Keyword;

    fn expect<'a>(&self, token: &'a TokenKind) -> Option<&'a Self::Output> {
        if let TokenKind::Token(Token::Keyword(keyword)) = token {
            if keyword.kind == *self {
                Some(keyword)
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl Expect for DelimiterKind {
    type Output = token_stream::Fragment;

    fn expect<'a>(&self, token: &'a TokenKind) -> Option<&'a Self::Output> {
        if let TokenKind::Fragment(fragment) = token {
            if fragment
                .kind
                .as_delimiter()
                .is_some_and(|x| x.delimiter == *self)
            {
                Some(fragment)
            } else {
                None
            }
        } else {
            None
        }
    }
}
