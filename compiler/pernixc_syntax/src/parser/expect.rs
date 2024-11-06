//! Contains various data structures used for representing the parser's
//! token expectations.

use pernixc_lexical::token;

use super::Reading;
use crate::error::SyntaxKind;

/// Used for converting the [`Reading`] into the expected token.
pub trait Expect: Into<SyntaxKind> {
    /// The expected token output;
    type Output;

    /// Converts the [`Reading`] into the expected token.
    ///
    /// # Errors
    ///
    /// Returns `Err` with original [`Reading`] if the expected token is not
    /// found.
    fn expect(&self, reading: Reading) -> Result<Self::Output, Reading>;
}

/// Expects the [`Reading`] to be an [`token::Identifier`] token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier;

impl From<Identifier> for SyntaxKind {
    fn from(_: Identifier) -> Self { SyntaxKind::Identifier }
}

impl Expect for Identifier {
    type Output = token::Identifier;

    fn expect(&self, reading: Reading) -> Result<Self::Output, Reading> {
        match reading {
            Reading::Unit(token::Token::Identifier(ident)) => Ok(ident),
            found => Err(found),
        }
    }
}

/// Expects the [`Reading`] to be a [`char`] punctuation token.
///
/// In case of '(' '[' '{', the [`Reading`] is expected to be
/// [`Reading::IntoDelimited`].
///
/// In case of ')' ']' '}', the [`Reading`] is expected to be
/// [`Reading::DelimitedEnd`].
pub type Punctuation = char;

impl From<char> for SyntaxKind {
    fn from(c: char) -> Self { SyntaxKind::Punctuation(c) }
}

impl Expect for char {
    type Output = token::Punctuation;

    fn expect(&self, reading: Reading) -> Result<Self::Output, Reading> {
        match *self {
            '{' | '(' | '[' => match reading {
                Reading::IntoDelimited(_, punc) => Ok(punc),
                found => Err(found),
            },
            '}' | ')' | ']' => match reading {
                Reading::DelimitedEnd(_, punc) => Ok(punc),
                found => Err(found),
            },
            _ => match reading {
                Reading::Unit(token::Token::Punctuation(punc))
                    if punc.punctuation == *self =>
                {
                    Ok(punc)
                }
                found => Err(found),
            },
        }
    }
}

/// Expects the [`Reading`] to be a [`Keyword`] token of the specific
/// kind.
pub type Keyword = pernixc_lexical::token::KeywordKind;

impl From<Keyword> for SyntaxKind {
    fn from(k: Keyword) -> Self { SyntaxKind::Keyword(k) }
}

impl Expect for Keyword {
    type Output = token::Keyword;

    fn expect(&self, reading: Reading) -> Result<Self::Output, Reading> {
        match reading {
            Reading::Unit(token::Token::Keyword(keyword))
                if keyword.kind == *self =>
            {
                Ok(keyword)
            }
            found => Err(found),
        }
    }
}

/// Expects the [`Reading`] to be a [`Numeric`] literal token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Numeric;

impl From<Numeric> for SyntaxKind {
    fn from(_: Numeric) -> Self { SyntaxKind::Numeric }
}

impl Expect for Numeric {
    type Output = token::Numeric;

    fn expect(&self, reading: Reading) -> Result<Self::Output, Reading> {
        match reading {
            Reading::Unit(token::Token::Numeric(numeric)) => Ok(numeric),
            found => Err(found),
        }
    }
}

/// Expects the [`Reading`] to be a [`token::String`] literal token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct String;

impl From<String> for SyntaxKind {
    fn from(_: String) -> Self { SyntaxKind::String }
}

impl Expect for String {
    type Output = token::String;

    fn expect(&self, reading: Reading) -> Result<Self::Output, Reading> {
        match reading {
            Reading::Unit(token::Token::String(string)) => Ok(string),
            found => Err(found),
        }
    }
}

/// Expects the [`Reading`] to be a [`token::Character`] literal token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Character;

impl From<Character> for SyntaxKind {
    fn from(_: Character) -> Self { SyntaxKind::Character }
}

impl Expect for Character {
    type Output = token::Character;

    fn expect(&self, reading: Reading) -> Result<Self::Output, Reading> {
        match reading {
            Reading::Unit(token::Token::Character(character)) => Ok(character),
            found => Err(found),
        }
    }
}

/// Expects the [`Reading::IntoDelimited`]` with the given delimiter.
pub type IntoDelimited = super::Delimiter;

impl From<IntoDelimited> for SyntaxKind {
    fn from(delimiter: IntoDelimited) -> Self {
        match delimiter {
            super::Delimiter::Parenthesis => SyntaxKind::Punctuation('('),
            super::Delimiter::Brace => SyntaxKind::Punctuation('{'),
            super::Delimiter::Bracket => SyntaxKind::Punctuation('['),
        }
    }
}

impl Expect for IntoDelimited {
    type Output = token::Punctuation;

    fn expect(&self, reading: Reading) -> Result<Self::Output, Reading> {
        match reading {
            Reading::IntoDelimited(delimiter, punc) if delimiter == *self => {
                Ok(punc)
            }
            found => Err(found),
        }
    }
}
