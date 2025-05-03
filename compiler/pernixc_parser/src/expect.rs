//! contains the definition of [`Expect`] trait and its implementations for
//! various token types.

use pernixc_lexical::{kind, token, tree::RelativeSpan};

use crate::output::{One, Verify};

/// The most basic kind of parser that used to determine if a token is a valid
/// choice for a certain syntax tree node.
pub trait Expect: Verify {
    /// Determines if the given token passes the expectation.
    fn expect(&self, terminal: &token::Kind<RelativeSpan>) -> bool;
}

/// Expecting the token to be an identifier;
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Identifier;

impl Verify for Identifier {
    type Extract = One;
    type Output = token::Kind<RelativeSpan>;
}

impl Expect for Identifier {
    fn expect(&self, terminal: &token::Kind<RelativeSpan>) -> bool {
        terminal.kind.is_identifier()
    }
}

/// Expecting the token to be an identifier of the given value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IdentifierValue<'a>(pub &'a str);

impl Verify for IdentifierValue<'_> {
    type Extract = One;
    type Output = token::Kind<RelativeSpan>;
}

impl Expect for IdentifierValue<'_> {
    fn expect(&self, terminal: &token::Kind<RelativeSpan>) -> bool {
        terminal.kind.as_identifier().is_some_and(|x| x.0.as_str() == self.0)
    }
}

/// Expecting the token to be a string token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct String;

impl Verify for String {
    type Extract = One;
    type Output = token::Kind<RelativeSpan>;
}

impl Expect for String {
    fn expect(&self, terminal: &token::Kind<RelativeSpan>) -> bool {
        terminal.kind.is_string()
    }
}

/// Expecting the token to be a character token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Character;

impl Verify for Character {
    type Extract = One;
    type Output = token::Kind<RelativeSpan>;
}

impl Expect for Character {
    fn expect(&self, terminal: &token::Kind<RelativeSpan>) -> bool {
        terminal.kind.is_character()
    }
}

/// Expecting the token to be a numeric token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Numeric;

impl Verify for Numeric {
    type Extract = One;
    type Output = token::Kind<RelativeSpan>;
}

impl Expect for Numeric {
    fn expect(&self, terminal: &token::Kind<RelativeSpan>) -> bool {
        terminal.kind.is_numeric()
    }
}

/// Expecting the token to be a punctuation token.
pub type Punctuation = char;

impl Verify for Punctuation {
    type Extract = One;
    type Output = token::Kind<RelativeSpan>;
}

impl Expect for Punctuation {
    fn expect(&self, terminal: &token::Kind<RelativeSpan>) -> bool {
        terminal.kind.as_punctuation().is_some_and(|x| x.0 == *self)
    }
}

/// Expecting the token to be a newline token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NewLine;

impl Verify for NewLine {
    type Extract = One;
    type Output = token::Kind<RelativeSpan>;
}

impl Expect for NewLine {
    fn expect(&self, terminal: &token::Kind<RelativeSpan>) -> bool {
        terminal.kind.is_new_line()
    }
}

/// Expecting the token to be a keyword token.
pub type Keyword = kind::Keyword;

impl Verify for Keyword {
    type Extract = One;
    type Output = token::Kind<RelativeSpan>;
}

impl Expect for Keyword {
    fn expect(&self, terminal: &token::Kind<RelativeSpan>) -> bool {
        terminal.kind.as_keyword().is_some_and(|x| x == self)
    }
}

/// See [`Ext::no_prior_insignificant`] for more information.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NoPriorInsignificant<T>(pub T);

impl<T: Verify> Verify for NoPriorInsignificant<T> {
    type Extract = T::Extract;
    type Output = T::Output;
}

impl<T: Expect> Expect for NoPriorInsignificant<T> {
    fn expect(&self, terminal: &token::Kind<RelativeSpan>) -> bool {
        if terminal.prior_insignificant.is_some() {
            return false;
        }

        self.0.expect(terminal)
    }
}

/// Extension trait for [`Expect`] that adds additional functionality.
pub trait Ext {
    /// Wrapper around an [`Expect`] that adds a check for the token to not have
    /// any prior insignificant tokens.
    fn no_prior_insignificant(self) -> NoPriorInsignificant<Self>
    where
        Self: Sized;
}

impl<T: Sized + Expect> Ext for T {
    fn no_prior_insignificant(self) -> NoPriorInsignificant<Self> {
        NoPriorInsignificant(self)
    }
}
