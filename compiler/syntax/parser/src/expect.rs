//! contains the definition of [`Expect`] trait and its implementations for
//! various token types.

use derive_more::{Display, From};
use enum_as_inner::EnumAsInner;
use pernixc_lexical::{
    kind, token,
    tree::{DelimiterKind, RelativeLocation},
};
use qbice::{Decode, Encode, StableHash};

use crate::output::{One, Output};

/// The most basic kind of parser that used to determine if a token is a valid
/// choice for a certain syntax tree node.
pub trait Expect: Output + Into<Expected> {
    /// Determines if the given token passes the expectation.
    fn expect(&self, terminal: &token::Kind<RelativeLocation>) -> bool;
}

/// Expecting the token to be an identifier;
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Encode,
    Decode,
    StableHash,
)]
pub struct Identifier;

impl Output for Identifier {
    type Extract = One;

    type Output<'a> = token::Identifier<RelativeLocation>;

    fn output<'a>(
        &self,
        node: &'a crate::concrete_tree::Node,
    ) -> Option<Self::Output<'a>> {
        node.as_leaf().and_then(|x| {
            x.kind.as_identifier().cloned().map(|y| token::Token {
                kind: y,
                span: x.span,
                prior_insignificant: x.prior_insignificant,
            })
        })
    }
}

impl Expect for Identifier {
    fn expect(&self, terminal: &token::Kind<RelativeLocation>) -> bool {
        terminal.kind.is_identifier()
    }
}

/// Expects an identifier with pre-defined value.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Display,
    Encode,
    Decode,
    StableHash,
)]
#[allow(missing_docs)]
pub enum IdentifierValue {
    #[display("x")]
    X,
}

impl IdentifierValue {
    /// Gets the string that the identifier must have.
    #[must_use]
    pub const fn expected_string(&self) -> &'static str {
        match self {
            Self::X => "x",
        }
    }
}

impl Output for IdentifierValue {
    type Extract = One;
    type Output<'a> = token::Identifier<RelativeLocation>;

    fn output<'a>(
        &self,
        node: &'a crate::concrete_tree::Node,
    ) -> Option<Self::Output<'a>> {
        node.as_leaf().and_then(|x| {
            x.kind
                .as_identifier()
                .cloned()
                .filter(|y| y.0.as_ref() == self.expected_string())
                .map(|y| token::Token {
                    kind: y,
                    span: x.span,
                    prior_insignificant: x.prior_insignificant,
                })
        })
    }
}

impl Expect for IdentifierValue {
    fn expect(&self, terminal: &token::Kind<RelativeLocation>) -> bool {
        terminal
            .kind
            .as_identifier()
            .is_some_and(|x| x.0.as_ref() == self.expected_string())
    }
}

/// Expecting the token to be a string token.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct String;

impl Output for String {
    type Extract = One;
    type Output<'a> = token::String<RelativeLocation>;

    fn output<'a>(
        &self,
        node: &'a crate::concrete_tree::Node,
    ) -> Option<Self::Output<'a>> {
        node.as_leaf().and_then(|x| {
            x.kind.as_string().cloned().map(|y| token::Token {
                kind: y,
                span: x.span,
                prior_insignificant: x.prior_insignificant,
            })
        })
    }
}

impl Expect for String {
    fn expect(&self, terminal: &token::Kind<RelativeLocation>) -> bool {
        terminal.kind.is_string()
    }
}

/// Expecting the token to be a character token.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct Character;

impl Output for Character {
    type Extract = One;
    type Output<'a> = token::Character<RelativeLocation>;

    fn output<'a>(
        &self,
        node: &'a crate::concrete_tree::Node,
    ) -> Option<Self::Output<'a>> {
        node.as_leaf().and_then(|x| {
            x.kind.as_character().copied().map(|y| token::Token {
                kind: y,
                span: x.span,
                prior_insignificant: x.prior_insignificant,
            })
        })
    }
}

impl Expect for Character {
    fn expect(&self, terminal: &token::Kind<RelativeLocation>) -> bool {
        terminal.kind.is_character()
    }
}

/// Expecting the token to be a numeric token.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct Numeric;

impl Output for Numeric {
    type Extract = One;
    type Output<'a> = token::Numeric<RelativeLocation>;

    fn output<'a>(
        &self,
        node: &'a crate::concrete_tree::Node,
    ) -> Option<Self::Output<'a>> {
        node.as_leaf().and_then(|x| {
            x.kind.as_numeric().cloned().map(|y| token::Token {
                kind: y,
                span: x.span,
                prior_insignificant: x.prior_insignificant,
            })
        })
    }
}

impl Expect for Numeric {
    fn expect(&self, terminal: &token::Kind<RelativeLocation>) -> bool {
        terminal.kind.is_numeric()
    }
}

/// Expecting the token to be a punctuation token.
pub type Punctuation = char;

impl Output for Punctuation {
    type Extract = One;
    type Output<'a> = token::Punctuation<RelativeLocation>;

    fn output<'a>(
        &self,
        node: &'a crate::concrete_tree::Node,
    ) -> Option<Self::Output<'a>> {
        node.as_leaf().and_then(|x| {
            x.kind.as_punctuation().filter(|x| ***x == *self).copied().map(
                |y| token::Token {
                    kind: y,
                    span: x.span,
                    prior_insignificant: x.prior_insignificant,
                },
            )
        })
    }
}

impl Expect for Punctuation {
    fn expect(&self, terminal: &token::Kind<RelativeLocation>) -> bool {
        terminal.kind.as_punctuation().is_some_and(|x| x.0 == *self)
    }
}

/// Expecting the token to be a newline token.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct NewLine;

impl Output for NewLine {
    type Extract = One;
    type Output<'a> = token::NewLine<RelativeLocation>;

    fn output<'a>(
        &self,
        node: &'a crate::concrete_tree::Node,
    ) -> Option<Self::Output<'a>> {
        node.as_leaf().and_then(|x| {
            x.kind.as_new_line().copied().map(|y| token::Token {
                kind: y,
                span: x.span,
                prior_insignificant: x.prior_insignificant,
            })
        })
    }
}

impl Expect for NewLine {
    fn expect(&self, terminal: &token::Kind<RelativeLocation>) -> bool {
        terminal.kind.is_new_line()
    }
}

/// Expecting the token to be a keyword token.
pub type Keyword = kind::Keyword;

impl Output for Keyword {
    type Extract = One;
    type Output<'a> = token::Keyword<RelativeLocation>;

    fn output<'a>(
        &self,
        node: &'a crate::concrete_tree::Node,
    ) -> Option<Self::Output<'a>> {
        node.as_leaf().and_then(|x| {
            x.kind.as_keyword().filter(|x| *x == self).copied().map(|y| {
                token::Token {
                    kind: y,
                    span: x.span,
                    prior_insignificant: x.prior_insignificant,
                }
            })
        })
    }
}

impl Expect for Keyword {
    fn expect(&self, terminal: &token::Kind<RelativeLocation>) -> bool {
        terminal.kind.as_keyword().is_some_and(|x| x == self)
    }
}

/// See [`Ext::no_prior_insignificant`] for more information.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct NoPriorInsignificant<T>(pub T);

impl<T: Output> Output for NoPriorInsignificant<T> {
    type Extract = T::Extract;
    type Output<'a> = T::Output<'a>;

    fn output<'a>(
        &self,
        node: &'a crate::concrete_tree::Node,
    ) -> Option<Self::Output<'a>> {
        self.0.output(node)
    }
}

impl<T: Expect> Expect for NoPriorInsignificant<T> {
    fn expect(&self, terminal: &token::Kind<RelativeLocation>) -> bool {
        if terminal.prior_insignificant.is_some() {
            return false;
        }

        self.0.expect(terminal)
    }
}

impl<T: Expect> From<NoPriorInsignificant<T>> for Expected {
    fn from(val: NoPriorInsignificant<T>) -> Self { val.0.into() }
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

/// An enumeration of what kind of fragment that the state machine can step
/// into.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
#[allow(missing_docs)]
pub enum Fragment {
    Indentation,
    Delimited(DelimiterKind),
}

/// An enumeration of all the possible expected token types.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    From,
    Encode,
    Decode,
    StableHash,
)]
#[allow(missing_docs)]
pub enum Expected {
    Identifier(Identifier),
    IdentifierValue(IdentifierValue),
    String(String),
    Character(Character),
    Numeric(Numeric),
    Punctuation(Punctuation),
    NewLine(NewLine),
    Keyword(Keyword),
    Fragment(Fragment),
}
