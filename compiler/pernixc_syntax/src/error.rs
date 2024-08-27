//! Contains the definition of [`Error`]

use std::{convert::Infallible, sync::Arc};

use enum_as_inner::EnumAsInner;
use pernixc_base::{
    diagnostic::{Diagnostic, Report},
    log::Severity,
    source_file::{SourceFile, Span},
};
use pernixc_lexical::token::{KeywordKind, Token};

/// Enumeration of all possible syntax kinds.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
#[allow(missing_docs)]
pub enum SyntaxKind {
    RefutablePattern,
    IrrefutablePattern,
    GenericParameter,
    HigherRankedBound,
    Identifier,
    TypeSpecifier,
    Expression,
    StructMember,
    Item,
    AccessModifier,
    Punctuation(char),
    Keyword(KeywordKind),
    TraitMember,
    ImplementationMember,
    Numeric,
    Predicate,
    String,
    JsonValue,
}

impl SyntaxKind {
    fn get_expected_string(self) -> String {
        match self {
            Self::IrrefutablePattern => {
                "an irrefutable pattern syntax".to_string()
            }
            Self::HigherRankedBound => {
                "a higher ranked bound syntax".to_string()
            }
            Self::GenericParameter => "a generic parameter syntax".to_string(),
            Self::Identifier => "an identifier token".to_string(),
            Self::Predicate => "a predicate syntax".to_string(),
            Self::TypeSpecifier => "a type specifier syntax".to_string(),
            Self::Expression => "an expression syntax".to_string(),
            Self::StructMember => "a struct member syntax".to_string(),
            Self::Item => "an item syntax".to_string(),
            Self::AccessModifier => "an access modifier syntax".to_string(),
            Self::Punctuation(char) => format!("a punctuation token `{char}`"),
            Self::Keyword(keyword) => {
                format!("a keyword token `{}`", keyword.as_str())
            }
            Self::TraitMember => "a trait member syntax".to_string(),
            Self::ImplementationMember => {
                "an implements member syntax".to_string()
            }
            Self::RefutablePattern => "a refutable pattern syntax".to_string(),
            Self::Numeric => "a numeric token".to_string(),
            Self::String => "a string literal".to_string(),
            Self::JsonValue => "a JSON value syntax".to_string(),
        }
    }
}

///A token was found unexpectedly.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unexpected {
    /// The insignificant token that was found before the unexpected token.
    ///
    /// This is used for providing a better error message.
    pub(crate) prior_insignificant: Option<Token>,

    /// The unexpected token that was found.
    pub(crate) unexpected: Token,
}

/// What was found in place of the expected syntax kind.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Found {
    Unexpected(Unexpected),
    EndOfFile(Arc<SourceFile>),
}

impl Found {
    /// Gets the span of the found token.
    #[must_use]
    pub fn span(&self) -> Span {
        match self {
            Self::Unexpected(unexpected) => {
                unexpected.prior_insignificant.as_ref().map_or_else(
                    || unexpected.unexpected.span().clone(),
                    |x| x.span().join(unexpected.unexpected.span()).unwrap(),
                )
            }
            Self::EndOfFile(source_file) => {
                let last_byte = source_file.content().len();
                let mut char_boundary = last_byte - 1;

                while !source_file.content().is_char_boundary(char_boundary) {
                    char_boundary -= 1;
                }

                Span::new(source_file.clone(), char_boundary, last_byte)
                    .unwrap()
            }
        }
    }
}

/// A syntax/token is expected but found an other invalid token.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Error {
    /// The kind of syntax that was expected.
    pub expected: SyntaxKind,

    /// The alternative syntax that can be used other than the expected one.
    pub alternatives: Vec<SyntaxKind>,

    /// The invalid token that was found.
    pub found: Found,
}

impl Error {
    fn get_expected_string(&self) -> String {
        match self.alternatives.len() {
            0 => self.expected.get_expected_string(),
            1 => {
                format!(
                    "{} or {}",
                    self.expected.get_expected_string(),
                    self.alternatives[0].get_expected_string()
                )
            }
            _ => {
                format!(
                    "{}, or {}",
                    self.alternatives
                        .iter()
                        .copied()
                        .map(SyntaxKind::get_expected_string)
                        .collect::<Vec<_>>()
                        .join(", "),
                    self.expected.get_expected_string()
                )
            }
        }
    }
}

impl Report<()> for Error {
    type Error = Infallible;

    fn report(&self, (): ()) -> Result<Diagnostic, Self::Error> {
        let expected_string = self.get_expected_string();

        let found_string = match &self.found {
            Found::Unexpected(Unexpected {
                unexpected: Token::Comment(..),
                ..
            }) => "a comment token".to_string(),
            Found::Unexpected(Unexpected {
                unexpected: Token::Identifier(..),
                ..
            }) => "an identifier token".to_string(),
            Found::Unexpected(Unexpected {
                unexpected: Token::Keyword(keyword),
                ..
            }) => {
                format!("a keyword token `{}`", keyword.kind.as_str())
            }
            Found::Unexpected(Unexpected {
                unexpected: Token::WhiteSpaces(..),
                ..
            }) => "a white spaces token".to_string(),
            Found::Unexpected(Unexpected {
                unexpected: Token::Punctuation(punctuation),
                ..
            }) => {
                format!("a punctuation token `{}`", punctuation.punctuation)
            }
            Found::Unexpected(Unexpected {
                unexpected: Token::Numeric(..),
                ..
            }) => "a numeric token".to_string(),
            Found::Unexpected(Unexpected {
                unexpected: Token::Character(..),
                ..
            }) => "a character literal token".to_string(),
            Found::Unexpected(Unexpected {
                unexpected: Token::String(..),
                ..
            }) => "a string literal token".to_string(),

            Found::EndOfFile(_) => "EOF".to_string(),
        };

        let message =
            format!("expected {expected_string}, but found {found_string}");

        Ok(Diagnostic {
            span: self.found.span(),
            message,
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}
