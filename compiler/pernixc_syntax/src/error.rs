//! Contains the definition of [`Error`]

use std::{fmt::Display, sync::Arc};

use enum_as_inner::EnumAsInner;
use pernixc_base::{
    log::{Message, Severity, SourceCodeDisplay},
    source_file::{SourceElement, SourceFile, Span},
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
        }
    }
}

/// What was found in place of the expected syntax kind.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum Found {
    Token(Token),
    EndOfFile(Arc<SourceFile>),
}

impl SourceElement for Found {
    fn span(&self) -> Span {
        match self {
            Found::Token(token) => token.span().clone(),
            Found::EndOfFile(source_file) => {
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
#[derive(Debug, Clone)]
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

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let expected_string = self.get_expected_string();

        let found_string = match &self.found {
            Found::Token(Token::Comment(..)) => "a comment token".to_string(),
            Found::Token(Token::Identifier(..)) => {
                "an identifier token".to_string()
            }
            Found::Token(Token::Keyword(keyword)) => {
                format!("a keyword token `{}`", keyword.kind.as_str())
            }
            Found::Token(Token::WhiteSpaces(..)) => {
                "a white spaces token".to_string()
            }
            Found::Token(Token::Punctuation(punctuation)) => {
                format!("a punctuation token `{}`", punctuation.punctuation)
            }
            Found::Token(Token::Numeric(..)) => "a numeric token".to_string(),
            Found::Token(Token::Character(..)) => {
                "a character literal token".to_string()
            }
            Found::Token(Token::String(..)) => {
                "a string literal token".to_string()
            }

            Found::EndOfFile(_) => "EOF".to_string(),
        };

        let message =
            format!("expected {expected_string}, but found {found_string}");

        write!(f, "{}", Message::new(Severity::Error, message))?;

        let span = self.found.span();
        write!(f, "\n{}", SourceCodeDisplay::new(&span, None::<i32>))
    }
}
