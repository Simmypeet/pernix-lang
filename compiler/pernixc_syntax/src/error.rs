//! Contains the definition of [`Error`]

use std::fmt::Display;

use enum_as_inner::EnumAsInner;
use pernixc_base::log::{Message, Severity, SourceCodeDisplay};
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
    pub found: Option<Token>,
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

        let found_string = match self.found.clone() {
            Some(Token::Comment(..)) => "a comment token".to_string(),
            Some(Token::Identifier(..)) => "an identifier token".to_string(),
            Some(Token::Keyword(keyword)) => {
                format!("a keyword token `{}`", keyword.kind.as_str())
            }
            Some(Token::WhiteSpaces(..)) => "a white spaces token".to_string(),
            Some(Token::Punctuation(punctuation)) => {
                format!("a punctuation token `{}`", punctuation.punctuation)
            }
            Some(Token::Numeric(..)) => "a numeric token".to_string(),

            None => "EOF".to_string(),
        };

        let message =
            format!("expected {expected_string}, but found {found_string}");

        write!(f, "{}", Message::new(Severity::Error, message))?;

        self.found.as_ref().map_or(Ok(()), |span| {
            write!(
                f,
                "\n{}",
                SourceCodeDisplay::new(span.span(), Option::<i32>::None)
            )
        })
    }
}
