use std::fmt::Display;

use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_lexical::token::{KeywordKind, Token};
use pernixc_print::{LogSeverity, MessageLog, SourceCodeDisplay};
use pernixc_source::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum SyntaxKind {
    Pattern,
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
    ImplementsMember,
}

/// A syntax/token is expected but found an other invalid token.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnexpectedSyntax {
    /// The kind of syntax that was expected.
    pub expected: SyntaxKind,

    /// The invalid token that was found.
    pub found: Option<Token>,
}

impl Display for UnexpectedSyntax {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let expected_binding = match self.expected {
            SyntaxKind::HigherRankedBound => "a higher ranked bound syntax".to_string(),
            SyntaxKind::Identifier => "an identifier token".to_string(),
            SyntaxKind::TypeSpecifier => "a type specifier syntax".to_string(),
            SyntaxKind::Expression => "an expression syntax".to_string(),
            SyntaxKind::StructMember => "a struct member syntax".to_string(),
            SyntaxKind::Item => "an item syntax".to_string(),
            SyntaxKind::AccessModifier => "an access modifier syntax".to_string(),
            SyntaxKind::Punctuation(char) => format!("a punctuation token `{char}`"),
            SyntaxKind::Keyword(keyword) => format!("a keyword token `{}`", keyword.as_str()),
            SyntaxKind::TraitMember => "a trait member syntax".to_string(),
            SyntaxKind::ImplementsMember => "an implements member syntax".to_string(),
            SyntaxKind::Pattern => "a pattern syntax".to_string(),
        };
        let found_binding = match self.found.clone() {
            Some(Token::Comment(..)) => "a comment token".to_string(),
            Some(Token::Identifier(..)) => "an identifier token".to_string(),
            Some(Token::Keyword(keyword)) => {
                format!("a keyword token `{}`", keyword.keyword.as_str())
            }
            Some(Token::WhiteSpaces(..)) => "a white spaces token".to_string(),
            Some(Token::Punctuation(punctuation)) => {
                format!("a punctuation token `{}`", punctuation.punctuation)
            }
            Some(Token::Numeric(..)) => "a numeric token".to_string(),

            None => "EOF".to_string(),
        };

        let message = format!("expected {expected_binding}, but found {found_binding}");

        write!(f, "{}", MessageLog::new(LogSeverity::Error, message))?;

        self.found.as_ref().map_or(Ok(()), |span| {
            write!(
                f,
                "{}",
                SourceCodeDisplay::new(span.span(), Option::<i32>::None)
            )
        })
    }
}

/// A higher ranked bound parameter cannot be empty.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HigherRankedBoundParameterCannotBeEmpty {
    /// The span of the higher ranked bound parameter.
    pub span: Span,
}

impl Display for HigherRankedBoundParameterCannotBeEmpty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            MessageLog::new(
                LogSeverity::Error,
                "a higher ranked bound parameter cannot be empty"
            ),
            SourceCodeDisplay::new(&self.span, Option::<i32>::None)
        )
    }
}

/// A generic arugment/parameter list cannot be empty.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericArgumentParameterListCannotBeEmpty {
    /// The span of the generic argument/parameter.
    pub span: Span,
}

impl Display for GenericArgumentParameterListCannotBeEmpty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            MessageLog::new(
                LogSeverity::Error,
                "a generic argument/parameter list cannot be empty"
            ),
            SourceCodeDisplay::new(&self.span, Option::<i32>::None)
        )
    }
}

/// Is an enumeration containing all kinds of syntactic errors that can occur while parsing the
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Error {
    GenericArgumentParameterListCannotBeEmpty(GenericArgumentParameterListCannotBeEmpty),
    HigherRankedBoundParameterCannotBeEmpty(HigherRankedBoundParameterCannotBeEmpty),
    UnexpectedSyntax(UnexpectedSyntax),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::GenericArgumentParameterListCannotBeEmpty(e) => e.fmt(f),
            Self::HigherRankedBoundParameterCannotBeEmpty(e) => e.fmt(f),
            Self::UnexpectedSyntax(e) => e.fmt(f),
        }
    }
}
