//! Contains all kinds of lexical errors that can occur while tokenizing the
//! source code.

use std::fmt::Display;

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_base::{
    log::{Message, Severity, SourceCodeDisplay},
    source_file::Span,
};

use crate::token_stream::Delimiter;

/// The source code contains an unclosed `/*` comment.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct UnterminatedDelimitedComment {
    /// The span of the unclosed `/*` that starts the comment.
    pub span: Span,
}

impl Display for UnterminatedDelimitedComment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{}",
            Message::new(Severity::Error, "found an unclosed `/*` comment"),
            SourceCodeDisplay::new(&self.span, Option::<i32>::None)
        )
    }
}

/// The delimiter is not closed by its corresponding closing pair.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct UndelimitedDelimiter {
    /// The span of the opening delimiter.
    pub opening_span: Span,

    /// The kind of the delimiter.
    pub delimiter: Delimiter,
}

impl Display for UndelimitedDelimiter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{}",
            Message::new(Severity::Error, "found an undelimited delimiter"),
            SourceCodeDisplay::new(
                &self.opening_span,
                Some(
                    "this delimiter is not closed by its corresponding \
                     closing pair"
                )
            )
        )
    }
}

/// The source code contains an unterminated string literal.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct UnterminatedStringLiteral {
    /// The span of the unclosed double quote that starts the string literal.
    pub span: Span,
}

impl Display for UnterminatedStringLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{}",
            Message::new(
                Severity::Error,
                "found an unterminated string literal"
            ),
            SourceCodeDisplay::new(&self.span, Option::<i32>::None)
        )
    }
}

/// The source code contains an invalid escape sequence.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct InvalidEscapeSequence {
    /// The span of the invalid escape sequence (including the backslash).
    pub span: Span,
}

impl Display for InvalidEscapeSequence {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{}",
            Message::new(Severity::Error, "found an invalid escape sequence"),
            SourceCodeDisplay::new(&self.span, Option::<i32>::None)
        )
    }
}

/// Is an enumeration containing all kinds of lexical errors that can occur
/// while tokenizing the source code.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From,
)]
#[allow(missing_docs)]
pub enum Error {
    UnterminatedDelimitedComment(UnterminatedDelimitedComment),
    UndelimitedDelimiter(UndelimitedDelimiter),
    UnterminatedStringLiteral(UnterminatedStringLiteral),
    InvalidEscapeSequence(InvalidEscapeSequence),
}

impl Error {
    /// Gets the span where the error occurred.
    pub fn span(&self) -> &Span {
        match self {
            Self::UnterminatedDelimitedComment(err) => &err.span,
            Self::UndelimitedDelimiter(err) => &err.opening_span,
            Self::UnterminatedStringLiteral(err) => &err.span,
            Self::InvalidEscapeSequence(err) => &err.span,
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnterminatedDelimitedComment(err) => write!(f, "{err}"),
            Self::UndelimitedDelimiter(err) => write!(f, "{err}"),
            Self::UnterminatedStringLiteral(err) => write!(f, "{err}"),
            Self::InvalidEscapeSequence(err) => write!(f, "{err}"),
        }
    }
}
