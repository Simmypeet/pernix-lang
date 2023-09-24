//! Contains all kinds of lexical errors that can occur while tokenizing the source code.

use std::fmt::Display;

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_print::{LogSeverity, MessageLog, SourceCodeDisplay};
use pernixc_source::Span;

use crate::token_stream::Delimiter;

/// The source code contains an unclosed `/*` comment.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Getters)]
pub struct UnterminatedDelimitedComment {
    /// The span of the unclosed `/*` that starts the comment.
    pub span: Span,
}

impl Display for UnterminatedDelimitedComment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            MessageLog::new(LogSeverity::Error, "found an unclosed `/*` comment"),
            SourceCodeDisplay::new(&self.span, Option::<i32>::None)
        )
    }
}

/// The delimiter is not closed by its corresponding closing pair.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Getters)]
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
            "{}{}",
            MessageLog::new(LogSeverity::Error, "found an undelimited delimiter"),
            SourceCodeDisplay::new(
                &self.opening_span,
                Some("this delimiter is not closed by its corresponding closing pair")
            )
        )
    }
}

/// Is an enumeration containing all kinds of lexical errors that can occur while tokenizing the
/// source code.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Error {
    UnterminatedDelimitedComment(UnterminatedDelimitedComment),
    UndelimitedDelimiter(UndelimitedDelimiter),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnterminatedDelimitedComment(err) => write!(f, "{err}"),
            Self::UndelimitedDelimiter(err) => write!(f, "{err}"),
        }
    }
}
