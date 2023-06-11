//! Contains all kinds of lexical errors that can occur while tokenizing the source code.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_print::LogSeverity;
use pernixc_source::Span;
use thiserror::Error;

use crate::token_stream::Delimiter;

/// The source code contains an unclosed `/*` comment.
#[derive(Debug, Clone, Getters)]
pub struct UnterminatedDelimitedComment {
    /// The span of the unclosed `/*` that starts the comment.
    pub span: Span,
}

impl UnterminatedDelimitedComment {
    /// Prints the error message to the console
    pub fn print(&self) {
        pernixc_print::print(LogSeverity::Error, "found an unclosed `/*` comment");
        pernixc_print::print_source_code(&self.span, Some("this `/*` comment is unclosed"));
        println!();
    }
}

/// The delimiter is not closed by its corresponding closing pair.
#[derive(Debug, Clone, Getters)]
pub struct UndelimitedDelimiter {
    /// The span of the opening delimiter.
    pub opening_span: Span,

    /// The kind of the delimiter.
    pub delimiter: Delimiter,
}

impl UndelimitedDelimiter {
    /// Prints the error message to the console
    pub fn print(&self) {
        pernixc_print::print(LogSeverity::Error, "found an undelimited delimiter");
        pernixc_print::print_source_code(
            &self.opening_span,
            Some("this delimiter is not closed by its corresponding closing pair"),
        );
        println!();
    }
}

/// Is an enumeration containing all kinds of lexical errors that can occur while tokenizing the
/// source code.
#[derive(Debug, Clone, EnumAsInner, Error, From)]
#[error("Encountered an lexical error while tokenizing the source code.")]
#[allow(missing_docs)]
pub enum Error {
    UnterminatedDelimitedComment(UnterminatedDelimitedComment),
    UndelimitedDelimiter(UndelimitedDelimiter),
}

impl Error {
    /// Prints the error message to the console
    pub fn print(&self) {
        match self {
            Self::UnterminatedDelimitedComment(error) => error.print(),
            Self::UndelimitedDelimiter(error) => error.print(),
        }
    }
}
