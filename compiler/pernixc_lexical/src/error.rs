//! Contains all kinds of lexical errors that can occur while tokenizing the
//! source code.

use std::convert::Infallible;

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_base::{
    diagnostic::{Diagnostic, Report},
    log::Severity,
    source_file::Span,
};

use crate::token_stream::Delimiter;

/// The source code contains an unclosed `/*` comment.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct UnterminatedDelimitedComment {
    /// The span of the unclosed `/*` that starts the comment.
    pub span: Span,
}

impl Report for UnterminatedDelimitedComment {
    type Error = Infallible;
    type Parameter = ();

    /// Gets the diagnostic information for the error.
    fn report(&self, (): Self::Parameter) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: "found an unclosed `/*` comment".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
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

impl Report for UndelimitedDelimiter {
    type Error = Infallible;
    type Parameter = ();

    fn report(&self, (): Self::Parameter) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.opening_span.clone(),
            message: "found an undelimited delimiter".to_string(),
            severity: Severity::Error,
            help_message: Some(
                "this delimiter is not closed by its corresponding closing \
                 pair"
                    .to_string(),
            ),
            related: Vec::new(),
        })
    }
}

/// The source code contains an unterminated string literal.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct UnterminatedStringLiteral {
    /// The span of the unclosed double quote that starts the string literal.
    pub span: Span,
}

impl Report for UnterminatedStringLiteral {
    type Error = Infallible;
    type Parameter = ();

    fn report(&self, (): Self::Parameter) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: "found an unterminated string literal".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The source code contains an invalid escape sequence.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct InvalidEscapeSequence {
    /// The span of the invalid escape sequence (including the backslash).
    pub span: Span,
}

impl Report for InvalidEscapeSequence {
    type Error = Infallible;
    type Parameter = ();

    fn report(&self, (): Self::Parameter) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: "found an invalid escape sequence".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
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

impl Report for Error {
    type Error = Infallible;
    type Parameter = ();

    fn report(&self, (): Self::Parameter) -> Result<Diagnostic, Self::Error> {
        match self {
            Self::UnterminatedDelimitedComment(err) => err.report(()),
            Self::UndelimitedDelimiter(err) => err.report(()),
            Self::UnterminatedStringLiteral(err) => err.report(()),
            Self::InvalidEscapeSequence(err) => err.report(()),
        }
    }
}
