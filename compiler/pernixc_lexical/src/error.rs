//! Contains all kinds of lexical errors that can occur while tokenizing the
//! source code.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_diagnostic::{Diagnostic, Related, Report, Severity};
use pernixc_source_file::{AbsoluteSpan, ByteIndex, SourceMap};

use crate::tree::DelimiterKind;

/// The delimiter is not closed by its corresponding closing pair.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct UndelimitedDelimiter {
    /// The span of the opening delimiter.
    pub opening_span: AbsoluteSpan,

    /// The kind of the delimiter.
    pub delimiter: DelimiterKind,
}

impl Report<&SourceMap> for UndelimitedDelimiter {
    type Location = ByteIndex;

    fn report(&self, _: &SourceMap) -> Diagnostic<Self::Location> {
        Diagnostic {
            span: self.opening_span,
            message: "found an undelimited delimiter".to_string(),
            label: Some(format!(
                "need to be cloesd with `{}` pair",
                match self.delimiter {
                    DelimiterKind::Parenthesis => ')',
                    DelimiterKind::Brace => '}',
                    DelimiterKind::Bracket => ']',
                }
            )),
            severity: Severity::Error,
            help_message: Some(
                "this delimiter is not closed by its corresponding closing \
                 pair"
                    .to_string(),
            ),
            related: Vec::new(),
        }
    }
}

/// The source code contains an unterminated string literal.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct UnterminatedStringLiteral {
    /// The span of the unclosed double quote that starts the string literal.
    pub span: AbsoluteSpan,
}

impl Report<&SourceMap> for UnterminatedStringLiteral {
    type Location = ByteIndex;

    fn report(&self, _: &SourceMap) -> Diagnostic<Self::Location> {
        Diagnostic {
            span: self.span,
            label: Some("need to be closed with a double quote".to_string()),
            message: "found an unterminated string literal".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The source code contains an invalid escape sequence.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct InvalidEscapeSequence {
    /// The span of the invalid escape sequence (including the backslash).
    pub span: AbsoluteSpan,
}

impl Report<&SourceMap> for InvalidEscapeSequence {
    type Location = ByteIndex;

    fn report(&self, _: &SourceMap) -> Diagnostic<Self::Location> {
        Diagnostic {
            span: self.span,
            message: "found an invalid escape sequence".to_string(),
            label: None,
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The available indentation level that can be used.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct AvailableIndentation {
    /// The span of the colon that starts the indentation level.
    pub colon_span: AbsoluteSpan,

    /// The indentation level of that particular indentation.
    pub indentation_size: usize,
}

/// Found a token in an invalid indentation level.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct InvalidIndentation {
    /// The span of the invalid indentation.
    pub span: AbsoluteSpan,

    /// The indentation level found in the source code.
    pub found_indentation: usize,

    /// The available indentation level that can be used.
    pub available_indentations: Vec<AvailableIndentation>,
}

impl Report<&SourceMap> for InvalidIndentation {
    type Location = ByteIndex;

    fn report(&self, _: &SourceMap) -> Diagnostic<Self::Location> {
        Diagnostic {
            span: self.span,
            message: "the token is in an invalid indentation level".to_string(),
            label: Some(format!("found {} space(s)", self.found_indentation,)),
            severity: Severity::Error,
            help_message: None,
            related: self
                .available_indentations
                .iter()
                .map(|span| Related {
                    span: span.colon_span,
                    message: format!(
                        "previous indentation level is {} space(s)",
                        span.indentation_size
                    ),
                })
                .collect(),
        }
    }
}

/// A new indentation level was expected, but the source code does not
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ExpectIndentation {
    /// The span of the expected indentation.
    pub span: AbsoluteSpan,

    /// The span to the colon that starts the indentation level.
    pub indentation_start: AbsoluteSpan,
}

impl Report<&SourceMap> for ExpectIndentation {
    type Location = ByteIndex;

    fn report(&self, source_map: &SourceMap) -> Diagnostic<Self::Location> {
        Diagnostic {
            span: self.span,
            message: "expect an indentation".to_string(),
            label: Some(format!(
                "`{}` is not indented",
                &source_map[self.span.source_id].content()[self.span.range()]
            )),
            severity: Severity::Error,
            help_message: Some(format!(
                "add spaces before `{}` to indent",
                &source_map[self.span.source_id].content()[self.span.range()]
            )),
            related: vec![Related {
                span: self.indentation_start,
                message: "this colon starts the indentation level".to_string(),
            }],
        }
    }
}

/// The source code contains an invalid new indentation level.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct InvalidNewIndentationLevel {
    /// The span of the invalid new indentation level.
    pub span: AbsoluteSpan,

    /// The span of the previous indentation level.
    pub previous_indentation_span: AbsoluteSpan,

    /// The expected indentation level.
    pub latest_indentation: usize,

    /// The indentation level found in the source code.
    pub found_indentation: usize,
}

impl Report<&SourceMap> for InvalidNewIndentationLevel {
    type Location = ByteIndex;

    fn report(&self, _: &SourceMap) -> Diagnostic<Self::Location> {
        Diagnostic {
            span: self.span,
            message: "found an invalid new indentation level".to_string(),
            label: Some(format!(
                "found {} space(s), but the previous indentation level is {} \
                 space(s)",
                self.found_indentation, self.latest_indentation
            )),
            severity: Severity::Error,
            help_message: Some(
                "must be deeper than the previous indentation level"
                    .to_string(),
            ),
            related: vec![Related {
                span: self.previous_indentation_span,
                message: format!(
                    "previous indentation level is {} space(s)",
                    self.latest_indentation
                ),
            }],
        }
    }
}

/// The closing delimiter (e.g. `)`, `}`, or `]`) was found without any prior
/// opening delimiter (e.g. `(`, `{`, or `[`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct UnexpectedClosingDelimiter {
    /// The span of the closing delimiter.
    pub span: AbsoluteSpan,
}

impl Report<&SourceMap> for UnexpectedClosingDelimiter {
    type Location = ByteIndex;

    fn report(&self, source_map: &SourceMap) -> Diagnostic<Self::Location> {
        Diagnostic {
            span: self.span,
            message: "found an unexpected closing delimiter".to_string(),
            label: Some(format!(
                "this closing delimiter `{}` does not have a corresponding \
                 opening delimiter",
                &source_map[self.span.source_id].content()[self.span.range()]
            )),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The closing delimiter does not match the opening delimiter.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct MismatchedClosingDelimiter {
    /// The span of the closing delimiter.
    pub span: AbsoluteSpan,

    /// The span of the opening delimiter.
    pub opening_span: AbsoluteSpan,

    /// The kind of the closing delimiter.
    pub closing_delimiter: DelimiterKind,

    /// The kind of the opening delimiter.
    pub opening_delimiter: DelimiterKind,
}

impl Report<&SourceMap> for MismatchedClosingDelimiter {
    type Location = ByteIndex;

    fn report(&self, source_map: &SourceMap) -> Diagnostic<Self::Location> {
        Diagnostic {
            span: self.span,
            message: "found a mismatched closing delimiter".to_string(),
            label: Some(format!(
                "this closing delimiter `{}` does not match the opening \
                 delimiter `{}`",
                &source_map[self.span.source_id].content()[self.span.range()],
                &source_map[self.opening_span.source_id].content()
                    [self.opening_span.range()]
            )),
            severity: Severity::Error,
            help_message: Some(format!(
                "replace with `{}` instead",
                match self.opening_delimiter {
                    DelimiterKind::Parenthesis => ')',
                    DelimiterKind::Brace => '}',
                    DelimiterKind::Bracket => ']',
                }
            )),
            related: vec![Related {
                span: self.opening_span,
                message: format!(
                    "has an opening delimiter `{}`",
                    &source_map[self.opening_span.source_id].content()
                        [self.opening_span.range()]
                ),
            }],
        }
    }
}

/// Is an enumeration containing all kinds of lexical errors that can occur
/// while tokenizing the source code.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From,
)]
#[allow(missing_docs)]
pub enum Error {
    UndelimitedDelimiter(UndelimitedDelimiter),
    UnterminatedStringLiteral(UnterminatedStringLiteral),
    InvalidEscapeSequence(InvalidEscapeSequence),
    InvalidIndentation(InvalidIndentation),
    InvalidNewIndentationLevel(InvalidNewIndentationLevel),
    UnexpectedClosingDelimiter(UnexpectedClosingDelimiter),
    MismatchedClosingDelimiter(MismatchedClosingDelimiter),
    ExpectIndentation(ExpectIndentation),
}

impl Error {
    /// Gets the span where the error occurred.
    #[must_use]
    pub const fn span(&self) -> &AbsoluteSpan {
        match self {
            Self::UndelimitedDelimiter(err) => &err.opening_span,
            Self::UnterminatedStringLiteral(err) => &err.span,
            Self::InvalidEscapeSequence(err) => &err.span,
            Self::InvalidIndentation(err) => &err.span,
            Self::InvalidNewIndentationLevel(err) => &err.span,
            Self::UnexpectedClosingDelimiter(err) => &err.span,
            Self::MismatchedClosingDelimiter(err) => &err.span,
            Self::ExpectIndentation(err) => &err.span,
        }
    }
}

impl Report<&SourceMap> for Error {
    type Location = ByteIndex;

    fn report(&self, source_map: &SourceMap) -> Diagnostic<Self::Location> {
        match self {
            Self::UndelimitedDelimiter(err) => err.report(source_map),
            Self::UnterminatedStringLiteral(err) => err.report(source_map),
            Self::InvalidEscapeSequence(err) => err.report(source_map),
            Self::InvalidIndentation(err) => err.report(source_map),
            Self::InvalidNewIndentationLevel(err) => err.report(source_map),
            Self::UnexpectedClosingDelimiter(err) => err.report(source_map),
            Self::MismatchedClosingDelimiter(err) => err.report(source_map),
            Self::ExpectIndentation(err) => err.report(source_map),
        }
    }
}
