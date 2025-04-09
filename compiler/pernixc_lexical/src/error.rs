//! Contains all kinds of lexical errors that can occur while tokenizing the
//! source code.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_diagnostic::{Diagnostic, Related, Report, Severity};
use pernixc_source_file::Span;

use crate::token_stream::DelimiterKind;

/// The delimiter is not closed by its corresponding closing pair.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct UndelimitedDelimiter<ID> {
    /// The span of the opening delimiter.
    pub opening_span: Span<ID>,

    /// The kind of the delimiter.
    pub delimiter: DelimiterKind,
}

impl<ID: Clone> Report<()> for UndelimitedDelimiter<ID> {
    type Span = Span<ID>;

    fn report(&self, (): ()) -> Diagnostic<Self::Span> {
        Diagnostic {
            span: self.opening_span.clone(),
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct UnterminatedStringLiteral<ID> {
    /// The span of the unclosed double quote that starts the string literal.
    pub span: Span<ID>,
}

impl<ID: Clone> Report<()> for UnterminatedStringLiteral<ID> {
    type Span = Span<ID>;

    fn report(&self, (): ()) -> Diagnostic<Self::Span> {
        Diagnostic {
            span: self.span.clone(),
            label: Some("need to be closed with a double quote".to_string()),
            message: "found an unterminated string literal".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The source code contains an invalid escape sequence.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct InvalidEscapeSequence<ID> {
    /// The span of the invalid escape sequence (including the backslash).
    pub span: Span<ID>,
}

impl<ID: Clone> Report<()> for InvalidEscapeSequence<ID> {
    type Span = Span<ID>;

    fn report(&self, (): ()) -> Diagnostic<Self::Span> {
        Diagnostic {
            span: self.span.clone(),
            message: "found an invalid escape sequence".to_string(),
            label: None,
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// Found a token in an invalid indentation level.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct InvalidIndentation<ID> {
    /// The span of the invalid indentation.
    pub span: Span<ID>,

    /// The expected indentation level.
    pub expected_indentation: usize,

    /// The indentation level found in the source code.
    pub found_indentation: usize,

    /// The span top the previous indentation starting point
    pub previous_indentation_start: Option<Span<ID>>,
}

impl<ID: Clone> Report<()> for InvalidIndentation<ID> {
    type Span = Span<ID>;

    fn report(&self, (): ()) -> Diagnostic<Self::Span> {
        Diagnostic {
            span: self.span.clone(),
            message: "the token is in an invalid indentation level".to_string(),
            label: Some(format!("found {} space(s)", self.found_indentation,)),
            severity: Severity::Error,
            help_message: None,
            related: self
                .previous_indentation_start
                .clone()
                .map(|span| Related {
                    span,
                    message: format!(
                        "previous indentation level is {} space(s)",
                        self.expected_indentation
                    ),
                })
                .into_iter()
                .collect(),
        }
    }
}

/// The source code contains an invalid new indentation level.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct InvalidNewIndentationLevel<ID> {
    /// The span of the invalid new indentation level.
    pub span: Span<ID>,

    /// The span of the previous indentation level.
    pub previous_indentation_span: Option<Span<ID>>,

    /// The expected indentation level.
    pub latest_indentation: usize,

    /// The indentation level found in the source code.
    pub found_indentation: usize,
}

impl<ID: Clone> Report<()> for InvalidNewIndentationLevel<ID> {
    type Span = Span<ID>;

    fn report(&self, (): ()) -> Diagnostic<Self::Span> {
        Diagnostic {
            span: self.span.clone(),
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
            related: self
                .previous_indentation_span
                .clone()
                .map(|span| Related {
                    span,
                    message: format!(
                        "previous indentation level is {} space(s)",
                        self.latest_indentation
                    ),
                })
                .into_iter()
                .collect(),
        }
    }
}

/// Is an enumeration containing all kinds of lexical errors that can occur
/// while tokenizing the source code.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From,
)]
#[allow(missing_docs)]
pub enum Error<ID> {
    UndelimitedDelimiter(UndelimitedDelimiter<ID>),
    UnterminatedStringLiteral(UnterminatedStringLiteral<ID>),
    InvalidEscapeSequence(InvalidEscapeSequence<ID>),
    InvalidIndentation(InvalidIndentation<ID>),
    InvalidNewIndentationLevel(InvalidNewIndentationLevel<ID>),
}

impl<ID> Error<ID> {
    /// Gets the span where the error occurred.
    #[must_use]
    pub const fn span(&self) -> &Span<ID> {
        match self {
            Self::UndelimitedDelimiter(err) => &err.opening_span,
            Self::UnterminatedStringLiteral(err) => &err.span,
            Self::InvalidEscapeSequence(err) => &err.span,
            Self::InvalidIndentation(err) => &err.span,
            Self::InvalidNewIndentationLevel(err) => &err.span,
        }
    }
}

impl<ID: Clone> Report<()> for Error<ID> {
    type Span = Span<ID>;

    fn report(&self, (): ()) -> Diagnostic<Self::Span> {
        match self {
            Self::UndelimitedDelimiter(err) => err.report(()),
            Self::UnterminatedStringLiteral(err) => err.report(()),
            Self::InvalidEscapeSequence(err) => err.report(()),
            Self::InvalidIndentation(err) => err.report(()),
            Self::InvalidNewIndentationLevel(err) => err.report(()),
        }
    }
}
