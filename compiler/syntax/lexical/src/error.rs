//! Contains all kinds of lexical errors that can occur while tokenizing the
//! source code.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_diagnostic::{Diagnostic, Related, Report, Severity};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::{AbsoluteSpan, ByteIndex, GlobalSourceID};
use pernixc_stable_hash::StableHash;

use crate::tree::DelimiterKind;

/// The delimiter is not closed by its corresponding closing pair.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct UndelimitedDelimiter {
    /// The span of the opening delimiter.
    pub opening_span: AbsoluteSpan,

    /// The kind of the delimiter.
    pub delimiter: DelimiterKind,
}

impl<T: Send> Report<T> for UndelimitedDelimiter {
    type Location = ByteIndex;

    async fn report(&self, _: T) -> Diagnostic<Self::Location> {
        Diagnostic {
            span: Some((
                self.opening_span,
                Some(format!("need to be cloesd with `{}` pair", match self
                    .delimiter
                {
                    DelimiterKind::Parenthesis => ')',
                    DelimiterKind::Brace => '}',
                    DelimiterKind::Bracket => ']',
                })),
            )),
            message: "found an undelimited delimiter".to_string(),
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
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct UnterminatedStringLiteral {
    /// The span of the unclosed double quote that starts the string literal.
    pub span: AbsoluteSpan,
}

impl<T: Send> Report<T> for UnterminatedStringLiteral {
    type Location = ByteIndex;

    async fn report(&self, _: T) -> Diagnostic<Self::Location> {
        Diagnostic {
            span: Some((
                self.span,
                Some("need to be closed with a double quote".to_string()),
            )),
            message: "found an unterminated string literal".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The source code contains an invalid escape sequence.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct InvalidEscapeSequence {
    /// The span of the invalid escape sequence (including the backslash).
    pub span: AbsoluteSpan,
}

impl<T: Send> Report<T> for InvalidEscapeSequence {
    type Location = ByteIndex;

    async fn report(&self, _: T) -> Diagnostic<Self::Location> {
        Diagnostic {
            span: Some((self.span, None)),
            message: "found an invalid escape sequence".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The available indentation level that can be used.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct AvailableIndentation {
    /// The span of the colon that starts the indentation level.
    pub colon_span: AbsoluteSpan,

    /// The indentation level of that particular indentation.
    pub indentation_size: usize,
}

/// Found a token in an invalid indentation level.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct InvalidIndentation {
    /// The span of the invalid indentation.
    pub span: AbsoluteSpan,

    /// The indentation level found in the source code.
    pub found_indentation: usize,

    /// The available indentation level that can be used.
    pub available_indentations: Vec<AvailableIndentation>,
}

impl<T: Send> Report<T> for InvalidIndentation {
    type Location = ByteIndex;

    async fn report(&self, _: T) -> Diagnostic<Self::Location> {
        Diagnostic {
            span: Some((
                self.span,
                Some(format!("found {} space(s)", self.found_indentation)),
            )),
            message: "the token is in an invalid indentation level".to_string(),
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
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct ExpectIndentation {
    /// The span of the expected indentation.
    pub span: AbsoluteSpan,

    /// The span to the colon that starts the indentation level.
    pub indentation_start: AbsoluteSpan,
}

impl<
        'a,
        T: codespan_reporting::files::Files<'a, FileId = GlobalSourceID>
            + Send
            + Sync,
    > Report<&'a T> for ExpectIndentation
{
    type Location = ByteIndex;

    async fn report(&self, source_map: &'a T) -> Diagnostic<Self::Location> {
        let source_file = source_map
            .source(self.span.source_id)
            .expect("source map should contain the source code");

        Diagnostic {
            span: Some((
                self.span,
                Some(format!(
                    "`{}` is not indented",
                    &source_file.as_ref()[self.span.range()]
                )),
            )),
            message: "expect an indentation".to_string(),
            severity: Severity::Error,
            help_message: Some(format!(
                "add spaces before `{}` to indent",
                &source_file.as_ref()[self.indentation_start.range()]
            )),
            related: vec![Related {
                span: self.indentation_start,
                message: "this colon starts the indentation level".to_string(),
            }],
        }
    }
}

/// The source code contains an invalid new indentation level.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
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

impl<T: Send> Report<T> for InvalidNewIndentationLevel {
    type Location = ByteIndex;

    async fn report(&self, _: T) -> Diagnostic<Self::Location> {
        Diagnostic {
            span: Some((
                self.span,
                Some(format!(
                    "found {} space(s), but the previous indentation level is \
                     {} space(s)",
                    self.found_indentation, self.latest_indentation
                )),
            )),
            message: "found an invalid new indentation level".to_string(),
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
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct UnexpectedClosingDelimiter {
    /// The span of the closing delimiter.
    pub span: AbsoluteSpan,

    /// The kind of the closing delimiter.
    pub closing_delimiter: DelimiterKind,
}

impl<T: Send> Report<T> for UnexpectedClosingDelimiter {
    type Location = ByteIndex;

    async fn report(&self, _: T) -> Diagnostic<Self::Location> {
        Diagnostic {
            span: Some((
                self.span,
                Some(format!(
                    "this closing delimiter `{}` does not have a \
                     corresponding opening delimiter",
                    self.closing_delimiter.closing_character()
                )),
            )),
            message: "found an unexpected closing delimiter".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The closing delimiter does not match the opening delimiter.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
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

impl<T: Send> Report<T> for MismatchedClosingDelimiter {
    type Location = ByteIndex;

    async fn report(&self, _: T) -> Diagnostic<Self::Location> {
        let opening_delimiter_p = self.opening_delimiter.opening_character();
        let closing_delimiter_p = self.closing_delimiter.closing_character();
        Diagnostic {
            span: Some((
                self.span,
                Some(format!(
                    "this closing delimiter `{closing_delimiter_p}` does not \
                     match the opening delimiter `{opening_delimiter_p}`",
                )),
            )),
            message: "found a mismatched closing delimiter".to_string(),
            severity: Severity::Error,
            help_message: Some(format!(
                "replace with `{}` instead",
                self.opening_delimiter.closing_character()
            )),
            related: vec![Related {
                span: self.opening_span,
                message: format!(
                    "has an opening delimiter `{opening_delimiter_p}`",
                ),
            }],
        }
    }
}

/// Is an enumeration containing all kinds of lexical errors that can occur
/// while tokenizing the source code.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    From,
    Serialize,
    Deserialize,
    StableHash,
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

impl<
        'a,
        T: codespan_reporting::files::Files<'a, FileId = GlobalSourceID>
            + Send
            + Sync,
    > Report<&'a T> for Error
{
    type Location = ByteIndex;

    async fn report(&self, source_map: &'a T) -> Diagnostic<Self::Location> {
        match self {
            Self::UndelimitedDelimiter(err) => err.report(source_map).await,
            Self::UnterminatedStringLiteral(err) => {
                err.report(source_map).await
            }
            Self::InvalidEscapeSequence(err) => err.report(source_map).await,
            Self::InvalidIndentation(err) => err.report(source_map).await,
            Self::InvalidNewIndentationLevel(err) => {
                err.report(source_map).await
            }
            Self::UnexpectedClosingDelimiter(err) => {
                err.report(source_map).await
            }
            Self::MismatchedClosingDelimiter(err) => {
                err.report(source_map).await
            }
            Self::ExpectIndentation(err) => err.report(source_map).await,
        }
    }
}
