//! Contains all kinds of lexical errors that can occur while tokenizing the
//! source code.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_diagnostic::{ByteIndex, Highlight, Rendered, Report, Severity};
use pernixc_query::{TrackedEngine, runtime::executor};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::AbsoluteSpan;
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

impl Report for UndelimitedDelimiter {
    async fn report(
        &self,
        _: &TrackedEngine,
    ) -> Result<Rendered<ByteIndex>, executor::CyclicError> {
        Ok(Rendered {
            primary_highlight: Some(Highlight::new(
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
        })
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

impl Report for UnterminatedStringLiteral {
    async fn report(
        &self,
        _: &TrackedEngine,
    ) -> Result<Rendered<ByteIndex>, executor::CyclicError> {
        Ok(Rendered {
            primary_highlight: Some(Highlight::new(
                self.span,
                Some("need to be closed with a double quote".to_string()),
            )),
            message: "found an unterminated string literal".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
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

impl Report for InvalidEscapeSequence {
    async fn report(
        &self,
        _: &TrackedEngine,
    ) -> Result<Rendered<ByteIndex>, executor::CyclicError> {
        Ok(Rendered {
            primary_highlight: Some(Highlight::new(self.span, None)),
            message: "found an invalid escape sequence".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
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

impl Report for InvalidIndentation {
    async fn report(
        &self,
        _: &TrackedEngine,
    ) -> Result<Rendered<ByteIndex>, executor::CyclicError> {
        Ok(Rendered {
            primary_highlight: Some(Highlight::new(
                self.span,
                Some(format!("found {} space(s)", self.found_indentation)),
            )),
            message: "the token is in an invalid indentation level".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: self
                .available_indentations
                .iter()
                .map(|span| {
                    Highlight::new(
                        span.colon_span,
                        Some(format!(
                            "previous indentation level is {} space(s)",
                            span.indentation_size
                        )),
                    )
                })
                .collect(),
        })
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

impl Report for ExpectIndentation {
    async fn report(
        &self,
        _: &TrackedEngine,
    ) -> Result<Rendered<ByteIndex>, executor::CyclicError> {
        Ok(Rendered {
            primary_highlight: Some(Highlight::new(
                self.span,
                Some("this is not indented".to_string()),
            )),
            message: "expect an indentation".to_string(),
            severity: Severity::Error,
            help_message: Some("add spaces before this to indent".to_string()),
            related: vec![Highlight::new(
                self.indentation_start,
                Some("this colon starts the indentation level".to_string()),
            )],
        })
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

impl Report for InvalidNewIndentationLevel {
    async fn report(
        &self,
        _: &TrackedEngine,
    ) -> Result<Rendered<ByteIndex>, executor::CyclicError> {
        Ok(Rendered {
            primary_highlight: Some(Highlight::new(
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
            related: vec![Highlight::new(
                self.previous_indentation_span,
                Some(format!(
                    "previous indentation level is {} space(s)",
                    self.latest_indentation
                )),
            )],
        })
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

impl Report for UnexpectedClosingDelimiter {
    async fn report(
        &self,
        _: &TrackedEngine,
    ) -> Result<Rendered<ByteIndex>, executor::CyclicError> {
        Ok(Rendered {
            primary_highlight: Some(Highlight::new(
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
        })
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

impl Report for MismatchedClosingDelimiter {
    async fn report(
        &self,
        _: &TrackedEngine,
    ) -> Result<Rendered<ByteIndex>, executor::CyclicError> {
        let opening_delimiter_p = self.opening_delimiter.opening_character();
        let closing_delimiter_p = self.closing_delimiter.closing_character();
        Ok(Rendered {
            primary_highlight: Some(Highlight::new(
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
            related: vec![Highlight::new(
                self.opening_span,
                Some(format!(
                    "has an opening delimiter `{opening_delimiter_p}`",
                )),
            )],
        })
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

impl Report for Error {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<Rendered<ByteIndex>, executor::CyclicError> {
        match self {
            Self::UndelimitedDelimiter(err) => err.report(engine).await,
            Self::UnterminatedStringLiteral(err) => err.report(engine).await,
            Self::InvalidEscapeSequence(err) => err.report(engine).await,
            Self::InvalidIndentation(err) => err.report(engine).await,
            Self::InvalidNewIndentationLevel(err) => err.report(engine).await,
            Self::UnexpectedClosingDelimiter(err) => err.report(engine).await,
            Self::MismatchedClosingDelimiter(err) => err.report(engine).await,
            Self::ExpectIndentation(err) => err.report(engine).await,
        }
    }
}
