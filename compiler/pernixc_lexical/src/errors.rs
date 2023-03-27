//! Contains all kinds of lexical errors that can occur while tokenizing the source code.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_common::source_file::Span;
use thiserror::Error;

/// The source code contains an invalid escape character sequences.
///
/// The spans represent the locations of the invalid escape character sequences.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InvalidEscapeCharacterSequences {
    /// The spans represent the locations of the invalid escape character sequences.
    pub spans: Vec<Span>,
}

/// The source code contains an unclosed double quote.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnterminatedStringLiteral {
    /// The span of the unclosed double quote.
    pub span: Span,
}

/// The source code contains an unclosed `/*` comment.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnterminatedDelimitedComment {
    /// The span of the unclosed `/*` comment.
    pub span: Span,
}

/// The source code contains an empty character literal.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EmptyCharacterLiteral {
    /// The span of the empty character literal.
    pub span: Span,
}

/// In a character literal, it contains the control character that must be escaped explicitly.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ControlCharactersMustBeEscaped {
    /// The span of the control character that must be escaped explicitly.
    pub span: Span,
}

/// Is an enumeration containing all kinds of lexical errors that can occur while tokenizing the
/// source code.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, Error, From)]
#[error("Encountered an lexical error while tokenizing the source code.")]
pub enum LexicalError {
    /// The source code contains an invalid escape character sequences.
    InvalidEscapeCharacterSequences(InvalidEscapeCharacterSequences),
    /// The source code contains an unclosed double quote.
    UnterminatedStringLiteral(UnterminatedStringLiteral),
    /// The source code contains an unclosed `/*` comment.
    UnterminatedDelimitedComment(UnterminatedDelimitedComment),
    /// The source code contains an empty character literal.
    EmptyCharacterLiteral(EmptyCharacterLiteral),
    /// In a character literal, it contains the control character that must be escaped explicitly.
    ControlCharactersMustBeEscaped(ControlCharactersMustBeEscaped),
}
