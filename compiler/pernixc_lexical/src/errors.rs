//! Contains the definition of the [`LexicalError`].

use enum_as_inner::EnumAsInner;
use pernixc_common::source_file::Span;
use thiserror::Error;

/// Is an enumeration containing all kinds of lexical errors that can occur while tokenizing the
/// source code.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, Error)]
#[error("Encountered an lexical error while tokenizing the source code.")]
pub enum LexicalError {
    /// The source code contains an invalid escape character sequences.
    ///
    /// The spans represent the locations of the invalid escape character sequences.
    InvalidEscapeCharacterSequences(Vec<Span>),

    /// The source code contains an unclosed double quote.
    UnterminatedStringLiteral(Span),

    /// The source code contains an unclosed `/*` comment.
    UnterminatedDelimitedComment(Span),

    /// The source code contains an empty character literal.
    EmptyCharacterLiteral(Span),

    /// In a character literal, it contains the control character that must be escaped explicitly.
    ControlCharactersMustBeEscaped(Span),
}
