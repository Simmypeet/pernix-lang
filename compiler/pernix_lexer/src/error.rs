use pernix_diagnostics::{
    highlight_source_code, print_message, HighlightStyle, Severity,
};
use pernix_project::source_code::{SourceCode, SourcePositionWithoutByteIndex};

/// Represent a struct that contains a [`LexicalError`] and a reference to the
/// source code.
#[derive(Debug, Clone)]
pub struct Error<'a> {
    pub(crate) source_reference: &'a SourceCode,
    pub(crate) lexical_error: LexicalError,
}

impl<'a> Error<'a> {
    /// Return a reference to the source reference of this [`Error`].
    pub fn source_reference(&self) -> &'a SourceCode {
        self.source_reference
    }

    /// Return a reference to the lexical error of this [`Error`].
    pub fn lexical_error(&self) -> &LexicalError {
        &self.lexical_error
    }

    /// Print the error message of this [`Error`]
    pub fn print_error(&self) {
        match &self.lexical_error {
            LexicalError::InvalidCharacter {
                position,
                character,
            } => {
                print_message(
                    format!("`{}` is not a valid character", character)
                        .as_str(),
                    Severity::Error,
                    Some("LXA"),
                );

                highlight_source_code(
                    self.source_reference,
                    HighlightStyle::Position((*position).into()),
                    Some("invalid character found here"),
                )
            }
            LexicalError::UnterminatedMultilineComment {
                multiline_comment_position,
            } => {
                print_message(
                    "multiline comment not terminated",
                    Severity::Error,
                    Some("LXA"),
                );

                let range = {
                    let end = SourcePositionWithoutByteIndex {
                        line: multiline_comment_position.line,
                        column: multiline_comment_position.column + 2,
                    };

                    *multiline_comment_position..end
                };

                highlight_source_code(
                    self.source_reference,
                    HighlightStyle::Range(range),
                    None,
                )
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum LexicalError {
    /// Represent an error when a character is not valid
    InvalidCharacter {
        position: SourcePositionWithoutByteIndex,
        character: char,
    },

    /// Represent an error of an unterminated multiline comment (Started with /* and
    /// not ended with */)
    UnterminatedMultilineComment {
        multiline_comment_position: SourcePositionWithoutByteIndex,
    },
}
