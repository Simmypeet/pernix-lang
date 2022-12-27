use pernixc_common::{
    printing::{
        highlight_source_file, print_message, HighlightStyle, Severity,
    },
    source_file::{SourceFile, TextPosition},
};

/// Represent a struct that contains a [`LexicalError`] and a reference to the
/// source file.
#[derive(Debug, Clone)]
pub struct Error<'src> {
    pub source_file: &'src SourceFile,
    pub lexical_error: LexicalError,
}

impl<'src> Error<'src> {
    /// Print the error message of this [`Error`]
    pub fn print_error(&self) {
        // 0 -> LXA000
        // 1 -> LXA001
        // 2 -> LXA002
        // ...
        let category =
            format!("LXA{:03}", self.lexical_error.get_error_number());

        match &self.lexical_error {
            LexicalError::InvalidCharacter {
                position,
                character,
            } => {
                print_message(
                    format!("`{}` is not a valid character", character)
                        .as_str(),
                    Severity::Error,
                    Some(&category),
                );

                highlight_source_file(
                    self.source_file,
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
                    Some(&category),
                );

                let range = {
                    let end = TextPosition {
                        line: multiline_comment_position.line,
                        column: multiline_comment_position.column + 2,
                    };

                    *multiline_comment_position..end
                };

                highlight_source_file(
                    self.source_file,
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
        position: TextPosition,
        character: char,
    },

    /// Represent an error of an unterminated multiline comment (Started with /* and
    /// not ended with */)
    UnterminatedMultilineComment {
        multiline_comment_position: TextPosition,
    },
}

impl LexicalError {
    /// Return the error number of this [`LexicalError`].
    pub fn get_error_number(&self) -> usize {
        match self {
            LexicalError::InvalidCharacter { .. } => 0,
            LexicalError::UnterminatedMultilineComment { .. } => 1,
        }
    }
}
