use std::ops::Range;

use pernixc_common::{
    printing::{highlight_source_file, print_message, HighlightStyle, Severity},
    source_file::{SourceFile, TextPosition},
};

/// Is an enumeration of all the possible errors that can occur during the lexical analysis phase.
#[derive(Debug, Clone)]
pub enum LexicalError {
    /// Represents an error when encountering an invalid character.
    InvalidCharacter {
        position: TextPosition,
        character: char,
    },

    /// Represents an error of an unterminated multiline comment (Started with /* and  not ended
    /// with */)
    UnterminatedMultilineComment {
        multiline_comment_position: TextPosition,
    },

    /// Represents an error when encountering an invalid literal suffix.
    InvalidLiteralSuffix {
        literal_suffix_position: Range<TextPosition>,
    },
}

impl LexicalError {
    /// Returns the error number of this [`LexicalError`].
    pub fn get_error_number(&self) -> usize {
        match self {
            LexicalError::InvalidCharacter { .. } => 0,
            LexicalError::UnterminatedMultilineComment { .. } => 1,
            LexicalError::InvalidLiteralSuffix { .. } => 2,
        }
    }

    /// Prints the error message of this [`LexicalError`].
    pub fn print_error(&self, source_file_reference: &SourceFile) {
        // 0 -> LXA000
        // 1 -> LXA001
        // 2 -> LXA002
        // ...
        let category = format!("LXA{:03}", self.get_error_number());

        match &self {
            LexicalError::InvalidCharacter {
                position,
                character,
            } => {
                print_message(
                    format!("`{}` is not a valid character", character).as_str(),
                    Severity::Error,
                    Some(&category),
                );

                highlight_source_file(
                    source_file_reference,
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

                highlight_source_file(source_file_reference, HighlightStyle::Range(range), None)
            }
            LexicalError::InvalidLiteralSuffix {
                literal_suffix_position,
            } => {
                print_message("invalid literal suffix", Severity::Error, Some(&category));

                highlight_source_file(
                    source_file_reference,
                    HighlightStyle::Range(literal_suffix_position.clone()),
                    None,
                )
            }
        }
    }
}
