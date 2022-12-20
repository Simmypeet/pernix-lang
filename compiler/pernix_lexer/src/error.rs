use pernix_project::source_code::SourcePosition;

/// Represent an enumeration containing all lexical errors
#[derive(Debug, Clone)]
pub enum Error {
    InvalidCharacter {
        position: SourcePosition,
        character: char,
    },
    UnterminatedMultilineComment {
        multiline_comment_position: SourcePosition,
    },
}
