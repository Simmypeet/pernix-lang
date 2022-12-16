use pernix_project::source_code::SourcePosition;

/// Represent a lexical error caused by an invalid character
#[derive(Debug, Clone)]
pub struct InvalidCharacter {
    pub position: SourcePosition,
    pub character: char,
}

/// Represent a lexical error caused by an unterminated multiline comment
#[derive(Debug, Clone)]
pub struct UnterminatedMultilineComment {
    pub multiline_comment_position: SourcePosition,
}

/// Describe an enumeration containing all lexical errors
#[derive(Debug, Clone)]
pub enum Error {
    InvalidCharacter(InvalidCharacter),
    UnterminatedMultilineComment(UnterminatedMultilineComment),
}
