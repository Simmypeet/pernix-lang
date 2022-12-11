use pernix_project::source_code::{SourceCode, SourcePosition};

/// Represent a lexical error caused by an invalid character
#[derive(Debug, Clone)]
pub struct InvalidCharacter<'a> {
    pub position: SourcePosition,
    pub character: char,
    pub source_refernece: &'a SourceCode,
}

/// Represent a lexical error caused by an unterminated multiline comment
#[derive(Debug, Clone)]
pub struct UnterminatedMultilineComment<'a> {
    pub multiline_comment_position: SourcePosition,
    pub source_refernece: &'a SourceCode,
}

/// Describe an enumeration containing all lexical errors
#[derive(Debug, Clone)]
pub enum Error<'a> {
    InvalidCharacter(InvalidCharacter<'a>),
    UnterminatedMultilineComment(UnterminatedMultilineComment<'a>),
}
