use std::ops::Range;

use pernix_project::source_code::SourcePosition;

/// List of all reserved keywords used by the language
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Return,
}

/// Enumeration containing all patterns of token
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Identifier,
    Space,
    Comment,
    Keyword(Keyword),
    LiteralConstant,
    Punctuator(char),
    EndOfFile,
}

/// Represents a single token word; containing type of the token and its lexeme
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    pattern: Pattern,
    position_range: Range<SourcePosition>,
    lexeme: &'a str,
}

impl<'a> Token<'a> {
    /// Creates a new [`Token`].
    pub(crate) fn new(
        pattern: Pattern,
        position_range: Range<SourcePosition>,
        lexeme: &'a str,
    ) -> Self {
        Self {
            pattern,
            position_range,
            lexeme,
        }
    }

    /// Returns a reference to the pattern of this [`Token`].
    pub fn pattern(&self) -> &Pattern {
        &self.pattern
    }

    /// Returns a reference to the position range of this [`Token`].
    pub fn position_range(&self) -> &Range<SourcePosition> {
        &self.position_range
    }

    /// Returns a reference to the lexeme of this [`Token`].
    pub fn lexeme(&self) -> &str {
        self.lexeme
    }
}
