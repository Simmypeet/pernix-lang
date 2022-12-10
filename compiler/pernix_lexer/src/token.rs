use std::ops::Range;

use pernix_project::source_code::SourcePosition;

/// List of all reserved keywords used by the language
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Return,
    Let,
    Using,
    Namespace,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LiteralConstantType<'a> {
    Integer {
        value: &'a str,
        literal_prefix: Option<&'a str>,
    },
    Float {
        value: &'a str,
        literal_prefix: Option<&'a str>,
    },
    Boolean(bool),
}

/// Enumeration containing all patterns of token
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind<'a> {
    Identifier,
    Space,
    Comment,
    Keyword(Keyword),
    LiteralConstant(LiteralConstantType<'a>),
    Punctuator(char),
    EndOfFile,
}

/// Represents a single token word; containing type of the token and its lexeme
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    token_kind: TokenKind<'a>,
    position_range: Range<SourcePosition>,
    lexeme: &'a str,
}

impl<'a> Token<'a> {
    /// Creates a new [`Token`].
    pub(crate) fn new(
        token_kind: TokenKind<'a>,
        position_range: Range<SourcePosition>,
        lexeme: &'a str,
    ) -> Self {
        Self {
            token_kind,
            position_range,
            lexeme,
        }
    }

    /// Returns a reference to the token kind of this [`Token`].
    pub fn token_kind(&self) -> &TokenKind {
        &self.token_kind
    }

    /// Returns a reference to the position range of this [`Token`].
    pub fn position_range(&self) -> &Range<SourcePosition> {
        &self.position_range
    }

    /// Returns a reference to the lexeme of this [`Token`].
    pub fn lexeme(&self) -> &str {
        self.lexeme
    }

    /// Returns a boolean indicating whether this [`Token`] is a significant.
    /// Insignificant tokens are spaces and comments.
    pub fn is_significant_token(&self) -> bool {
        match self.token_kind() {
            TokenKind::Space | TokenKind::Comment => false,
            _ => true,
        }
    }
}
