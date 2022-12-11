use std::ops::Range;

use pernix_project::source_code::SourcePosition;

/// Represent an enumeration containing all keywords. 
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Return,
    Let,
    Using,
    Namespace,
    Mutable,
    If,
    Else,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LiteralConstantType<'a> {
    Integer {
        value: &'a str,
        literal_suffix: Option<&'a str>,
    },
    Float {
        value: &'a str,
        literal_suffix: Option<&'a str>,
    },
    Boolean(bool),
}

/// Represent an enumeration containing all patterns of tokens.
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

/// Represent a single token word; containing type of the token and its lexeme.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    token_kind: TokenKind<'a>,
    position_range: Range<SourcePosition>,
    lexeme: &'a str,
}

impl<'a> Token<'a> {
    // Create a new [`Token`].
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

    /// Return a reference to the token kind of this [`Token`].
    pub fn token_kind(&self) -> &TokenKind<'a> {
        &self.token_kind
    }

    /// Return a reference to the position range of this [`Token`].
    pub fn position_range(&self) -> &Range<SourcePosition> {
        &self.position_range
    }

    /// Return a reference to the lexeme of this [`Token`].
    pub fn lexeme(&self) -> &'a str {
        self.lexeme
    }

    /// Return a boolean indicating whether this [`Token`] is a significant.
    /// Insignificant tokens are spaces and comments.
    pub fn is_significant_token(&self) -> bool {
        match self.token_kind() {
            TokenKind::Space | TokenKind::Comment => false,
            _ => true,
        }
    }
}
