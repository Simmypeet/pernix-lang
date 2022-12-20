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
    While,
    Break,
    Continue,
}

impl Keyword {
    /// Return a string representation of this [`Keyword`].
    pub fn get_keyword_string(&self) -> &'static str {
        match self {
            Keyword::Return => "return",
            Keyword::Let => "let",
            Keyword::Using => "using",
            Keyword::Namespace => "namespace",
            Keyword::Mutable => "mutable",
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::While => "while",
            Keyword::Break => "break",
            Keyword::Continue => "continue",
        }
    }
}

/// Represent an enumeration containing all literal constant types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LiteralConstantToken<'a> {
    Number {
        value: &'a str,
        literal_suffix: Option<&'a str>,
        is_decimal: bool,
    },
    Boolean(bool),
}

impl<'a> LiteralConstantToken<'a> {
    /// Return a literal suffix of this [`LiteralConstantToken`].
    pub fn get_literal_suffix(&self) -> Option<&'a str> {
        match self {
            LiteralConstantToken::Number { literal_suffix, .. } => {
                *literal_suffix
            }
            LiteralConstantToken::Boolean(_) => None,
        }
    }
}

/// Represent an enumeration containing all patterns of tokens.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind<'a> {
    Identifier,
    Space,
    Comment,
    Keyword(Keyword),
    LiteralConstant(LiteralConstantToken<'a>),
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
