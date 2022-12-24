use std::ops::Range;

use pernixc_common::source_file::SourcePosition;

/// Represent an enumeration containing all keywords.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Return,
    Let,
    Var,
    Using,
    Namespace,
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
            Keyword::Var => "var",
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
pub enum LiteralConstantToken<'src> {
    Number {
        value: &'src str,
        literal_suffix: Option<&'src str>,
        is_decimal: bool,
    },
    Boolean(bool),
}

impl<'src> LiteralConstantToken<'src> {
    /// Return a literal suffix of this [`LiteralConstantToken`].
    pub fn get_literal_suffix(&self) -> Option<&'src str> {
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
pub enum TokenKind<'src> {
    Identifier,
    Space,
    Comment,
    Keyword(Keyword),
    LiteralConstant(LiteralConstantToken<'src>),
    Punctuator(char),
    EndOfFile,
}

/// Represent a single token word; containing type of the token and its lexeme.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'src> {
    pub(crate) token_kind: TokenKind<'src>,
    pub(crate) position_range: Range<SourcePosition>,
    pub(crate) lexeme: &'src str,
}

impl<'src> Token<'src> {
    /// Return a reference to the token kind of this [`Token`].
    pub fn token_kind(&self) -> &TokenKind<'src> {
        &self.token_kind
    }

    /// Return a reference to the position range of this [`Token`].
    pub fn position_range(&self) -> &Range<SourcePosition> {
        &self.position_range
    }

    /// Return a reference to the lexeme of this [`Token`].
    pub fn lexeme(&self) -> &'src str {
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
