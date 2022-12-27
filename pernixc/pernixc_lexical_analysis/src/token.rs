use std::ops::Range;

use pernixc_common::source_file::SourcePosition;

/// Represent an enumeration containing all keywords.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Return,
    Let,
    Public,
    Private,
    Using,
    Namespace,
    If,
    Else,
    While,
    Break,
    Continue,
    Mutable,
    New,
    Int8,
    Int16,
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Float32,
    Float64,
    Bool,
    Void,
    Class,
}

impl Keyword {
    /// Return a string representation of this [`Keyword`].
    pub fn get_keyword_string(&self) -> &'static str {
        match self {
            Keyword::Return => "return",
            Keyword::Using => "using",
            Keyword::Namespace => "namespace",
            Keyword::Let => "let",
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::While => "while",
            Keyword::Break => "break",
            Keyword::Continue => "continue",
            Keyword::Public => "public",
            Keyword::Class => "class",
            Keyword::Private => "private",
            Keyword::Mutable => "mutable",
            Keyword::New => "new",
            Keyword::Int8 => "int8",
            Keyword::Int16 => "int16",
            Keyword::Int32 => "int32",
            Keyword::Int64 => "int64",
            Keyword::Uint8 => "uint8",
            Keyword::Uint16 => "uint16",
            Keyword::Uint32 => "uint32",
            Keyword::Uint64 => "uint64",
            Keyword::Float32 => "float32",
            Keyword::Float64 => "float64",
            Keyword::Bool => "bool",
            Keyword::Void => "void",
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

impl<'src> TokenKind<'src> {
    /// Return a boolean indicating whether this [`TokenKind`] is a significant
    /// token or not.
    pub fn is_significant_token(&self) -> bool {
        match self {
            TokenKind::Space | TokenKind::Comment => false,
            _ => true,
        }
    }
}

/// Represent a single token word; containing type of the token and its lexeme.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'src> {
    pub token_kind: TokenKind<'src>,
    pub position_range: Range<SourcePosition>,
    pub lexeme: &'src str,
}

impl<'src> Token<'src> {
    /// Return a boolean indicating whether this [`Token`] is a significant.
    /// Insignificant tokens are spaces and comments.
    pub fn is_significant_token(&self) -> bool {
        match self.token_kind {
            TokenKind::Space | TokenKind::Comment => false,
            _ => true,
        }
    }
}
