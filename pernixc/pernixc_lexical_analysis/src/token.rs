use std::ops::Range;

use pernixc_common::source_file::TextPosition;

/// Is an enumeration of all the keywords defined in the language.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Keyword {
    /// `new` keyword.
    New,

    /// `export` keyword.
    Export,

    /// `mutable` keyword.
    Mutable,

    /// `let` keyword.
    Let,

    /// `var` keyword.
    Var,

    /// `public` keyword.
    Public,

    /// `private` keyword.
    Private,

    /// `function` keyword.
    Function,

    /// `module` keyword.
    Module,

    /// `using` keyword
    Using,

    /// `void` keyword.
    Void,

    /// `int8` keyword.
    Int8,

    /// `int16` keyword.
    Int16,

    /// `int32` keyword.
    Int32,

    /// `int64` keyword.
    Int64,

    /// `uint8` keyword.
    Uint8,

    /// `uint16` keyword.
    Uint16,

    /// `uint32` keyword.
    Uint32,

    /// `uint64` keyword.
    Uint64,

    /// `float32` keyword.
    Float32,

    /// `float64` keyword.
    Float64,

    /// `bool` keyword.
    Bool,

    /// `if` keyword.
    If,

    /// `else` keyword.
    Else,

    /// `while` keyword.
    While,

    /// `break` keyword.
    Break,

    /// `continue` keyword.
    Continue,

    /// `return` keyword.
    Return,

    /// `class` keyword.
    Class,

    /// `import` keyword.
    Import,
}

/// Is an enumeration of all number literal suffixes defined in the language.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NumberLiteralSuffix {
    /// `i8` suffix.
    Int8,

    /// `i16` suffix.
    Int16,

    /// `i32` suffix.
    Int32,

    /// `i64` suffix.
    Int64,

    /// `u8` suffix.
    Uint8,

    /// `u16` suffix.
    Uint16,

    /// `u32` suffix.
    Uint32,

    /// `u64` suffix.
    Uint64,

    /// `f32` suffix.
    Float32,

    /// `f64` suffix.
    Float64,
}

/// Is an enumeration of all literal types defined in the language.
#[derive(Debug, Clone)]
pub enum LiteralToken<'src> {
    /// Is a number literal
    NumberLiteral {
        value: &'src str,
        suffix: Option<NumberLiteralSuffix>,
        is_decimal: bool,
    },

    /// Is a boolean literal (`true` or `false`).
    BoolLiteral(bool),
}

/// Is an enumeration of all token types defined in the language.
#[derive(Debug, Clone)]
pub enum TokenType<'src> {
    /// Is a string used to identify a particular symbol.
    Identifier(&'src str),

    /// Is a keyword
    Keyword(Keyword),

    /// Is a punctuation symbol (ASCII character).
    Punctuation(char),

    /// Is a literal value
    Literal(LiteralToken<'src>),

    /// Represents a sequence of whitespace characters.
    Space,

    /// Represents a sequence of comments
    Comment(&'src str),

    /// Is a special kind of token that is used to indicate the end of the file.
    EndOfFile,
}

/// Is a structure that represents a single token resulted from the tokenization process.
#[derive(Debug, Clone)]
pub struct Token<'src> {
    pub token_type: TokenType<'src>,
    pub position_range: Range<TextPosition>,
    pub byte_index_range: Range<usize>,
}
