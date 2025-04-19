//! Contains all the kinds of tokens in the Pernix programming language.

use std::{collections::HashMap, str::FromStr, sync::LazyLock};

use derive_more::From;
use enum_as_inner::EnumAsInner;
use serde::{Deserialize, Serialize};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use thiserror::Error;

/// Is an enumeration representing keywords in the Pernix programming language.
///
/// Most enum variants names are the same as the keyword they represent, except
/// that the name is capitalized while the keyword is not. For example, the
/// `function` keyword is represented by the `Function` variant.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumIter,
    Serialize,
    Deserialize,
)]
pub enum Keyword {
    /// `match` keyword.
    Match,
    /// `public` keyword.
    Public,
    /// `struct` keyword.
    Struct,
    /// `implements` keyword.
    Implements,
    /// `let` keyword.
    Let,
    /// `const` keyword.
    Const,
    /// `if` keyword.
    If,
    /// `else` keyword.
    Else,
    /// `mut` keyword.
    Mut,
    /// `not` keyword.
    Not,
    /// `while` keyword.
    While,
    /// `break` keyword.
    Break,
    /// `continue` keyword.
    Continue,
    /// `return` keyword.
    Return,
    /// `true` keyword.
    True,
    /// `false` keyword.
    False,
    /// `bool` keyword.
    Bool,
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
    /// `usize` keyword.
    Usize,
    /// `isize` keyword.
    Isize,
    /// `and` keyword.
    And,
    /// `or` keyword.
    Or,
    /// `loop` keyword.
    Loop,
    /// `express` keyword.
    Express,
    /// `enum` keyword.
    Enum,
    /// `private` keyword.
    Private,
    /// `internal` keyword.
    Internal,
    /// `module` keyword.
    Module,
    /// `as` keyword.
    As,
    /// `type` keyword.
    Type,
    /// `static` keyword.
    Static,
    /// `where` keyword.
    Where,
    /// `trait` keyword.
    Trait,
    /// `import` keyword.
    Import,
    /// `function` keyword.
    Function,
    /// `unsafe` keyword.
    Unsafe,
    /// `for` keyword
    For,
    /// `delete` keyword
    Delete,
    /// `tuple` keyword
    Tuple,
    /// `case` keyword
    Case,
    /// `phantom` keyword
    Phantom,
    /// `final` keyword
    Final,
    /// `extern` keyword
    Extern,
    /// `effect` keyword
    Effect,
    /// `do` keyword
    Do,
    /// `try` keywrod
    Try,
    /// `with` keyword
    With,
    /// `null` keyword.
    Null,
    /// `super` keyword.
    Super,
    /// `target` keyword.
    Target,
    /// `this` keyword.
    This,
    /// `from` keyword.
    From,
    /// `marker` keyword.
    Marker,
    /// `panic` keyword.
    Panic,
    /// `pass` keyword.
    Pass,
    /// `scope` keyword.
    Scope,
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// A static map that maps a string representation of a keyword to its
/// [`Keyword`].
static STRING_KEYWORD_MAP: LazyLock<HashMap<&'static str, Keyword>> =
    LazyLock::new(|| {
        let mut map = HashMap::new();

        for keyword in Keyword::iter() {
            map.insert(keyword.as_str(), keyword);
        }

        map
    });

/// Is an error that is returned when a string cannot be parsed into a
/// [`Keyword`] in [`FromStr`] trait implementation.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Error,
    Serialize,
    Deserialize,
)]
#[error("invalid string representation of keyword.")]
pub struct KeywordParseError;

impl FromStr for Keyword {
    type Err = KeywordParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        STRING_KEYWORD_MAP.get(s).copied().ok_or(KeywordParseError)
    }
}

impl Keyword {
    /// Gets the string representation of the keyword as a `&str`.
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::For => "for",
            Self::Function => "function",
            Self::As => "as",
            Self::Enum => "enum",
            Self::Struct => "struct",
            Self::Express => "express",
            Self::Loop => "loop",
            Self::Public => "public",
            Self::Implements => "implements",
            Self::Let => "let",
            Self::Const => "const",
            Self::If => "if",
            Self::Else => "else",
            Self::While => "while",
            Self::Mut => "mut",
            Self::Break => "break",
            Self::Continue => "continue",
            Self::Return => "return",
            Self::True => "true",
            Self::False => "false",
            Self::Bool => "bool",
            Self::Int8 => "int8",
            Self::Int16 => "int16",
            Self::Int32 => "int32",
            Self::Int64 => "int64",
            Self::Uint8 => "uint8",
            Self::Uint16 => "uint16",
            Self::Uint32 => "uint32",
            Self::Uint64 => "uint64",
            Self::Float32 => "float32",
            Self::Float64 => "float64",
            Self::Usize => "usize",
            Self::Isize => "isize",
            Self::And => "and",
            Self::Or => "or",
            Self::Private => "private",
            Self::Internal => "internal",
            Self::Module => "module",
            Self::Delete => "delete",
            Self::Type => "type",
            Self::Static => "static",
            Self::Where => "where",
            Self::Trait => "trait",
            Self::Import => "import",
            Self::Unsafe => "unsafe",
            Self::Match => "match",
            Self::Tuple => "tuple",
            Self::Case => "case",
            Self::Phantom => "phantom",
            Self::Final => "final",
            Self::Extern => "extern",
            Self::Effect => "effect",
            Self::Do => "do",
            Self::Try => "try",
            Self::With => "with",
            Self::Null => "null",
            Self::Super => "super",
            Self::Target => "target",
            Self::This => "this",
            Self::From => "from",
            Self::Marker => "marker",
            Self::Panic => "panic",
            Self::Pass => "pass",
            Self::Not => "not",
            Self::Scope => "scope",
        }
    }
}

/// A tag struct that represents a new line token: LF or CRLF.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
)]
pub struct NewLine;

/// A tag struct that represents a character token: 'x'.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
)]
pub struct Character;

/// Represents a hardcoded string literal value in the source code.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
)]
pub struct String;

/// Represents a sequence of characters that can be used to name things in the
/// source code: `struct IDENTIFIER`.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
)]
pub struct Identifier;

/// A type alias of `char` used to represent a punctuation character in the
/// source code like `+`, `-`, `*`, `/`, etc.
pub type Punctuation = char;

/// Represents a sequence of digits that can be used to represent a number in
/// the source code: `let x = 123;`.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
)]
pub struct Numeric;

/// An enumeration of all possible kinds of tokens in the Pernix programming
/// language.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    From,
    EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Kind {
    Keyword(Keyword),
    NewLine(NewLine),
    Character(Character),
    String(String),
    Identifier(Identifier),
    Punctuation(Punctuation),
    Numeric(Numeric),
}
