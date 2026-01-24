//! Contains all the kinds of tokens in the Pernix programming language.

use std::{
    collections::HashMap,
    fmt::{Display, Write},
    str::FromStr,
    sync::LazyLock,
};

use derive_more::{Deref, DerefMut, From};
use enum_as_inner::EnumAsInner;
use qbice::{Decode, Encode, StableHash, storage::intern::Interned};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use thiserror::Error;

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

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
    Encode,
    Decode,
    StableHash,
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
    /// `resume` keyword.
    Resume,
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
    Encode,
    Decode,
    StableHash,
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
    // skipcq: RS-R1000
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
            Self::Resume => "resume",
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
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
    Encode,
    Decode,
    StableHash,
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
    Encode,
    Decode,
    Deref,
    DerefMut,
    StableHash,
)]
pub struct Character(pub char);

/// Represents a hardcoded string literal value in the source code.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    Deref,
    DerefMut,
    StableHash,
)]
pub struct String(pub Interned<str>);

/// Represents a sequence of characters that can be used to name things in the
/// source code: `struct IDENTIFIER`.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    Deref,
    DerefMut,
    StableHash,
)]
pub struct Identifier(pub Interned<str>);

impl Identifier {
    /// Checks if the given character can be used as the first character in an
    /// identifier.
    #[must_use]
    pub fn is_first_identifier_character(c: char) -> bool {
        c == '_'
            || (!c.is_control()
                && !c.is_whitespace()
                && !c.is_ascii_punctuation()
                && !c.is_ascii_digit())
    }

    /// Checks if the given character can be used as a non-initial character in
    /// an identifier.
    #[must_use]
    pub fn is_identifier_character(c: char) -> bool {
        c == '_'
            || (!c.is_control()
                && !c.is_whitespace()
                && !c.is_ascii_punctuation())
    }

    /// Checks if the given string is a valid identifier.
    #[must_use]
    pub fn is_identifier_string(s: &str) -> bool {
        let mut chars = s.chars();

        if let Some(first_char) = chars.next() {
            if !Self::is_first_identifier_character(first_char) {
                return false;
            }
        } else {
            return false;
        }

        for c in chars {
            if !Self::is_identifier_character(c) {
                return false;
            }
        }

        true
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

/// A new-type wrapper of `char` used to represent a punctuation character in
/// the source code like `+`, `-`, `*`, `/`, etc.
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
    Encode,
    Decode,
    Deref,
    DerefMut,
    StableHash,
)]
pub struct Punctuation(pub char);

impl Display for Punctuation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char(self.0)
    }
}

/// Represents a sequence of digits that can be used to represent a number in
/// the source code: `let x = 123;`.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    Deref,
    DerefMut,
    StableHash,
)]
pub struct Numeric(pub Interned<str>);

/// An enumeration of all possible kinds of tokens in the Pernix programming
/// language.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    From,
    EnumAsInner,
    StableHash,
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
