//! Is a module containing the [`Token`] type and all of its related types.

use std::{collections::HashMap, hash::Hash, iter::Iterator, str::FromStr};

use bimap::BiHashMap;
use derive_more::From;
use enum_as_inner::EnumAsInner;
use lazy_static::lazy_static;
use pernixc_base::{
    diagnostic::Handler,
    source_file::{self, ByteIndex, SourceElement, Span},
};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use thiserror::Error;

use crate::error::{self, InvalidEscapeSequence, UnterminatedDelimitedComment};

pub mod strategy;

/// Is an enumeration representing keywords in the Pernix programming language.
///
/// Most enum variants names are the same as the keyword they represent, except
/// that the name is capitalized while the keyword is not. For example, the
/// `function` keyword is represented by the `Function` variant.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumIter,
)]
pub enum KeywordKind {
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
    /// `mutable` keyword.
    Mutable,
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
    /// `unique` keyword.
    Unique,
    /// `where` keyword.
    Where,
    /// `trait` keyword.
    Trait,
    /// `using` keyword.
    Using,
    /// `function` keyword.
    Function,
    /// `unsafe` keyword.
    Unsafe,
    /// `for` keyword
    For,
    /// `delete` keyword
    Delete,
    /// `local` keyword
    Local,
    /// `unlocal` keyword
    Unlocal,
    /// `tuple` keyword
    Tuple,
    /// `case` keyword
    Case,
    /// `phantom` keyword
    Phantom,
    /// `ref` keyword
    Ref,
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
}

impl std::fmt::Display for KeywordKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// Is an error that is returned when a string cannot be parsed into a
/// [`Keyword`] in [`FromStr`] trait implementation.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Error,
)]
#[error("invalid string representation of keyword.")]
pub struct KeywordParseError;

impl FromStr for KeywordKind {
    type Err = KeywordParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref STRING_KEYWORD_MAP: HashMap<&'static str, KeywordKind> = {
                let mut map = HashMap::new();

                for keyword in KeywordKind::iter() {
                    map.insert(keyword.as_str(), keyword);
                }

                map
            };
        }
        STRING_KEYWORD_MAP.get(s).copied().ok_or(KeywordParseError)
    }
}

impl KeywordKind {
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
            Self::Mutable => "mutable",
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
            Self::Unique => "unique",
            Self::Where => "where",
            Self::Trait => "trait",
            Self::Using => "using",
            Self::Unsafe => "unsafe",
            Self::Local => "local",
            Self::Match => "match",
            Self::Unlocal => "unlocal",
            Self::Tuple => "tuple",
            Self::Case => "case",
            Self::Phantom => "phantom",
            Self::Ref => "ref",
            Self::Final => "final",
            Self::Extern => "extern",
            Self::Effect => "effect",
            Self::Do => "do",
            Self::Try => "try",
            Self::With => "with",
        }
    }
}

lazy_static! {
    /// A bidirectional map that maps a escape sequence (on the left) to its
    /// representation (on the right).
    pub static ref ESCAPE_SEQUENCE_BY_REPRESENTATION: BiHashMap<char, char> = {
        let mut map = BiHashMap::new();

        map.insert('\'', '\'');
        map.insert('"', '"');
        map.insert('\\', '\\');
        map.insert('a', '\x07');
        map.insert('b', '\x08');
        map.insert('t', '\x09');
        map.insert('n', '\x0A');
        map.insert('v', '\x0B');
        map.insert('f', '\x0C');
        map.insert('r', '\x0D');
        map.insert('0', '\0');

        map
    };
}

/// Is an enumeration containing all kinds of tokens in the Pernix programming
/// language.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From,
)]
#[allow(missing_docs)]
pub enum Token {
    WhiteSpaces(WhiteSpaces),
    Identifier(Identifier),
    Keyword(Keyword),
    Punctuation(Punctuation),
    Numeric(Numeric),
    Comment(Comment),
    Character(Character),
    String(String),
}

impl Token {
    /// Returns the span of the token.
    #[must_use]
    pub const fn span(&self) -> &Span {
        match self {
            Self::WhiteSpaces(token) => &token.span,
            Self::Identifier(token) => &token.span,
            Self::Keyword(token) => &token.span,
            Self::Punctuation(token) => &token.span,
            Self::Numeric(token) => &token.span,
            Self::Comment(token) => &token.span,
            Self::Character(token) => &token.span,
            Self::String(token) => &token.span,
        }
    }
}

impl SourceElement for Token {
    fn span(&self) -> Span {
        match self {
            Self::WhiteSpaces(token) => token.span(),
            Self::Identifier(token) => token.span(),
            Self::Keyword(token) => token.span(),
            Self::Punctuation(token) => token.span(),
            Self::Numeric(token) => token.span(),
            Self::Comment(token) => token.span(),
            Self::Character(token) => token.span(),
            Self::String(token) => token.span(),
        }
    }
}

/// Represents a single ASCII character literal enclosed in single quotes.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Character {
    /// Is the span that makes up the token.
    pub span: Span,

    /// The value of the character literal.
    ///
    /// The value is `None` if the character literal is invalid (e.g., invalid
    /// escape sequence).
    pub value: Option<char>,
}

impl SourceElement for Character {
    fn span(&self) -> Span { self.span.clone() }
}

/// Represents a hardcoded string literal value in the source code.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct String {
    /// Is the span that makes up the token.
    pub span: Span,

    /// The value of the string literal. The value is `None` if the string
    /// literal is invalid (e.g., invalid escape sequence).
    pub value: Option<std::string::String>,
}

impl SourceElement for String {
    fn span(&self) -> Span { self.span.clone() }
}

/// Represents a contiguous sequence of whitespace characters.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WhiteSpaces {
    /// Is the span that makes up the token.
    pub span: Span,
}

impl SourceElement for WhiteSpaces {
    fn span(&self) -> Span { self.span.clone() }
}

/// Represents a contiguous sequence of characters that are valid in an
/// identifier.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier {
    /// Is the span that makes up the token.
    pub span: Span,
}

impl SourceElement for Identifier {
    fn span(&self) -> Span { self.span.clone() }
}

/// Represents a contiguous sequence of characters that are reserved for a
/// keyword.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Keyword {
    /// Is the span that makes up the token.
    pub span: Span,

    /// Is the [`KeywordKind`] that the token represents.
    pub kind: KeywordKind,
}

impl SourceElement for Keyword {
    fn span(&self) -> Span { self.span.clone() }
}

/// Represents a single ASCII punctuation character.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Punctuation {
    /// Is the span that makes up the token.
    pub span: Span,

    /// Is the ASCII punctuation character that the token represents.
    pub punctuation: char,
}

impl SourceElement for Punctuation {
    fn span(&self) -> Span { self.span.clone() }
}

/// Represents a hardcoded numeric literal value in the source code.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Numeric {
    /// Is the span that makes up the token.
    pub span: Span,
}

impl SourceElement for Numeric {
    fn span(&self) -> Span { self.span.clone() }
}

/// Is an enumeration representing the two kinds of comments in the Pernix
/// programming language.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CommentKind {
    /// A comment that starts with `//` and ends at the end of the line.
    Line,

    /// A comment that starts with `/*` and ends with `*/`.
    Delimited,
}

/// Represents a portion of the source code that is ignored by the compiler.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Comment {
    /// Is the span that makes up the token.
    pub span: Span,

    /// Is the kind of comment that the token represents.
    pub kind: CommentKind,
}

impl SourceElement for Comment {
    fn span(&self) -> Span { self.span.clone() }
}

/// Is an error that can occur when invoking the [`Token::lex`] method.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    thiserror::Error,
    From,
)]
#[allow(missing_docs)]
pub enum Error {
    #[error(
        "encountered a fatal lexical error that causes the process to stop."
    )]
    FatalLexicalError,

    #[error("the iterator argument is at the end of the source code.")]
    EndOfSourceCodeIteratorArgument,
}

impl Token {
    /// Increments the iterator until the predicate returns false.
    fn walk_iter(
        iter: &mut source_file::Iterator,
        predicate: impl Fn(char) -> bool,
    ) {
        while let Some((_, character)) = iter.peek() {
            if !predicate(character) {
                break;
            }

            iter.next();
        }
    }

    /// Creates a span from the given start location to the current location of
    /// the iterator.
    fn create_span(start: ByteIndex, iter: &mut source_file::Iterator) -> Span {
        iter.peek().map_or_else(
            || Span::to_end(iter.source_file().clone(), start).unwrap(),
            |(index, _)| {
                Span::new(iter.source_file().clone(), start, index).unwrap()
            },
        )
    }

    /// Checks if the given character is a valid first character of an
    /// identifier.
    fn is_first_identifier_character(character: char) -> bool {
        character == '_'
            || (!character.is_control()
                && !character.is_whitespace()
                && !character.is_ascii_punctuation()
                && !character.is_ascii_digit())
    }

    /// Checks if the given character is a valid character of an identifier.
    fn is_identifier_character(character: char) -> bool {
        character == '_'
            || (!character.is_control()
                && !character.is_whitespace()
                && !character.is_ascii_punctuation())
    }

    fn handle_whitespace(
        iter: &mut source_file::Iterator,
        start: ByteIndex,
    ) -> Self {
        Self::walk_iter(iter, char::is_whitespace);

        WhiteSpaces { span: Self::create_span(start, iter) }.into()
    }

    fn handle_identifier_and_keyword(
        iter: &mut source_file::Iterator,
        start: ByteIndex,
    ) -> Self {
        Self::walk_iter(iter, Self::is_identifier_character);

        let span = Self::create_span(start, iter);
        let word = span.str();

        // Checks if the word is a keyword
        KeywordKind::from_str(word).ok().map_or_else(
            || Identifier { span: span.clone() }.into(),
            |kw| Keyword { span: span.clone(), kind: kw }.into(),
        )
    }

    fn handle_comment(
        iter: &mut source_file::Iterator,
        start: ByteIndex,
        character: char,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self, Error> {
        // Single line comment
        if let Some((_, '/')) = iter.peek() {
            iter.next();

            Self::walk_iter(iter, |character| {
                !(character == '\n' || character == '\r')
            });

            let is_cr =
                iter.peek().map_or(false, |(_, character)| character == '\r');

            if let (true, Some((_, '\n'))) = (is_cr, iter.next()) {
                // skips the crlf
                iter.next();
            }

            Ok(Comment {
                span: Self::create_span(start, iter),
                kind: CommentKind::Line,
            }
            .into())
        }
        // Delimited comment
        else if let Some((_, '*')) = iter.peek() {
            iter.next();

            let mut is_terminated = false;

            while let Some((_, character)) = iter.next() {
                if character == '*' {
                    if let Some((_, '/')) = iter.peek() {
                        iter.next();

                        is_terminated = true;

                        break;
                    }
                }
            }

            // Checks if the comment is terminated
            if is_terminated {
                Ok(Comment {
                    span: Self::create_span(start, iter),
                    kind: CommentKind::Delimited,
                }
                .into())
            } else {
                handler.receive(
                    UnterminatedDelimitedComment {
                        span: Span::new(
                            iter.source_file().clone(),
                            start,
                            start + 2,
                        )
                        .unwrap(),
                    }
                    .into(),
                );
                return Err(Error::FatalLexicalError);
            }
        }
        // Just a single slash punctuation
        else {
            Ok(Punctuation {
                span: Self::create_span(start, iter),
                punctuation: character,
            }
            .into())
        }
    }

    fn handle_numeric_literal(
        iter: &mut source_file::Iterator,
        start: ByteIndex,
    ) -> Self {
        // Tokenizes the whole number part
        Self::walk_iter(iter, |character| character.is_ascii_digit());

        Numeric { span: Self::create_span(start, iter) }.into()
    }

    fn handle_string_literal(
        iter: &mut source_file::Iterator,
        start: ByteIndex,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self, Error> {
        let mut string = Some(std::string::String::new());
        let mut last_backslash = false;
        let mut last_byte_index = start;

        loop {
            let Some((byte_index, character)) = iter.next() else {
                handler.receive(error::Error::UnterminatedStringLiteral(
                    error::UnterminatedStringLiteral {
                        span: Span::new(
                            iter.source_file().clone(),
                            start,
                            start + 1,
                        )
                        .unwrap(),
                    },
                ));

                return Err(Error::FatalLexicalError);
            };

            if last_backslash {
                if let Some(value) =
                    ESCAPE_SEQUENCE_BY_REPRESENTATION.get_by_left(&character)
                {
                    if let Some(string) = string.as_mut() {
                        string.push(*value);
                    }
                } else {
                    handler.receive(error::Error::InvalidEscapeSequence(
                        InvalidEscapeSequence {
                            span: Span::new(
                                iter.source_file().clone(),
                                last_byte_index,
                                byte_index,
                            )
                            .unwrap(),
                        },
                    ));
                }

                last_backslash = false;
            } else {
                match character {
                    // end the string
                    '"' => {
                        return Ok(Self::String(String {
                            span: Self::create_span(start, iter),
                            value: string,
                        }));
                    }

                    // escape sequence
                    '\\' => {
                        last_backslash = true;
                    }

                    // normal character
                    character => {
                        if let Some(x) = string.as_mut() {
                            x.push(character);
                        }
                        last_backslash = false;
                    }
                }
            }

            last_byte_index = byte_index;
        }
    }

    fn handle_single_quote(
        iter: &mut source_file::Iterator,
        start: ByteIndex,
        handler: &dyn Handler<error::Error>,
    ) -> Self {
        let mut iter_cloned = iter.clone();

        match iter_cloned.next() {
            // escaped character
            Some((content_start, mut char)) => {
                let is_escaped = if char == '\\' {
                    // eat the next character
                    let Some((_, new_char)) = iter_cloned.next() else {
                        return Self::Punctuation(Punctuation {
                            span: Self::create_span(start, iter),
                            punctuation: '\'',
                        });
                    };

                    char = new_char;

                    true
                } else {
                    false
                };

                if !is_escaped && char == '\'' {
                    return Self::Punctuation(Punctuation {
                        span: Self::create_span(start, iter),
                        punctuation: '\'',
                    });
                }

                match iter_cloned.next() {
                    // a caharceter literal
                    Some((content_end, '\'')) => {
                        if is_escaped {
                            iter.next(); // eat the backslash
                        }

                        iter.next(); // eat the character
                        iter.next(); // eat the closing quote

                        Self::Character(Character {
                            span: Self::create_span(start, iter),
                            value: if !is_escaped {
                                Some(char)
                            } else if let Some(value) =
                                ESCAPE_SEQUENCE_BY_REPRESENTATION
                                    .get_by_left(&char)
                                    .copied()
                            {
                                Some(value)
                            } else {
                                handler.receive(
                                    error::Error::InvalidEscapeSequence(
                                        InvalidEscapeSequence {
                                            span: Span::new(
                                                iter.source_file().clone(),
                                                content_start,
                                                content_end,
                                            )
                                            .unwrap(),
                                        },
                                    ),
                                );
                                None
                            },
                        })
                    }

                    _ => Self::Punctuation(Punctuation {
                        span: Self::create_span(start, iter),
                        punctuation: '\'',
                    }),
                }
            }

            None => Self::Punctuation(Punctuation {
                span: Self::create_span(start, iter),
                punctuation: '\'',
            }),
        }
    }

    /// Lexes the source code from the given iterator.
    ///
    /// The tokenization starts at the current location of the iterator. The
    /// function moves the iterator at least once and forwards it until it
    /// makes a token. After the token is made, the iterator is left at the
    /// next character that is not part of the token.
    ///
    /// # Errors
    /// - [`Error::EndOfSourceCodeIteratorArgument`] - The iterator argument is
    ///   at the end of the source code.
    /// - [`Error::FatalLexicalError`] - A fatal lexical error occurred.
    pub fn lex(
        iter: &mut source_file::Iterator,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self, Error> {
        // Gets the first character
        let (start, character) =
            iter.next().ok_or(Error::EndOfSourceCodeIteratorArgument)?;

        // Found white spaces
        if character.is_whitespace() {
            Ok(Self::handle_whitespace(iter, start))
        }
        // Found identifier/keyword
        else if Self::is_first_identifier_character(character) {
            Ok(Self::handle_identifier_and_keyword(iter, start))
        }
        // Found comment/single slash punctuation
        else if character == '/' {
            Self::handle_comment(iter, start, character, handler)
        }
        // Found numeric literal
        else if character.is_ascii_digit() {
            Ok(Self::handle_numeric_literal(iter, start))
        }
        // Might found a character literal
        else if character == '\'' {
            Ok(Self::handle_single_quote(iter, start, handler))
        }
        // Found a string literal
        else if character == '"' {
            Self::handle_string_literal(iter, start, handler)
        }
        // Found a punctuation
        else if character.is_ascii_punctuation() {
            Ok(Punctuation {
                span: Self::create_span(start, iter),
                punctuation: character,
            }
            .into())
        } else {
            unreachable!("should've been handled by earlier cases")
        }
    }
}

#[cfg(test)]
pub(crate) mod test;
