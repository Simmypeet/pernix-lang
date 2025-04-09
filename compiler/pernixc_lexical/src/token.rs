//! Is a module containing the [`Token`] type and all of its related types.

use std::{
    collections::HashMap,
    hash::Hash,
    iter::{Iterator, Peekable},
    str::{CharIndices, FromStr},
    sync::LazyLock,
};

use bimap::BiHashMap;
use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_handler::Handler;
use pernixc_source_file::{ByteIndex, SourceElement, Span};
use serde::{Deserialize, Serialize};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use thiserror::Error;

use crate::error::{self, InvalidEscapeSequence};

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
    Serialize,
    Deserialize,
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

impl std::fmt::Display for KeywordKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// A bidirectional map that maps a escape sequence (on the left) to its
/// representation (on the right).
static ESCAPE_SEQUENCE_BY_REPRESENTATION: LazyLock<BiHashMap<char, char>> =
    LazyLock::new(|| {
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
    });

/// A static map that maps a string representation of a keyword to its
/// [`KeywordKind`].
static STRING_KEYWORD_MAP: LazyLock<HashMap<&'static str, KeywordKind>> =
    LazyLock::new(|| {
        let mut map = HashMap::new();

        for keyword in KeywordKind::iter() {
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

impl FromStr for KeywordKind {
    type Err = KeywordParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
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

/// Represents a single whitespace character.
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
)]
pub struct NewLine<S> {
    /// The span to the new line character, either `\n` or `\r\n`.
    pub span: S,
}

impl<S: Clone> SourceElement for NewLine<S> {
    type Span = S;

    fn span(&self) -> S { self.span.clone() }
}

/// Represents a single ASCII character literal enclosed in single quotes.
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
)]
pub struct Character<S> {
    /// Is the span that makes up the token.
    pub span: S,

    /// The value of the character literal.
    ///
    /// The value is `None` if the character literal is invalid (e.g., invalid
    /// escape sequence).
    pub value: Option<char>,
}

impl<S: Clone> SourceElement for Character<S> {
    type Span = S;

    fn span(&self) -> S { self.span.clone() }
}

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
    Serialize,
    Deserialize,
)]
pub struct String<S> {
    /// Is the span that makes up the token. This includes the opening and
    /// closing double quotes.
    pub span: S,

    /// Checks if the string token is valid (properly escaped and closed with
    /// double quotes).
    pub is_valid: bool,
}

impl<S: Clone> SourceElement for String<S> {
    type Span = S;

    fn span(&self) -> S { self.span.clone() }
}

/// Represents a contiguous sequence of characters that are valid in an
/// identifier.
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
)]
pub struct Identifier<S> {
    /// Is the span that makes up the token.
    pub span: S,
}

/// Checks if the given string is a valid identifier string.
pub fn is_valid_identifier_string(s: &str) -> bool {
    let mut chars = s.chars();

    if let Some(character) = chars.next() {
        if !is_first_identifier_character(character) {
            return false;
        }
    } else {
        return false;
    }

    let identifier = chars.all(is_identifier_character);

    KeywordKind::from_str(s).is_err() && identifier
}

impl<S: Clone> SourceElement for Identifier<S> {
    type Span = S;

    fn span(&self) -> S { self.span.clone() }
}

/// Represents a contiguous sequence of characters that are reserved for a
/// keyword.
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
)]
pub struct Keyword<S> {
    /// Is the span that makes up the token.
    pub span: S,

    /// Is the [`KeywordKind`] that the token represents.
    pub kind: KeywordKind,
}

impl<S: Clone> SourceElement for Keyword<S> {
    type Span = S;

    fn span(&self) -> S { self.span.clone() }
}

/// Represents a single ASCII punctuation character.
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
)]
pub struct Punctuation<S> {
    /// Is the span that makes up the token.
    pub span: S,

    /// Is the ASCII punctuation character that the token represents.
    pub punctuation: char,
}

impl<S: Clone> SourceElement for Punctuation<S> {
    type Span = S;

    fn span(&self) -> S { self.span.clone() }
}

/// Represents a hardcoded numeric literal value in the source code.
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
)]
pub struct Numeric<S> {
    /// Is the span that makes up the token.
    pub span: S,
}

impl<S: Clone> SourceElement for Numeric<S> {
    type Span = S;

    fn span(&self) -> S { self.span.clone() }
}

/// Is an enumeration containing all kinds of tokens in the Pernix programming
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
    EnumAsInner,
    From,
    Serialize,
    Deserialize,
)]
#[allow(missing_docs)]
pub enum Kind<S> {
    Identifier(Identifier<S>),
    Keyword(Keyword<S>),
    Punctuation(Punctuation<S>),
    Numeric(Numeric<S>),
    Character(Character<S>),
    String(String<S>),
    NewLine(NewLine<S>),
}

impl<S> Kind<S> {
    /// Returns the span of the token.
    #[must_use]
    pub const fn span(&self) -> &S {
        match self {
            Self::Identifier(token) => &token.span,
            Self::Keyword(token) => &token.span,
            Self::Punctuation(token) => &token.span,
            Self::Numeric(token) => &token.span,
            Self::Character(token) => &token.span,
            Self::String(token) => &token.span,
            Self::NewLine(token) => &token.span,
        }
    }

    /// Returns `true` if the token is significant.
    ///
    /// A token is significant if it is not a whitespace or a comment.
    #[must_use]
    pub const fn is_significant(&self) -> bool {
        match self {
            Self::NewLine(_) => false,

            Self::Identifier(_)
            | Self::Keyword(_)
            | Self::Punctuation(_)
            | Self::Numeric(_)
            | Self::Character(_)
            | Self::String(_) => true,
        }
    }
}

impl<S: Clone> SourceElement for Kind<S> {
    type Span = S;

    fn span(&self) -> S {
        match self {
            Self::NewLine(token) => token.span(),
            Self::Identifier(token) => token.span(),
            Self::Keyword(token) => token.span(),
            Self::Punctuation(token) => token.span(),
            Self::Numeric(token) => token.span(),
            Self::Character(token) => token.span(),
            Self::String(token) => token.span(),
        }
    }
}

/// A wrapper over a token and prior insignificant span pair.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    derive_more::Deref,
    derive_more::DerefMut,
    derive_new::new,
)]
pub struct WithInsignificant<T, S> {
    /// The token that is wrapped.
    #[deref]
    #[deref_mut]
    pub token: T,

    /// The span of the insignificant token that precedes this token.
    ///
    /// Insignificant tokens are tokens that are most likely to be ignored by
    /// the parser. For example, whitespace and comments are insignificant
    /// tokens.
    pub prior_insignificant: Option<S>,
}

/// Represents a token in the Pernix programming language.
pub type Token<S> = WithInsignificant<Kind<S>, S>;

fn is_whitespace(character: char) -> bool {
    character.is_whitespace() && character != '\n' && character != '\r'
}

/// Increments the iterator until the predicate returns false.
fn walk_iter(
    iter: &mut Peekable<impl Iterator<Item = (ByteIndex, char)>>,
    predicate: impl Fn(char) -> bool,
) {
    while let Some((_, character)) = iter.peek() {
        if !predicate(*character) {
            break;
        }

        iter.next();
    }
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

/// A struct used for tokenizing the source code. The struct implements
/// [`Iterator`] trait, which allows it to be used as an iterator that iterates
/// over token sequences in the source code.
pub struct Tokenizer<'a, 'h, ID> {
    source: &'a str,
    iter: Peekable<CharIndices<'a>>,
    handler: &'h dyn Handler<error::Error<ID>>,
    source_id: ID,
}

impl<ID: std::fmt::Debug> std::fmt::Debug for Tokenizer<'_, '_, ID> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Tokenizer")
            .field("source", &self.source)
            .field("iter", &self.iter)
            .field("source_id", &self.source_id)
            .finish_non_exhaustive()
    }
}

impl<'a, 'h, ID> Tokenizer<'a, 'h, ID> {
    /// Creates a new [`Tokenizer`] instance.
    ///
    /// # Parameters
    ///
    /// - `source`: The source code to tokenize.
    /// - `source_id`: The ID of the source code used for creating spans.
    /// - `handler`: The handler to use for reporting errors.
    pub fn new(
        source: &'a str,
        source_id: ID,
        handler: &'h dyn Handler<error::Error<ID>>,
    ) -> Self {
        Self {
            source,
            iter: source.char_indices().peekable(),
            handler,
            source_id,
        }
    }
}

impl<ID: Clone> Tokenizer<'_, '_, ID> {
    /// Creates a span from the given start location to the current location of
    /// the iterator.
    fn create_span(&mut self, start: ByteIndex) -> Span<ID>
    where
        ID: Clone,
    {
        if let Some((index, _)) = self.iter.peek().copied() {
            Span { start, end: index, source_id: self.source_id.clone() }
        } else {
            Span {
                start,
                end: self.source.len(),
                source_id: self.source_id.clone(),
            }
        }
    }

    fn handle_insignificant(&mut self) -> Option<Span<ID>> {
        let (start, _) = self.iter.peek().copied()?;

        while let Some((_, character)) = self.iter.peek().copied() {
            if is_whitespace(character) {
                walk_iter(&mut self.iter, is_whitespace);
            } else if character == '#' {
                walk_iter(&mut self.iter, |x| x != '\n' && x != '\r');
            } else {
                break;
            }
        }

        if let Some((index, _)) = self.iter.peek().copied() {
            if index == start {
                return None;
            }

            Some(Span::new(start, index, self.source_id.clone()))
        } else {
            Some(Span::new(start, self.source.len(), self.source_id.clone()))
        }
    }

    fn handle_new_line(
        &mut self,
        character: char,
        start: ByteIndex,
    ) -> NewLine<Span<ID>> {
        // cr
        if character == '\r' {
            //  lf
            if let Some((_, '\n')) = self.iter.peek() {
                // crlf
                self.iter.next();
            }
        }

        NewLine { span: self.create_span(start) }
    }

    fn handle_identifier_and_keyword(
        &mut self,
        start: ByteIndex,
    ) -> Kind<Span<ID>> {
        walk_iter(&mut self.iter, is_identifier_character);

        let span = self.create_span(start);
        let word = &self.source[start..span.end];

        // Checks if the word is a keyword
        if let Ok(kind) = KeywordKind::from_str(word) {
            Kind::Keyword(Keyword { span, kind })
        } else {
            Kind::Identifier(Identifier { span })
        }
    }

    fn handle_numeric_literal(
        &mut self,
        start: ByteIndex,
    ) -> Numeric<Span<ID>> {
        // Tokenizes the whole number part
        walk_iter(&mut self.iter, |character| character.is_ascii_digit());

        Numeric { span: self.create_span(start) }
    }

    fn handle_single_quote(&mut self, start: ByteIndex) -> Kind<Span<ID>> {
        let mut iter_cloned = self.iter.clone();

        match iter_cloned.next() {
            // escaped character
            Some((content_start, mut char)) => {
                let is_escaped = if char == '\\' {
                    // eat the next character
                    let Some((_, new_char)) = iter_cloned.next() else {
                        return Kind::Punctuation(Punctuation {
                            span: self.create_span(start),
                            punctuation: '\'',
                        });
                    };

                    char = new_char;

                    true
                } else {
                    false
                };

                if !is_escaped && char == '\'' {
                    return Kind::Punctuation(Punctuation {
                        span: self.create_span(start),
                        punctuation: '\'',
                    });
                }

                match iter_cloned.next() {
                    // a caharceter literal
                    Some((content_end, '\'')) => {
                        if is_escaped {
                            self.iter.next(); // eat the backslash
                        }

                        self.iter.next(); // eat the character
                        self.iter.next(); // eat the closing quote

                        Kind::Character(Character {
                            span: self.create_span(start),
                            value: if !is_escaped {
                                Some(char)
                            } else if let Some(value) =
                                ESCAPE_SEQUENCE_BY_REPRESENTATION
                                    .get_by_left(&char)
                                    .copied()
                            {
                                Some(value)
                            } else {
                                self.handler.receive(
                                    error::Error::InvalidEscapeSequence(
                                        InvalidEscapeSequence {
                                            span: Span::new(
                                                content_start,
                                                content_end,
                                                self.source_id.clone(),
                                            ),
                                        },
                                    ),
                                );
                                None
                            },
                        })
                    }

                    _ => Kind::Punctuation(Punctuation {
                        span: self.create_span(start),
                        punctuation: '\'',
                    }),
                }
            }

            None => Kind::Punctuation(Punctuation {
                span: self.create_span(start),
                punctuation: '\'',
            }),
        }
    }

    fn handle_string_literal(&mut self, start: ByteIndex) -> String<Span<ID>> {
        let mut last_backslash = false;
        let mut last_byte_index = start;

        let mut has_error = false;

        loop {
            let Some((byte_index, character)) = self.iter.next() else {
                self.handler.receive(error::Error::UnterminatedStringLiteral(
                    error::UnterminatedStringLiteral {
                        span: Span::new(
                            start,
                            start + 1,
                            self.source_id.clone(),
                        ),
                    },
                ));

                return String {
                    span: Span::new(
                        start,
                        self.source.len(),
                        self.source_id.clone(),
                    ),
                    is_valid: false,
                };
            };

            if last_backslash {
                if ESCAPE_SEQUENCE_BY_REPRESENTATION
                    .get_by_left(&character)
                    .is_none()
                {
                    self.handler.receive(error::Error::InvalidEscapeSequence(
                        InvalidEscapeSequence {
                            span: Span::new(
                                last_byte_index,
                                byte_index,
                                self.source_id.clone(),
                            ),
                        },
                    ));
                    has_error = true;
                }

                last_backslash = false;
            } else {
                match character {
                    // end the string
                    '"' => {
                        return String {
                            span: self.create_span(start),
                            is_valid: !has_error,
                        };
                    }

                    // escape sequence
                    '\\' => {
                        last_backslash = true;
                    }

                    // normal character
                    _ => {
                        last_backslash = false;
                    }
                }
            }

            last_byte_index = byte_index;
        }
    }
}

impl<ID: Clone> Iterator for Tokenizer<'_, '_, ID> {
    type Item = Token<Span<ID>>;

    fn next(&mut self) -> Option<Self::Item> {
        // Found white spaces
        let prior_insignificant = self.handle_insignificant();

        // Gets the first character
        let (start, character) = self.iter.next()?;

        // Found new line character
        let kind = if character == '\n' || character == '\r' {
            Kind::NewLine(self.handle_new_line(character, start))
        }
        // Found identifier/keyword
        else if is_first_identifier_character(character) {
            self.handle_identifier_and_keyword(start)
        }
        // Found numeric literal
        else if character.is_ascii_digit() {
            Kind::Numeric(self.handle_numeric_literal(start))
        }
        // Might found a character literal
        else if character == '\'' {
            self.handle_single_quote(start)
        }
        // Found a string literal
        else if character == '"' {
            Kind::String(self.handle_string_literal(start))
        }
        // Found a punctuation
        else if character.is_ascii_punctuation() {
            Kind::Punctuation(Punctuation {
                span: self.create_span(start),
                punctuation: character,
            })
        } else {
            unreachable!("should've been handled by earlier cases")
        };

        Some(Token { token: kind, prior_insignificant })
    }
}

#[cfg(test)]
pub(crate) mod test;
