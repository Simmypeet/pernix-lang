//! Is a module containing the [`Token`] type and all of its related types.

use std::{collections::HashMap, hash::Hash, iter::Iterator, str::FromStr};

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use lazy_static::lazy_static;
use pernixc_source::{ByteIndex, SourceElement, SourceFile, Span, SpanError};
use pernixc_system::diagnostic::Handler;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use thiserror::Error;

use crate::error::{
    ControlCharactersMustBeEscaped, EmptyCharacterLiteral, Error, InvalidEscapeCharacterSequences,
    UnterminatedDelimitedComment, UnterminatedStringLiteral,
};

/// Is an enumeration representing keywords in the Pernix programming language.
///
/// Most enum variants names are the same as the keyword they represent, except that the name is
/// capitalized while the keyword is not. For example, the `function` keyword is represented by the
/// `Function` variant.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumIter)]
pub enum KeywordKind {
    /// `public` keyword.
    Public,
    /// `struct` keyword.
    Struct,
    /// `interface` keyword.
    Interface,
    /// `implement` keyword.
    Implement,
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
    /// `void` keyword.
    Void,
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
    /// `this` keyword.
    This,
    /// `self` keyword.
    SelfKeyword,
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
    /// `restrict` keyword.
    Restrict,
    /// `where` keyword.
    Where,
    /// `trait` keyword.
    Trait,
}

impl ToString for KeywordKind {
    fn to_string(&self) -> String { self.as_str().to_string() }
}

/// Is an error that is returned when a string cannot be parsed into a [`Keyword`] in [`FromStr`]
/// trait implementation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Error)]
#[error("Invalid string representation of keyword.")]
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
    pub fn as_str(self) -> &'static str {
        match self {
            Self::As => "as",
            Self::Enum => "enum",
            Self::Struct => "struct",
            Self::Express => "express",
            Self::Loop => "loop",
            Self::Public => "public",
            Self::Interface => "interface",
            Self::Implement => "implement",
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
            Self::Void => "void",
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
            Self::This => "this",
            Self::SelfKeyword => "self",
            Self::And => "and",
            Self::Or => "or",
            Self::Private => "private",
            Self::Internal => "internal",
            Self::Module => "module",
            Self::Type => "type",
            Self::Static => "static",
            Self::Restrict => "restrict",
            Self::Where => "where",
            Self::Trait => "trait",
        }
    }

    /// Parses a string into its corresponding keyword.
    #[must_use]
    pub fn from_string(str: &str) -> Option<Self> {
        lazy_static! {
            static ref STRING_KEYWORD_MAP: HashMap<&'static str, KeywordKind> = {
                let mut map = HashMap::new();

                for keyword in KeywordKind::iter() {
                    map.insert(keyword.as_str(), keyword);
                }

                map
            };
        }

        STRING_KEYWORD_MAP.get(str).copied()
    }
}

/// Is an enumeration containing all kinds of tokens in the Pernix programming language.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Token {
    WhiteSpace(WhiteSpace),
    Identifier(Identifier),
    Keyword(Keyword),
    Punctuation(Punctuation),
    NumericLiteral(NumericLiteral),
    StringLiteral(StringLiteral),
    CharacterLiteral(CharacterLiteral),
    Comment(Comment),
}

impl Token {
    /// Returns the span of the token.
    #[must_use]
    pub fn span(&self) -> &Span {
        match self {
            Self::WhiteSpace(token) => &token.span,
            Self::Identifier(token) => &token.span,
            Self::Keyword(token) => &token.span,
            Self::Punctuation(token) => &token.span,
            Self::NumericLiteral(token) => &token.span,
            Self::StringLiteral(token) => &token.span,
            Self::CharacterLiteral(token) => &token.span,
            Self::Comment(token) => &token.span,
        }
    }
}

impl SourceElement for Token {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::WhiteSpace(token) => token.span(),
            Self::Identifier(token) => token.span(),
            Self::Keyword(token) => token.span(),
            Self::Punctuation(token) => token.span(),
            Self::NumericLiteral(token) => token.span(),
            Self::StringLiteral(token) => token.span(),
            Self::CharacterLiteral(token) => token.span(),
            Self::Comment(token) => token.span(),
        }
    }
}

/// Represents a contiguous sequence of whitespace characters.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct WhiteSpace {
    /// Is the span that makes up the token.
    pub span: Span,
}

impl SourceElement for WhiteSpace {
    fn span(&self) -> Result<Span, SpanError> { Ok(self.span.clone()) }
}

/// Represents a contiguous sequence of characters that are valid in an identifier.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct Identifier {
    /// Is the span that makes up the token.
    pub span: Span,
}

impl SourceElement for Identifier {
    fn span(&self) -> Result<Span, SpanError> { Ok(self.span.clone()) }
}

/// Represents a contiguous sequence of characters that are reserved for a keyword.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct Keyword {
    /// Is the span that makes up the token.
    pub span: Span,

    /// Is the [`KeywordKind`] that the token represents.
    pub keyword: KeywordKind,
}

impl SourceElement for Keyword {
    fn span(&self) -> Result<Span, SpanError> { Ok(self.span.clone()) }
}

/// Represents a single ASCII punctuation character.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct Punctuation {
    /// Is the span that makes up the token.
    pub span: Span,

    /// Is the ASCII punctuation character that the token represents.
    pub punctuation: char,
}

impl SourceElement for Punctuation {
    fn span(&self) -> Result<Span, SpanError> { Ok(self.span.clone()) }
}

/// Represents a hardcoded numeric literal value in the source code.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct NumericLiteral {
    /// Is the span that makes up the token.
    pub span: Span,

    /// Is the sapn representing the value part of the numeric literal.
    pub value_span: Span,

    /// Is the span representing the suffix part of the numeric literal if it exists.
    pub suffix_span: Option<Span>,
}

impl SourceElement for NumericLiteral {
    fn span(&self) -> Result<Span, SpanError> { Ok(self.span.clone()) }
}

/// Represents a single character or escape sequence enclosed in single quotes.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct CharacterLiteral {
    ///  The span that makes up the token.
    pub span: Span,

    /// The character value of the literal.
    pub character: char,
}

impl SourceElement for CharacterLiteral {
    fn span(&self) -> Result<Span, SpanError> { Ok(self.span.clone()) }
}

/// Represents a contiguous sequence of characters enclosed in double quotes.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct StringLiteral {
    /// Is the span that makes up the token.
    pub span: Span,
}

impl SourceElement for StringLiteral {
    fn span(&self) -> Result<Span, SpanError> { Ok(self.span.clone()) }
}

/// Is an enumeration representing the two kinds of comments in the Pernix programming language.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CommentKind {
    /// A comment that starts with `//` and ends at the end of the line.
    SingleLine,

    /// A comment that starts with `/*` and ends with `*/`.
    Delimited,
}

/// Represents a portion of the source code that is ignored by the compiler.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct Comment {
    /// Is the span that makes up the token.
    pub span: Span,

    /// Is the kind of comment that the token represents.
    pub kind: CommentKind,
}

impl SourceElement for Comment {
    fn span(&self) -> Result<Span, SpanError> { Ok(self.span.clone()) }
}

/// Is an error that can occur when invoking the [Token::tokenize()](Token::tokenize()) method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EnumAsInner, Error, From)]
#[allow(missing_docs)]
pub enum TokenizationError {
    #[error("Encountered a fatal lexical error that causes the process to stop.")]
    FatalLexicalError,

    #[error("The iterator argument is at the end of the source code.")]
    EndOfSourceCodeIteratorArgument,
}

impl Token {
    /// Increments the iterator until the predicate returns false.
    fn walk_iter(iter: &mut pernixc_source::Iterator, predicate: impl Fn(char) -> bool) {
        while let Some((_, character)) = iter.peek() {
            if !predicate(character) {
                break;
            }

            iter.next();
        }
    }

    /// Creates a span from the given start location to the current location of the iterator.
    fn create_span(start: ByteIndex, iter: &mut pernixc_source::Iterator) -> Span {
        iter.peek().map_or_else(
            || Span::to_end(iter.source_file().clone(), start).unwrap(),
            |(index, _)| Span::new(iter.source_file().clone(), start, index).unwrap(),
        )
    }

    /// Checks if the given character is a valid first character of an identifier.
    fn is_first_identifier_character(character: char) -> bool {
        character == '_'
            || character == '@'
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

    /// Maps the character following the `\` character in a string literal to the corresponding
    /// escape character.
    fn map_escape_character(character: char) -> Option<char> {
        match character {
            '\'' => Some(0x27 as char),
            '\"' => Some(0x22 as char),
            '?' => Some(0x3f as char),
            '\\' => Some(0x5c as char),
            'a' => Some(0x07 as char),
            'b' => Some(0x08 as char),
            'f' => Some(0x0c as char),
            'n' => Some(0x0a as char),
            'r' => Some(0x0d as char),
            't' => Some(0x09 as char),
            'v' => Some(0x0b as char),
            '0' => Some('\0'),
            _ => None,
        }
    }

    fn handle_whitespace(iter: &mut pernixc_source::Iterator, start: ByteIndex) -> Self {
        Self::walk_iter(iter, char::is_whitespace);

        WhiteSpace {
            span: Self::create_span(start, iter),
        }
        .into()
    }

    fn handle_identifier_and_keyword(
        iter: &mut pernixc_source::Iterator,
        start: ByteIndex,
    ) -> Self {
        Self::walk_iter(iter, Self::is_identifier_character);

        let span = Self::create_span(start, iter);
        let word = span.str();

        // Checks if the word is a keyword
        KeywordKind::from_string(word).map_or_else(
            || Identifier { span: span.clone() }.into(),
            |kw| {
                Keyword {
                    span: span.clone(),
                    keyword: kw,
                }
                .into()
            },
        )
    }

    fn handle_comment(
        iter: &mut pernixc_source::Iterator,
        start: ByteIndex,
        character: char,
        handler: &impl Handler<Error>,
    ) -> Result<Self, TokenizationError> {
        // Single line comment
        if let Some((_, '/')) = iter.peek() {
            iter.next();

            Self::walk_iter(iter, |character| character != SourceFile::NEW_LINE);

            iter.next();

            Ok(Comment {
                span: Self::create_span(start, iter),
                kind: CommentKind::SingleLine,
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
                handler.recieve(
                    UnterminatedDelimitedComment {
                        span: Span::new(iter.source_file().clone(), start, start + 2).unwrap(),
                    }
                    .into(),
                );
                return Err(TokenizationError::FatalLexicalError);
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

    fn handle_string_literal(
        iter: &mut pernixc_source::Iterator,
        start: ByteIndex,
        handler: &impl Handler<Error>,
    ) -> Result<Self, TokenizationError> {
        let mut found_invalid = false;
        loop {
            let character = iter.next().map_or('\0', |x| x.1);

            // Stops when the string literal is terminated
            if character == '"' {
                break;
            }
            // Handles escape characters
            else if character == '\\' {
                let escape_character = iter.next();
                let Some((location, character)) = escape_character else {
                        continue;
                    };

                let escape_character = Self::map_escape_character(character);

                if escape_character.is_none() {
                    handler.recieve(
                        InvalidEscapeCharacterSequences {
                            span: Self::create_span(location - 1, iter),
                        }
                        .into(),
                    );
                    found_invalid = true;
                }
            }
            // Unterminated string literal
            else if character == '\0' {
                handler.recieve(
                    UnterminatedStringLiteral {
                        span: Span::new(iter.source_file().clone(), start, start + 1).unwrap(),
                    }
                    .into(),
                );
                return Err(TokenizationError::FatalLexicalError);
            }
        }

        // No error found, returns the string literal token
        if found_invalid {
            // couldn't construct the string literal with the invalid escape characters since it
            // might be used later on
            Err(TokenizationError::FatalLexicalError)
        }
        // Found invalid escape characters
        else {
            Ok(StringLiteral {
                span: Self::create_span(start, iter),
            }
            .into())
        }
    }

    fn handle_character_literal(
        iter: &mut pernixc_source::Iterator,
        start: ByteIndex,
        handler: &impl Handler<Error>,
    ) -> Result<Self, TokenizationError> {
        // Empty character literal error
        if let Some((_, '\'')) = iter.peek() {
            iter.next();

            handler.recieve(
                EmptyCharacterLiteral {
                    span: Self::create_span(start, iter),
                }
                .into(),
            );

            Err(TokenizationError::FatalLexicalError)
        }
        // Might found a character literal or a single quote punctuation
        else {
            // If we found a backslash, we need to look ahead by two characters to check if the
            // next character is a single quote. Otherwise, we only need to look ahead by one

            let is_escape_character_sequence = matches!(iter.peek(), Some((_, '\\')));
            let look_ahead_count = if is_escape_character_sequence { 2 } else { 1 };
            let mut cloned_iter = iter.clone();

            if matches!(cloned_iter.nth(look_ahead_count), Some((_, '\''))) {
                // Maps the escape character to the actual value
                if is_escape_character_sequence {
                    iter.next();

                    let (escape_source_location, escape_character) = iter.next().unwrap();
                    let escape_character = Self::map_escape_character(escape_character);

                    let mut end_escape_character_iter = iter.clone();

                    iter.next();

                    escape_character.map_or_else(
                        || {
                            handler.recieve(
                                InvalidEscapeCharacterSequences {
                                    span: Self::create_span(
                                        escape_source_location - 1,
                                        &mut end_escape_character_iter,
                                    ),
                                }
                                .into(),
                            );

                            Err(TokenizationError::FatalLexicalError)
                        },
                        |character| {
                            Ok(CharacterLiteral {
                                span: Self::create_span(start, iter),
                                character,
                            }
                            .into())
                        },
                    )
                }
                // Found a normal character literal
                else {
                    let character = iter.next().unwrap().1;

                    iter.next();

                    // The control characters must be escaped
                    if character.is_control() {
                        handler.recieve(
                            ControlCharactersMustBeEscaped {
                                span: Self::create_span(start, iter),
                            }
                            .into(),
                        );
                        Err(TokenizationError::FatalLexicalError)
                    } else {
                        Ok(CharacterLiteral {
                            span: Self::create_span(start, iter),
                            character,
                        }
                        .into())
                    }
                }
            }
            // It's just a single quote punctuation
            else {
                Ok(Punctuation {
                    span: Self::create_span(start, iter),
                    punctuation: '\'',
                }
                .into())
            }
        }
    }

    fn handle_numeric_literal(iter: &mut pernixc_source::Iterator, start: ByteIndex) -> Self {
        // Tokenizes the whole number part
        Self::walk_iter(iter, |character| character.is_ascii_digit());

        // Tokenizes the fractional part
        if let Some((_, '.')) = iter.peek() {
            let iter_clone = iter.clone();

            iter.next();

            // Checks if the fractional part is followed by a digit
            if let Some((_, character)) = iter.peek() {
                if character.is_ascii_digit() {
                    iter.next();

                    Self::walk_iter(iter, |character| character.is_ascii_digit());
                } else {
                    *iter = iter_clone;
                }
            } else {
                *iter = iter_clone;
            }
        }

        let mut value_iter_end = iter.clone();

        // Tokenizes the suffix
        let suffix_span = if let Some((_, character)) = iter.peek() {
            if Self::is_first_identifier_character(character) {
                Self::walk_iter(iter, Self::is_identifier_character);
                Some(Self::create_span(value_iter_end.peek().unwrap().0, iter))
            } else {
                None
            }
        } else {
            None
        };

        NumericLiteral {
            span: Self::create_span(start, iter),
            value_span: Self::create_span(start, &mut value_iter_end),
            suffix_span,
        }
        .into()
    }

    /// Tokenizes the source code from the given iterator.
    ///
    /// The tokenization starts at the current location of the iterator. The function moves the
    /// iterator at least once and forwards it until it makes a token. After the token is made, the
    /// iterator is left at the next character that is not part of the token.
    ///
    /// # Errors
    /// - [`TokenizationError::EndOfSourceCodeIteratorArgument`] - The iterator argument is at the
    ///   end of the source code.
    /// - [`TokenizationError::FatalLexicalError`] - A fatal lexical error occurred.
    pub fn tokenize(
        iter: &mut pernixc_source::Iterator,
        handler: &impl Handler<Error>,
    ) -> Result<Self, TokenizationError> {
        // Gets the first character
        let (start, character) = iter
            .next()
            .ok_or(TokenizationError::EndOfSourceCodeIteratorArgument)?;

        // Found whitespaces
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
        // Found a string literal
        else if character == '"' {
            Self::handle_string_literal(iter, start, handler).map_err(Into::into)
        }
        // Found a character literal
        else if character == '\'' {
            Self::handle_character_literal(iter, start, handler).map_err(Into::into)
        }
        // Found numeric literal
        else if character.is_ascii_digit() {
            Ok(Self::handle_numeric_literal(iter, start))
        }
        // Found a punctuation
        else if character.is_ascii_punctuation() {
            Ok(Punctuation {
                span: Self::create_span(start, iter),
                punctuation: character,
            }
            .into())
        } else {
            todo!()
        }
    }
}

#[cfg(test)]
pub mod tests;
