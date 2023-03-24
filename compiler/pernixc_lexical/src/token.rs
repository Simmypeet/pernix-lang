//! Contains the definition of the [`Token`] and its variants.

use std::{collections::HashMap, hash::Hash, str::FromStr};

use enum_as_inner::EnumAsInner;
use lazy_static::lazy_static;
use pernixc_common::source_file::{Location, SourceFile, SourceFileIterator, Span, SpanEnding};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use thiserror::Error;

use crate::errors::LexicalError;

/// Is an enumeration representing keywords in the Pernix programming language.
///
/// Most enum variants names are the same as the keyword they represent, except that the name is
/// capitalized while the keyword is not. For example, the `function` keyword is represented by the
/// `Function` variant.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EnumIter, PartialOrd, Ord)]
pub enum Keyword {
    Function,
    Public,
    Struct,
    Interface,
    Implement,
    Let,
    Const,
    If,
    Else,
    Mutable,
    While,
    Break,
    Continue,
    Return,
    True,
    False,
    Void,
    Bool,
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
    This,
    SelfKeyword,
    And,
    Or,
    Loop,
    Express,
    Enum,
    Private,
    Internal,
    Module,
}

impl ToString for Keyword {
    fn to_string(&self) -> String { self.as_str().to_string() }
}

/// Is an error that is returned when a string cannot be parsed into a [`Keyword`] in [`FromStr`]
/// trait implementation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Error)]
#[error("Invalid string representation of keyword.")]
pub struct KeywordParseError;

impl FromStr for Keyword {
    type Err = KeywordParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref STRING_KEYWORD_MAP: HashMap<&'static str, Keyword> = {
                let mut map = HashMap::new();

                for keyword in Keyword::iter() {
                    map.insert(keyword.as_str(), keyword);
                }

                map
            };
        }

        STRING_KEYWORD_MAP
            .get(s)
            .cloned()
            .map_or(Err(KeywordParseError), Ok)
    }
}

impl Keyword {
    /// Gets the string representation of the keyword as a `&str`.
    pub fn as_str(self) -> &'static str {
        match self {
            Keyword::Enum => "enum",
            Keyword::Struct => "struct",
            Keyword::Express => "express",
            Keyword::Loop => "loop",
            Keyword::Function => "function",
            Keyword::Public => "public",
            Keyword::Interface => "interface",
            Keyword::Implement => "implement",
            Keyword::Let => "let",
            Keyword::Const => "const",
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::While => "while",
            Keyword::Mutable => "mutable",
            Keyword::Break => "break",
            Keyword::Continue => "continue",
            Keyword::Return => "return",
            Keyword::True => "true",
            Keyword::False => "false",
            Keyword::Void => "void",
            Keyword::Bool => "bool",
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
            Keyword::This => "this",
            Keyword::SelfKeyword => "self",
            Keyword::And => "and",
            Keyword::Or => "or",
            Keyword::Private => "private",
            Keyword::Internal => "internal",
            Keyword::Module => "module",
        }
    }

    pub fn from_string(str: &str) -> Option<Keyword> {
        lazy_static! {
            static ref STRING_KEYWORD_MAP: HashMap<&'static str, Keyword> = {
                let mut map = HashMap::new();

                for keyword in Keyword::iter() {
                    map.insert(keyword.as_str(), keyword);
                }

                map
            };
        }

        STRING_KEYWORD_MAP.get(str).copied()
    }
}

/// Is an enumeration containing all kinds of tokens in the Pernix programming language.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Token {
    WhiteSpace(WhiteSpaceToken),
    Identifier(IdentifierToken),
    Keyword(KeywordToken),
    Punctuation(PunctuationToken),
    NumericLiteral(NumericLiteralToken),
    StringLiteral(StringLiteralToken),
    CharacterLiteral(CharacterLiteralToken),
    Comment(CommentToken),
}

impl Token {
    /// Gets the range of characters in the source file that make up this [`Token`].
    pub fn span(&self) -> Span {
        match self {
            Token::WhiteSpace(token) => token.span,
            Token::Identifier(token) => token.span,
            Token::Keyword(token) => token.span,
            Token::Punctuation(token) => token.span,
            Token::NumericLiteral(token) => token.span,
            Token::StringLiteral(token) => token.span,
            Token::CharacterLiteral(token) => token.span,
            Token::Comment(token) => token.span,
        }
    }
}

/// Represents a contiguous sequence of whitespace characters.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WhiteSpaceToken {
    /// Is the span that makes up the token.
    pub span: Span,
}

/// Represents a contiguous sequence of characters that are valid in an identifier.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IdentifierToken {
    /// Is the span that makes up the token.
    pub span: Span,
}

/// Represents a contiguous sequence of characters that are reserved for a keyword.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct KeywordToken {
    /// Is the span that makes up the token.
    pub span: Span,

    /// Is the [`Keyword`] that the token represents.
    pub keyword: Keyword,
}

/// Represents a single ASCII punctuation character.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PunctuationToken {
    /// Is the span that makes up the token.
    pub span: Span,

    /// Is the ASCII punctuation character that the token represents.
    pub punctuation: char,
}

/// Represents a hardcoded numeric literal value in the source code.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NumericLiteralToken {
    /// Is the span that makes up the token.
    pub span: Span,

    /// Is the sapn representing the value part of the numeric literal.
    pub value_span: Span,

    /// Is the span representing the suffix part of the numeric literal if it exists.
    pub suffix_span: Option<Span>,
}

/// Represents a single character or escape sequence enclosed in single quotes.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CharacterLiteralToken {
    ///  The span that makes up the token.
    pub span: Span,

    /// The character value of the literal.
    pub character: char,
}

/// Represents a contiguous sequence of characters enclosed in double quotes.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StringLiteralToken {
    /// Is the span that makes up the token.
    pub span: Span,
}

/// Is an enumeration representing the two kinds of comments in the Pernix programming language.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CommentKind {
    /// A comment that starts with `//` and ends at the end of the line.
    SingleLine,

    /// A comment that starts with `/*` and ends with `*/`.
    Delimited,
}

/// Represents a portion of the source code that is ignored by the compiler.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CommentToken {
    /// Is the span that makes up the token.
    pub span: Span,

    /// Is the kind of comment that the token represents.
    pub kind: CommentKind,
}

/// Is an error that can occur when invoking the [Token::tokenize()](Token::tokenize()) method.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, Error)]
pub enum TokenizationError {
    #[error("Encountered an lexical error while tokenizing the source code.")]
    Lexical(#[from] LexicalError),

    #[error("The iterator argument is at the end of the source code.")]
    EndOfSourceCodeIteratorArgument,
}

impl Token {
    /// Tokenizes the source code from the given iterator.
    ///
    /// The tokenization starts at the current location of the iterator. The function moves the
    /// iterator at least once and forwards it until it makes a token. After the token is made, the
    /// iterator is left at the next character that is not part of the token.
    ///
    /// # Errors
    /// - [`TokenizationError::EndOfSourceCodeIteratorArgument`] - The iterator argument is at the
    ///   end of the source code.
    /// - [`TokenizationError::Lexical`] - Any lexical error encountered in the source code while
    ///   tokenizing.
    pub fn tokenize(iter: &mut SourceFileIterator) -> Result<Token, TokenizationError> {
        /// Increments the iterator until the predicate returns false.
        fn walk_iter(iter: &mut SourceFileIterator, predicate: impl Fn(char) -> bool) {
            while let Some((_, character)) = iter.peek() {
                if !predicate(character) {
                    break;
                }

                iter.next();
            }
        }

        /// Creates a span from the given start location to the current location of the iterator.
        fn create_span(start: Location, iter: &mut SourceFileIterator) -> Span {
            iter.peek().map_or(
                Span {
                    start,
                    end: SpanEnding::EndOfFile,
                },
                |(location, _)| Span {
                    start,
                    end: SpanEnding::Location(location),
                },
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

        // Gets the first character
        let (source_location, character) = iter
            .next()
            .ok_or(TokenizationError::EndOfSourceCodeIteratorArgument)?;

        // Found whitespaces
        if character.is_whitespace() {
            walk_iter(iter, |character| character.is_whitespace());

            Ok(Token::WhiteSpace(WhiteSpaceToken {
                span: create_span(source_location, iter),
            }))
        }
        // Found identifier/keyword
        else if is_first_identifier_character(character) {
            walk_iter(iter, is_identifier_character);

            let span = create_span(source_location, iter);
            let word = span
                .end
                .as_location()
                .map_or(&iter.source_code()[span.start.byte..], |end| {
                    &iter.source_code()[span.start.byte..end.byte]
                });

            // Checks if the word is a keyword
            Ok(if let Some(kw) = Keyword::from_string(word) {
                Token::Keyword(KeywordToken { span, keyword: kw })
            } else {
                Token::Identifier(IdentifierToken { span })
            })
        }
        // Found comment/single slash punctuation
        else if character == '/' {
            // Single line comment
            if let Some((_, '/')) = iter.peek() {
                iter.next();

                walk_iter(iter, |character| character != SourceFile::NEW_LINE);

                iter.next();

                Ok(Token::Comment(CommentToken {
                    span: create_span(source_location, iter),
                    kind: CommentKind::SingleLine,
                }))
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
                if !is_terminated {
                    Err(TokenizationError::Lexical(
                        LexicalError::UnterminatedDelimitedComment(create_span(
                            source_location,
                            iter,
                        )),
                    ))
                } else {
                    Ok(Token::Comment(CommentToken {
                        span: create_span(source_location, iter),
                        kind: CommentKind::Delimited,
                    }))
                }
            }
            // Just a single slash punctuation
            else {
                Ok(Token::Punctuation(PunctuationToken {
                    span:        create_span(source_location, iter),
                    punctuation: character,
                }))
            }
        }
        // Found a string literal
        else if character == '"' {
            let mut invalid_escape_characters = Vec::new();

            loop {
                let character = iter.next().map_or('\0', |x| x.1);

                // Stops when the string literal is terminated
                if character == '"' {
                    break;
                }
                // Handles escape characters
                else if character == '\\' {
                    let escape_character = iter.next();
                    let (location, character) =
                        if let Some((location, character)) = escape_character {
                            (location, character)
                        } else {
                            continue;
                        };

                    let escape_character = map_escape_character(character);

                    if escape_character.is_none() {
                        invalid_escape_characters.push(create_span(location, iter))
                    }
                }
                // Unterminated string literal
                else if character == '\0' {
                    return Err(TokenizationError::Lexical(
                        LexicalError::UnterminatedStringLiteral(create_span(source_location, iter)),
                    ));
                }
            }

            // No error found, returns the string literal token
            if invalid_escape_characters.is_empty() {
                Ok(Token::StringLiteral(StringLiteralToken {
                    span: create_span(source_location, iter),
                }))
            }
            // Found invalid escape characters
            else {
                Err(TokenizationError::Lexical(
                    LexicalError::InvalidEscapeCharacterSequences(invalid_escape_characters),
                ))
            }
        }
        // Found a character literal
        else if character == '\'' {
            // Empty character literal error
            if let Some((_, '\'')) = iter.peek() {
                iter.next();

                Err(TokenizationError::Lexical(
                    LexicalError::EmptyCharacterLiteral(create_span(source_location, iter)),
                ))
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
                        let escape_character = map_escape_character(escape_character);

                        let mut end_escape_character_iter = iter.clone();

                        iter.next();

                        if let Some(character) = escape_character {
                            Ok(Token::CharacterLiteral(CharacterLiteralToken {
                                span: create_span(source_location, iter),
                                character,
                            }))
                        } else {
                            Err(TokenizationError::Lexical(
                                LexicalError::InvalidEscapeCharacterSequences(vec![create_span(
                                    escape_source_location,
                                    &mut end_escape_character_iter,
                                )]),
                            ))
                        }
                    }
                    // Found a normal character literal
                    else {
                        let character = iter.next().unwrap().1;

                        iter.next();

                        // The control characters must be escaped
                        if character.is_control() {
                            Err(TokenizationError::Lexical(
                                LexicalError::ControlCharactersMustBeEscaped(create_span(
                                    source_location,
                                    iter,
                                )),
                            ))
                        } else {
                            Ok(Token::CharacterLiteral(CharacterLiteralToken {
                                span: create_span(source_location, iter),
                                character,
                            }))
                        }
                    }
                }
                // It's just a single quote punctuation
                else {
                    Ok(Token::Punctuation(PunctuationToken {
                        span:        create_span(source_location, iter),
                        punctuation: '\'',
                    }))
                }
            }
        }
        // Found numeric literal
        else if character.is_ascii_digit() {
            // Tokenizes the whole number part
            walk_iter(iter, |character| character.is_ascii_digit());

            // Tokenizes the fractional part
            if let Some((_, '.')) = iter.peek() {
                let iter_clone = iter.clone();

                iter.next();

                // Checks if the fractional part is followed by a digit
                if let Some((_, character)) = iter.peek() {
                    if character.is_ascii_digit() {
                        iter.next();

                        walk_iter(iter, |character| character.is_ascii_digit());
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
                if is_first_identifier_character(character) {
                    walk_iter(iter, is_identifier_character);
                    Some(create_span(value_iter_end.peek().unwrap().0, iter))
                } else {
                    None
                }
            } else {
                None
            };

            Ok(Token::NumericLiteral(NumericLiteralToken {
                span: create_span(source_location, iter),
                value_span: create_span(source_location, &mut value_iter_end),
                suffix_span,
            }))
        }
        // Found a punctuation
        else if character.is_ascii_punctuation() {
            Ok(Token::Punctuation(PunctuationToken {
                span:        create_span(source_location, iter),
                punctuation: character,
            }))
        } else {
            panic!("Everything should be handled by now.")
        }
    }
}

#[cfg(test)]
mod tests;
