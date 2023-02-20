use std::{collections::HashMap, hash::Hash, iter::Peekable};

use enum_as_inner::EnumAsInner;
use enum_dispatch::enum_dispatch;
use getset::Getters;
use lazy_static::lazy_static;
use pernixc_common::source_file::{SourceFile, SourceFileIterator, SourceLocation, Span};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use thiserror::Error;

/// Is an enumeration of all the keywords in the language.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EnumIter)]
pub enum Keyword {
    /// `function` keyword.
    Function,

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
    UInt8,

    /// `uint16` keyword.
    UInt16,

    /// `uint32` keyword.
    UInt32,

    /// `uint64` keyword.
    UInt64,

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
}

impl Keyword {
    /// Gets the string representation of this keyword.
    pub fn string(&self) -> &'static str {
        match self {
            Keyword::Function => "function",
            Keyword::Public => "public",
            Keyword::Struct => "struct",
            Keyword::Interface => "interface",
            Keyword::Implement => "implement",
            Keyword::Let => "let",
            Keyword::Const => "const",
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::While => "while",
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
            Keyword::UInt8 => "uint8",
            Keyword::UInt16 => "uint16",
            Keyword::UInt32 => "uint32",
            Keyword::UInt64 => "uint64",
            Keyword::Float32 => "float32",
            Keyword::Float64 => "float64",
            Keyword::This => "this",
            Keyword::SelfKeyword => "self",
            Keyword::And => "and",
            Keyword::Or => "or",
        }
    }

    /// Gets the keyword that corresponds to the given string.
    pub fn from_string(str: &str) -> Option<Keyword> {
        lazy_static! {
            static ref STRING_KEYWORD_MAP: HashMap<&'static str, Keyword> = {
                let mut map = HashMap::new();

                for keyword in Keyword::iter() {
                    map.insert(keyword.string(), keyword);
                }

                map
            };
        }

        STRING_KEYWORD_MAP.get(str).copied()
    }
}

/// Is a trait that all kinds of tokens must implement.
#[enum_dispatch]
pub trait TokenTrait<'a> {
    /// Gets the span of this token.
    fn span(&self) -> Span<'a>;

    /// Gets the lexeme of this token.
    fn lexeme(&self) -> &'a str {
        self.span().string()
    }
}

/// Represents a unit of grammar in the source code, the main unit of the lexical analysis.
#[derive(Debug, Clone, Copy, EnumAsInner)]
#[enum_dispatch(TokenTrait)]
pub enum Token<'a> {
    WhiteSpace(WhiteSpaceToken<'a>),
    Identifier(IdentifierToken<'a>),
    Keyword(KeywordToken<'a>),
    Punctuation(PunctuationToken<'a>),
    NumericLiteral(NumericLiteralToken<'a>),
    StringLiteral(StringLiteralToken<'a>),
    CharacterLiteral(CharacterLiteralToken<'a>),
    Comment(CommentToken<'a>),
}

/// Is a token that represents a consecutive sequence of whitespace characters.
#[derive(Debug, Clone, Copy, Getters)]
pub struct WhiteSpaceToken<'a> {
    span: Span<'a>,
}

impl<'a> TokenTrait<'a> for WhiteSpaceToken<'a> {
    fn span(&self) -> Span<'a> {
        self.span
    }
}

/// Is a token that represents a consecutive sequence of alphanumeric/non-punctuation characters.
#[derive(Debug, Clone, Copy, Getters)]
pub struct IdentifierToken<'a> {
    span: Span<'a>,
}

impl<'a> TokenTrait<'a> for IdentifierToken<'a> {
    fn span(&self) -> Span<'a> {
        self.span
    }
}

/// Is a token that represents a consecutive sequence of alphanumeric/non-punctuation characters
/// that is a keyword.
#[derive(Debug, Clone, Copy, Getters)]
pub struct KeywordToken<'a> {
    span: Span<'a>,

    /// Gets the keyword that this token represents.
    #[get = "pub"]
    keyword: Keyword,
}

impl<'a> TokenTrait<'a> for KeywordToken<'a> {
    fn span(&self) -> Span<'a> {
        self.span
    }
}

/// Is a token that represents a single punctuation character.
#[derive(Debug, Clone, Copy, Getters)]
pub struct PunctuationToken<'a> {
    span: Span<'a>,

    /// Gets the punctuation character that this token represents.
    #[get = "pub"]
    punctuation: char,
}

impl<'a> TokenTrait<'a> for PunctuationToken<'a> {
    fn span(&self) -> Span<'a> {
        self.span
    }
}

/// Is a token that represents a hardcoded numeric literal, both integer and floating point.
#[derive(Debug, Clone, Copy, Getters)]
pub struct NumericLiteralToken<'a> {
    span: Span<'a>,

    /// Gets the span that contains the value of this numeric literal (suffix is not included).
    /// For example, the value span of `1234.32u32` is `1234.32`.
    #[get = "pub"]
    value_span: Span<'a>,

    /// Gets the span that contains the suffix of this numeric literal (value is not included).
    /// For example, the suffix span of `1234.32u32` is `u32`. If this token does not have a suffix,
    /// this will be `None`.
    #[get = "pub"]
    suffix_span: Option<Span<'a>>,
}

impl<'a> TokenTrait<'a> for NumericLiteralToken<'a> {
    fn span(&self) -> Span<'a> {
        self.span
    }
}

/// Is a token that represents a hardcoded character literal (single character). For example,
/// `'a'`.
#[derive(Debug, Clone, Copy, Getters)]
pub struct CharacterLiteralToken<'a> {
    span: Span<'a>,

    /// Gets the character that this token represents.
    #[get = "pub"]
    character: char,
}

impl<'a> TokenTrait<'a> for CharacterLiteralToken<'a> {
    fn span(&self) -> Span<'a> {
        self.span
    }
}

/// Is a token that represents a hardcoded string literal. For example, `"Hello, world!"`.
#[derive(Debug, Clone, Copy, Getters)]
pub struct StringLiteralToken<'a> {
    span: Span<'a>,
}

impl<'a> TokenTrait<'a> for StringLiteralToken<'a> {
    fn span(&self) -> Span<'a> {
        self.span
    }
}

/// Is an enumeration of the kinds of comments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CommentKind {
    /// The comment starts with `//` and ends with a newline. The newline is included in the span.
    SingleLine,

    /// The comment starts with `/*` and ends with `*/`. The `/*` and `*/` are included in the span.
    Delimited,
}

/// Is a token that represents a comment, either a single line or a delimited comment.
#[derive(Debug, Clone, Copy, Getters)]
pub struct CommentToken<'a> {
    span: Span<'a>,

    /// Gets the kind of comment that this token represents.
    #[get = "pub"]
    kind: CommentKind,
}

impl<'a> TokenTrait<'a> for CommentToken<'a> {
    fn span(&self) -> Span<'a> {
        self.span
    }
}

/// Is an error that occurs during the process of [`Token::tokenize`]. This error is caused by the
/// lexical structure of the source code.
#[derive(Debug, Clone, EnumAsInner)]
pub enum LexicalError<'a> {
    /// Found invalid escape character sequences.
    ///
    /// Since invalid escape character sequences can be encountered many times in a string literal,
    /// therefore, the error variant holds a list of spans.
    InvalidEscapeCharacterSequences(Vec<Span<'a>>),

    /// Found an unterminated string literal.
    UnterminatedStringLiteral(Span<'a>),

    /// Found an unterminated delimited comment.
    UnterminatedDelimitedComment(Span<'a>),

    /// Found an empty character literal.
    EmptyCharacterLiteral(Span<'a>),

    /// Found a control character in the character literal but it is not escaped.
    ControlCharactersMustBeEscaped(Span<'a>),
}

/// Is an error that occurs during the process of [`Token::tokenize`].
#[derive(Debug, Clone, Error, EnumAsInner)]
pub enum TokenizationError<'a> {
    /// An error that occurs during the process of tokenization due to the lexical structure of the
    /// source code.
    #[error("Encountered an lexical error while tokenizing the source code.")]
    Lexical(LexicalError<'a>),

    /// The given iterator argument is at the end of the source code.
    #[error("The iterator argument is at the end of the source code.")]
    EndOfSourceCodeIteratorArgument,
}

impl<'a> Token<'a> {
    /// Tokenizes the source code from the given iterator. The iterator will be walked at least once
    /// After the tokenization is done, the iterator will be at the start of the next token.
    ///
    /// # Errors
    /// - If the iterator is at the end of the source code.
    pub fn tokenize(
        iter: &mut Peekable<SourceFileIterator<'a>>,
    ) -> Result<Token<'a>, TokenizationError<'a>> {
        /// Walks the iterator until the predicate returns false.
        fn walk_iter(iter: &mut Peekable<SourceFileIterator>, predicate: impl Fn(char) -> bool) {
            while let Some((_, character)) = iter.peek() {
                if !predicate(*character) {
                    break;
                }

                iter.next();
            }
        }

        /// Gets an iterator based on the given start location and the iterator.
        fn create_span<'a>(
            start: SourceLocation<'a>,
            iter: &mut Peekable<SourceFileIterator<'a>>,
        ) -> Span<'a> {
            iter.peek()
                .map_or(Span::from_source_location_to_end(start), |(end, _)| {
                    Span::from_source_location(start, *end)
                        .expect("The end location is before the start location.")
                })
        }

        /// Checks if the given character is a valid first character for an identifier.
        fn is_first_identifier_character(character: char) -> bool {
            character == '_'
                || character == '@'
                || (!character.is_control()
                    && !character.is_whitespace()
                    && !character.is_ascii_punctuation()
                    && !character.is_ascii_digit())
        }

        /// Checks if the given character is a valid character for an identifier.
        fn is_identifier_character(character: char) -> bool {
            character == '_'
                || (!character.is_control()
                    && !character.is_whitespace()
                    && !character.is_ascii_punctuation())
        }

        /// Maps the given character followed by backslash to its escape character value.
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

        // Based on the first character, determines the kind of token that this is.
        let (source_location, character) = iter
            .next()
            .ok_or(TokenizationError::EndOfSourceCodeIteratorArgument)?;

        // Handle whitespace
        if character.is_whitespace() {
            walk_iter(iter, |character| character.is_whitespace());

            Ok(Token::WhiteSpace(WhiteSpaceToken {
                span: create_span(source_location, iter),
            }))
        }
        // Handle identifer / keyword
        else if is_first_identifier_character(character) {
            walk_iter(iter, is_identifier_character);

            let span = create_span(source_location, iter);

            Ok(Keyword::from_string(span.string())
                .map_or(Token::Identifier(IdentifierToken { span }), |keyword| {
                    Token::Keyword(KeywordToken { span, keyword })
                }))
        }
        // Handle comment
        else if character == '/' {
            // Single line comment
            if let Some((_, '/')) = iter.peek() {
                // Eat the second slash
                iter.next();

                walk_iter(iter, |character| character != SourceFile::NEW_LINE);

                // Eat the new line
                iter.next();

                Ok(Token::Comment(CommentToken {
                    span: create_span(source_location, iter),
                    kind: CommentKind::SingleLine,
                }))
            }
            // Delimited comment
            else if let Some((_, '*')) = iter.peek() {
                // Eat the second start
                iter.next();

                let mut is_terminated = false;

                while let Some((_, character)) = iter.next() {
                    if character == '*' {
                        if let Some((_, '/')) = iter.peek() {
                            // Eat the second slash
                            iter.next();

                            is_terminated = true;

                            break;
                        }
                    }
                }

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
            // Single slash punctuation
            else {
                Ok(Token::Punctuation(PunctuationToken {
                    span: create_span(source_location, iter),
                    punctuation: character,
                }))
            }
        }
        // Handle string literal
        else if character == '"' {
            // A list of invalid escape character sequences
            let mut invalid_escape_characters = Vec::new();

            loop {
                let character = iter.next().map_or('\0', |x| x.1);

                // End of string
                if character == '"' {
                    break;
                }
                // Found escape, skip a character
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
                // End of file
                else if character == '\0' {
                    return Err(TokenizationError::Lexical(
                        LexicalError::UnterminatedStringLiteral(create_span(source_location, iter)),
                    ));
                }
            }

            if invalid_escape_characters.is_empty() {
                Ok(Token::StringLiteral(StringLiteralToken {
                    span: create_span(source_location, iter),
                }))
            } else {
                Err(TokenizationError::Lexical(
                    LexicalError::InvalidEscapeCharacterSequences(invalid_escape_characters),
                ))
            }
        }
        // Handle character literal
        else if character == '\'' {
            // Empty character literal
            if let Some((_, '\'')) = iter.peek().cloned() {
                // Eat the single quote
                iter.next();

                Err(TokenizationError::Lexical(
                    LexicalError::EmptyCharacterLiteral(create_span(source_location, iter)),
                ))
            } else {
                let is_escape_character_sequence = matches!(iter.peek().cloned(), Some((_, '\\')));
                let look_ahead_count = if is_escape_character_sequence { 2 } else { 1 };
                let mut cloned_iter = iter.clone();

                // It is indeed a character literal
                if matches!(cloned_iter.nth(look_ahead_count), Some((_, '\''))) {
                    if is_escape_character_sequence {
                        // Eat backslash
                        iter.next();

                        // Get the escape character
                        let (escape_source_location, escape_character) = iter.next().unwrap();
                        let escape_character = map_escape_character(escape_character);

                        let mut end_escape_character_iter = iter.clone();

                        // Eat enclosing '
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
                    } else {
                        let character = iter.next().unwrap().1;

                        // Eat enclosing '
                        iter.next();

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
                // Just a single character literal
                else {
                    Ok(Token::Punctuation(PunctuationToken {
                        span: create_span(source_location, iter),
                        punctuation: '\'',
                    }))
                }
            }
        }
        // Handle numeric literal
        else if character.is_ascii_digit() {
            walk_iter(iter, |character| character.is_ascii_digit());

            // If the next character and followed by a dot and a digit, then eat the dot and the
            // digits following it
            if let Some((_, '.')) = iter.peek() {
                let iter_clone = iter.clone();

                // Eat the dot
                iter.next();

                if let Some((_, character)) = iter.peek() {
                    if character.is_ascii_digit() {
                        // Eat the dot
                        iter.next();

                        walk_iter(iter, |character| character.is_ascii_digit());
                    } else {
                        *iter = iter_clone;
                    }
                } else {
                    *iter = iter_clone;
                }
            }

            // Is an iterator that marks the end of the value
            let mut value_iter_end = iter.clone();

            // If the next character is an identifier character, then there is a suffix
            let suffix_span = if let Some((_, character)) = iter.peek().copied() {
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
        // Handle punctuation
        else if character.is_ascii_punctuation() {
            Ok(Token::Punctuation(PunctuationToken {
                span: create_span(source_location, iter),
                punctuation: character,
            }))
        } else {
            // Should be unreachable
            panic!("Everything should be handled by now.")
        }
    }
}

#[cfg(test)]
mod tests;
