//! Is a module containing the [`Token`] type and all of its related types.

use std::{
    hash::Hash,
    iter::{Iterator, Peekable},
    str::{CharIndices, FromStr},
    sync::LazyLock,
};

use bimap::BiHashMap;
use flexstr::ToFlex;
use pernixc_handler::Handler;
use pernixc_source_file::{AbsoluteSpan, ByteIndex, GlobalSourceID, Span};
use serde::{Deserialize, Serialize};

use crate::{
    error::{self, InvalidEscapeSequence},
    kind,
};

// #[cfg(any(test, feature = "arbitrary"))]
// pub mod arbitrary;

/// Type alias for [`Token`] categorized as a [`kind::Keyword`].
pub type Keyword<S> = Token<kind::Keyword, S>;

/// Type alias for [`Token`] categorized as a [`kind::NewLine`].
pub type NewLine<S> = Token<kind::NewLine, S>;

/// Type alias for [`Token`] categorized as a [`kind::Character`].
pub type Character<S> = Token<kind::Character, S>;

/// Type alias for [`Token`] categorized as a [`kind::String`].
pub type String<S> = Token<kind::String, S>;

/// Type alias for [`Token`] categorized as a [`kind::Identifier`].
pub type Identifier<S> = Token<kind::Identifier, S>;

/// Type alias for [`Token`] categorized as a [`kind::Punctuation`].
pub type Punctuation<S> = Token<kind::Punctuation, S>;

/// Type alias for [`Token`] categorized as a [`kind::Numeric`].
pub type Numeric<S> = Token<kind::Numeric, S>;

/// Type alias for [`Token`] that uses [`kind::Kind`] to differentiate between
/// different kinds of tokens.
pub type Kind<S> = Token<kind::Kind, S>;

/// A template struct representing a token that contains the location, kind, and
/// its prior insignificant part.
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
pub struct Token<T, S> {
    /// Specifies the kind of the token.
    pub kind: T,

    /// The span of the significant part of the token.
    pub span: S,

    /// The span of the insignificant part of the token.
    ///
    /// This can be whitespaces or comments that are preceding the token.
    /// With this information, it's possible to reconstruct the original source
    /// code with the same formatting (lossless).
    pub prior_insignificant: Option<S>,
}

impl<T, S> Token<T, S> {
    /// Calls the given function with the [`Self::kind`] of the token and
    /// returns a new token with the result of the function where the same
    /// [`Self::span`] and [`Self::prior_insignificant`] are used.
    pub fn map_kind<U>(self, f: impl FnOnce(T) -> U) -> Token<U, S> {
        Token {
            kind: f(self.kind),
            span: self.span,
            prior_insignificant: self.prior_insignificant,
        }
    }
}

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
#[derive(Clone)]
pub struct Tokenizer<'a, 'h> {
    source: &'a str,
    iter: Peekable<CharIndices<'a>>,
    handler: &'h dyn Handler<error::Error>,
    source_id: GlobalSourceID,
}

impl std::fmt::Debug for Tokenizer<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Tokenizer")
            .field("source", &self.source)
            .field("iter", &self.iter)
            .field("source_id", &self.source_id)
            .finish_non_exhaustive()
    }
}

impl<'a, 'h> Tokenizer<'a, 'h> {
    /// Creates a new [`Tokenizer`] instance.
    ///
    /// # Parameters
    ///
    /// - `source`: The source code to tokenize.
    /// - `source_id`: The ID of the source code used for creating spans.
    /// - `handler`: The handler to use for reporting errors.
    pub fn new(
        source: &'a str,
        source_id: GlobalSourceID,
        handler: &'h dyn Handler<error::Error>,
    ) -> Self {
        Self {
            source,
            iter: source.char_indices().peekable(),
            handler,
            source_id,
        }
    }
}
/// A bidirectional map that maps a escape sequence (on the left) to its
/// representation (on the right).
pub static ESCAPE_SEQUENCE_BY_REPRESENTATION: LazyLock<BiHashMap<char, char>> =
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

impl Tokenizer<'_, '_> {
    /// Creates a span from the given start location to the current location of
    /// the iterator.
    fn create_span(&mut self, start: ByteIndex) -> AbsoluteSpan {
        if let Some((index, _)) = self.iter.peek().copied() {
            Span { start, end: index, source_id: self.source_id }
        } else {
            Span { start, end: self.source.len(), source_id: self.source_id }
        }
    }

    fn handle_insignificant(&mut self) -> Option<AbsoluteSpan> {
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

            Some(Span::new(start, index, self.source_id))
        } else {
            Some(Span::new(start, self.source.len(), self.source_id))
        }
    }

    fn handle_new_line(
        &mut self,
        character: char,
        start: ByteIndex,
    ) -> AbsoluteSpan {
        // cr
        if character == '\r' {
            //  lf
            if matches!(self.iter.peek(), Some((_, '\n'))) {
                // crlf
                self.iter.next();
            }
        }

        self.create_span(start)
    }

    fn handle_identifier_and_keyword(
        &mut self,
        start: ByteIndex,
    ) -> (kind::Kind, AbsoluteSpan) {
        walk_iter(&mut self.iter, is_identifier_character);

        let span = self.create_span(start);
        let word = &self.source[start..span.end];

        // Checks if the word is a keyword
        kind::Keyword::from_str(word).map_or_else(
            |_| {
                (kind::Kind::Identifier(kind::Identifier(word.to_flex())), span)
            },
            |kind| (kind::Kind::Keyword(kind), span),
        )
    }

    fn handle_numeric_literal(&mut self, start: ByteIndex) -> AbsoluteSpan {
        // Tokenizes the whole number part
        walk_iter(&mut self.iter, |character| character.is_ascii_digit());

        self.create_span(start)
    }

    fn handle_single_quote(
        &mut self,
        start: ByteIndex,
    ) -> (kind::Kind, AbsoluteSpan) {
        let mut iter_cloned = self.iter.clone();

        match iter_cloned.next() {
            // escaped character
            Some((content_start, mut char)) => {
                // skipcq: RS-W1209
                let is_escaped = if char == '\\' {
                    // eat the next character
                    let Some((_, new_char)) = iter_cloned.next() else {
                        return (
                            kind::Kind::Punctuation(kind::Punctuation('\'')),
                            self.create_span(start),
                        );
                    };

                    char = new_char;

                    true
                } else {
                    false
                };

                if !is_escaped && char == '\'' {
                    return (
                        kind::Kind::Punctuation(kind::Punctuation('\'')),
                        self.create_span(start),
                    );
                }

                match iter_cloned.next() {
                    // a caharceter literal
                    Some((content_end, '\'')) => {
                        if is_escaped {
                            self.iter.next(); // eat the backslash
                        }

                        self.iter.next(); // eat the character
                        self.iter.next(); // eat the closing quote

                        (
                            kind::Kind::Character(kind::Character(
                                if !is_escaped {
                                    char
                                } else if let Some(value) =
                                    ESCAPE_SEQUENCE_BY_REPRESENTATION
                                        .get_by_left(&char)
                                        .copied()
                                {
                                    value
                                } else {
                                    self.handler.receive(
                                        error::Error::InvalidEscapeSequence(
                                            InvalidEscapeSequence {
                                                span: Span::new(
                                                    content_start,
                                                    content_end,
                                                    self.source_id,
                                                ),
                                            },
                                        ),
                                    );

                                    char
                                },
                            )),
                            self.create_span(start),
                        )
                    }

                    _ => (
                        kind::Kind::Punctuation(kind::Punctuation('\'')),
                        self.create_span(start),
                    ),
                }
            }

            None => (
                kind::Kind::Punctuation(kind::Punctuation('\'')),
                self.create_span(start),
            ),
        }
    }

    fn handle_string_literal(
        &mut self,
        start: ByteIndex,
    ) -> (std::string::String, AbsoluteSpan) {
        let mut last_backslash = false;
        let mut last_byte_index = start;
        let mut string = std::string::String::default();

        loop {
            let Some((byte_index, character)) = self.iter.next() else {
                self.handler.receive(error::Error::UnterminatedStringLiteral(
                    error::UnterminatedStringLiteral {
                        span: Span::new(start, start + 1, self.source_id),
                    },
                ));

                return (
                    string,
                    Span::new(start, self.source.len(), self.source_id),
                );
            };

            if last_backslash {
                if let Some(value) = ESCAPE_SEQUENCE_BY_REPRESENTATION
                    .get_by_left(&character)
                    .copied()
                {
                    string.push(value);
                } else {
                    self.handler.receive(error::Error::InvalidEscapeSequence(
                        InvalidEscapeSequence {
                            span: Span::new(
                                last_byte_index,
                                byte_index,
                                self.source_id,
                            ),
                        },
                    ));

                    string.push(character);
                }

                last_backslash = false;
            } else {
                match character {
                    // end the string
                    '"' => return (string, self.create_span(start)),

                    // escape sequence
                    '\\' => {
                        last_backslash = true;
                    }

                    // normal character
                    _ => {
                        string.push(character);
                        last_backslash = false;
                    }
                }
            }

            last_byte_index = byte_index;
        }
    }
}

impl Iterator for Tokenizer<'_, '_> {
    type Item = Kind<AbsoluteSpan>;

    fn next(&mut self) -> Option<Self::Item> {
        // Found white spaces
        let prior_insignificant = self.handle_insignificant();

        // Gets the first character
        let (start, character) = self.iter.next()?;

        // Found new line character
        let (kind, span) = if character == '\n' || character == '\r' {
            (
                kind::Kind::NewLine(kind::NewLine),
                self.handle_new_line(character, start),
            )
        }
        // Found identifier/keyword
        else if is_first_identifier_character(character) {
            self.handle_identifier_and_keyword(start)
        }
        // Found numeric literal
        else if character.is_ascii_digit() {
            let span = self.handle_numeric_literal(start);
            (
                kind::Kind::Numeric(kind::Numeric(
                    self.source[span.range()].into(),
                )),
                span,
            )
        }
        // Might found a character literal
        else if character == '\'' {
            self.handle_single_quote(start)
        }
        // Found a string literal
        else if character == '"' {
            let (string, span) = self.handle_string_literal(start);

            (kind::Kind::String(kind::String(string.into())), span)
        }
        // Found a punctuation
        else if character.is_ascii_punctuation() {
            (
                kind::Kind::Punctuation(kind::Punctuation(character)),
                self.create_span(start),
            )
        } else {
            unreachable!("should've been handled by earlier cases")
        };

        Some(Token { kind, span, prior_insignificant })
    }
}

// #[cfg(test)]
// pub(crate) mod test;
