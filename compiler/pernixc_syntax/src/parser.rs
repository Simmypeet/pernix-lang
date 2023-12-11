//! Contains the [`Parser`] logic.

use derive_more::{Deref, DerefMut};
use enum_as_inner::EnumAsInner;
use pernixc_base::diagnostic::Handler;
use pernixc_lexical::{
    token::{Identifier, Keyword, KeywordKind, Numeric, Punctuation, Token},
    token_stream::{Delimited, Delimiter, TokenStream, TokenTree},
};

use crate::error::{Error, SyntaxKind};

/// Provides a way to iterate over a token stream.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum TokenProvider<'a> {
    /// Iterating at the top level of the token stream.
    TokenStream(&'a TokenStream),

    /// Iterating inside a delimited token stream.
    Delimited(&'a Delimited),
}

impl<'a> TokenProvider<'a> {
    /// Gets the token stream of the current token provider.
    #[must_use]
    pub fn token_stream(&self) -> &'a TokenStream {
        match self {
            TokenProvider::TokenStream(token_stream) => token_stream,
            TokenProvider::Delimited(delimited) => &delimited.token_stream,
        }
    }
}

/// Represents a single frame of the parser's stack, responsible for reading a token stream in
/// that given token stream level.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Frame<'a> {
    token_provider: TokenProvider<'a>,
    current_index: usize,
}

/// Represents the read value of the [`Frame`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Reading {
    /// A singular token.
    Unit(Token),

    /// Found an openning delimiter token, which means that the parser can step into a new
    /// delimited frame.
    IntoDelimited(Delimiter, Punctuation),

    /// Found a closing delimiter token, which means that the parser should step out of the current
    /// delimited frame.
    DelimitedEnd(Delimiter, Punctuation),

    /// End of file.
    Eof,
}

impl Reading {
    /// Gets the read token inside the [`Reading`] as `Option<Token>`
    ///
    /// # Returns
    ///
    /// Returns `None` if the [`Reading`] is [`Reading::Eof`].
    #[must_use]
    pub fn into_token(self) -> Option<Token> {
        match self {
            Self::Unit(token) => Some(token),
            Self::IntoDelimited(_, punc) | Self::DelimitedEnd(_, punc) => {
                Some(Token::Punctuation(punc))
            }
            Self::Eof => None,
        }
    }
}

impl<'a> Frame<'a> {
    /// Checks if the current [`Frame`] doesn't have any more significant [`TokenTree`]s to
    /// parse.
    #[must_use]
    pub fn is_exhausted(&self) -> bool {
        let token_stream = self.token_provider.token_stream();
        for i in self.current_index..self.token_provider.token_stream().len() {
            if !matches!(
                token_stream.get(i),
                Some(TokenTree::Token(
                    Token::WhiteSpaces(..) | Token::Comment(..)
                ))
            ) {
                return false;
            }
        }
        true
    }

    /// Checks if the current [`Frame`] has reached the end of the [`TokenStream`].
    #[must_use]
    pub fn is_end(&self) -> bool { self.current_index >= self.token_provider.token_stream().len() }

    fn get_reading(&self, token: Option<&TokenTree>) -> Reading {
        token.map_or_else(
            || match self.token_provider {
                // end of file
                TokenProvider::TokenStream(..) => Reading::Eof,
                TokenProvider::Delimited(delimited) => {
                    Reading::DelimitedEnd(delimited.delimiter, delimited.close.clone())
                }
            },
            |token| match token {
                TokenTree::Token(token) => Reading::Unit(token.clone()),
                TokenTree::Delimited(delimited) => {
                    Reading::IntoDelimited(delimited.delimiter, delimited.open.clone())
                }
            },
        )
    }

    /// Returns a [`Token`] pointing by the `current_index` of the [`Frame`].
    #[must_use]
    pub fn peek(&self) -> Reading {
        self.get_reading(self.token_provider.token_stream().get(self.current_index))
    }

    /// Returns a [`Token`] pointing by the `current_index` with the given index offset of the
    /// [`Frame`].
    ///
    /// # Returns
    ///
    /// `None` if `offset + current_index` is less than zero or greter than
    /// `self.token_provider.token_stream().len() + 1`
    #[must_use]
    pub fn peek_offset(&self, offset: isize) -> Option<Reading> {
        let index = self.current_index.checked_add(offset.try_into().ok()?)?;

        if index > self.token_provider.token_stream().len() + 1 {
            return None;
        }

        Some(self.get_reading(self.token_provider.token_stream().get(index)))
    }

    /// Returns a [`Token`] pointing by the `current_index` of the [`Frame`] and increments the
    /// `current_index` by 1.
    #[must_use = "use `forward` instead if you don't need the token and just want to increment the \
                  index"]
    pub fn next_token(&mut self) -> Reading {
        let token = self.peek();

        // increment the index
        self.forward();

        token
    }

    /// Forwards the `current_index` by 1 if the [`Frame`] is not exhausted.
    pub fn forward(&mut self) {
        // increment the index
        if !self.is_end() {
            self.current_index += 1;
        }
    }

    /// Skips any insignificant [`Token`]s, returns the next significant [`Token`] found, and
    /// increments the `current_index` afterward.
    pub fn next_significant_token(&mut self) -> Reading {
        let token = self.stop_at_significant();

        // increment the index
        self.forward();

        token
    }

    /// Makes the current [`Frame`] point to the significant [`Token`] if currently not.
    ///
    /// # Returns
    /// The significant [`Token`] if found, otherwise `None`.
    pub fn stop_at_significant(&mut self) -> Reading {
        while !self.is_end() {
            let token = self.peek();

            if !matches!(
                token,
                Reading::Unit(Token::WhiteSpaces(..) | Token::Comment(..))
            ) {
                return token;
            }

            self.forward();
        }

        match self.token_provider {
            TokenProvider::TokenStream(..) => Reading::Eof,
            TokenProvider::Delimited(delimited) => {
                Reading::DelimitedEnd(delimited.delimiter, delimited.close.clone())
            }
        }
    }

    /// Makes the current position stops at the first token that satisfies the predicate.
    pub fn stop_at(&mut self, predicate: impl Fn(&Reading) -> bool) -> Reading {
        while !self.is_end() {
            let token = self.peek();

            if predicate(&token) {
                return token;
            }

            self.current_index += 1;
        }

        match self.token_provider {
            TokenProvider::TokenStream(..) => Reading::Eof,
            TokenProvider::Delimited(delimited) => {
                Reading::DelimitedEnd(delimited.delimiter, delimited.close.clone())
            }
        }
    }

    /// Expects the next [`Token`] to be an [`Identifier`], and returns it.
    ///
    /// # Errors
    /// If the next [`Token`] is not an [`Identifier`].
    pub fn parse_identifier(&mut self, handler: &dyn Handler<Error>) -> Option<Identifier> {
        match self.next_significant_token() {
            Reading::Unit(Token::Identifier(ident)) => Some(ident),
            found => {
                handler.receive(Error {
                    expected: SyntaxKind::Identifier,
                    alternatives: Vec::new(),
                    found: found.into_token(),
                });
                None
            }
        }
    }

    /// Expects the next [`Token`] to be an [`Numeric`], and returns it.
    ///
    /// # Errors
    /// If the next [`Token`] is not an [`Identifier`].
    pub fn parse_numeric(&mut self, handler: &dyn Handler<Error>) -> Option<Numeric> {
        match self.next_significant_token() {
            Reading::Unit(Token::Numeric(ident)) => Some(ident),
            found => {
                handler.receive(Error {
                    expected: SyntaxKind::Numeric,
                    alternatives: Vec::new(),
                    found: found.into_token(),
                });
                None
            }
        }
    }

    /// Expects the next [`Token`] to be a [`Keyword`] of specific kind, and returns it.
    ///
    /// # Errors
    /// If the next [`Token`] is not a [`Keyword`] of specific kind.
    pub fn parse_keyword(
        &mut self,
        expected: KeywordKind,
        handler: &dyn Handler<Error>,
    ) -> Option<Keyword> {
        match self.next_significant_token() {
            Reading::Unit(Token::Keyword(keyword_token)) if keyword_token.kind == expected => {
                Some(keyword_token)
            }
            found => {
                handler.receive(Error {
                    expected: SyntaxKind::Keyword(expected),
                    alternatives: Vec::new(),
                    found: found.into_token(),
                });
                None
            }
        }
    }

    /// Expects the next [`Token`] to be a [`Punctuation`] of specific kind, and returns it.
    ///
    /// # Errors
    /// If the next [`Token`] is not a [`Punctuation`] of specific kind.
    pub fn parse_punctuation(
        &mut self,
        expected: char,
        skip_insignificant: bool,
        handler: &dyn Handler<Error>,
    ) -> Option<Punctuation> {
        match if skip_insignificant {
            self.next_significant_token()
        } else {
            self.next_token()
        } {
            Reading::Unit(Token::Punctuation(punctuation_token))
                if punctuation_token.punctuation == expected =>
            {
                Some(punctuation_token)
            }
            found => {
                handler.receive(Error {
                    expected: SyntaxKind::Punctuation(expected),
                    alternatives: Vec::new(),
                    found: found.into_token(),
                });
                None
            }
        }
    }

    /// Tries to parse the given function, and if it fails, resets the current index to the
    /// `current_index` before the function call.
    pub fn try_parse<T>(&mut self, f: impl FnOnce(&mut Self) -> Option<T>) -> Option<T> {
        let current_index = self.current_index;

        let result = f(self);

        if result.is_none() {
            self.current_index = current_index;
        }

        result
    }
}

/// The parser of the compiler.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Deref, DerefMut)]
pub struct Parser<'a> {
    #[deref]
    #[deref_mut]
    current_frame: Frame<'a>,
    stack: Vec<Frame<'a>>,
}

/// Represents a result of [`Parser::step_into()`] function.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DelimitedTree<T> {
    /// The opening delimiter.
    pub open: Punctuation,

    /// The tree inside the delimiter.
    pub tree: Option<T>,

    /// The closing delimiter.
    pub close: Punctuation,
}

impl<'a> Parser<'a> {
    /// Creates a new parser from the given token stream.
    #[must_use]
    pub fn new(token_stream: &'a TokenStream) -> Self {
        Self {
            current_frame: Frame {
                token_provider: TokenProvider::TokenStream(token_stream),
                current_index: 0,
            },
            stack: Vec::new(),
        }
    }

    /// Steps into the [`Delimited`] token stream and parses the content within the delimiters.
    ///
    /// The parser's position must be at the delimited token stream.
    pub fn step_into<T>(
        &mut self,
        delimiter: Delimiter,
        f: impl FnOnce(&mut Self) -> Option<T>,
        handler: &dyn Handler<Error>,
    ) -> Option<DelimitedTree<T>> {
        self.current_frame.stop_at_significant();
        let raw_token_tree = self
            .current_frame
            .token_provider
            .token_stream()
            .get(self.current_frame.current_index);

        // move after the whole delimited list
        self.current_frame.forward();

        let expected = match delimiter {
            Delimiter::Parenthesis => '(',
            Delimiter::Brace => '{',
            Delimiter::Bracket => '[',
        };

        let delimited_stream = if let Some(token_tree) = raw_token_tree {
            match token_tree {
                TokenTree::Delimited(delimited_tree) if delimited_tree.delimiter == delimiter => {
                    delimited_tree
                }
                found => {
                    handler.receive(Error {
                        expected: SyntaxKind::Punctuation(expected),
                        alternatives: Vec::new(),
                        found: Some(match found {
                            TokenTree::Token(token) => token.clone(),
                            TokenTree::Delimited(delimited_tree) => {
                                Token::Punctuation(delimited_tree.open.clone())
                            }
                        }),
                    });

                    return None;
                }
            }
        } else {
            handler.receive(Error {
                expected: SyntaxKind::Punctuation(expected),
                alternatives: Vec::new(),
                found: self.get_reading(None).into_token(),
            });

            return None;
        };

        // creates a new frame
        let new_frame = Frame {
            token_provider: TokenProvider::Delimited(delimited_stream),
            current_index: 0,
        };

        // pushes the current frame onto the stack and replaces the current frame with the new one
        self.stack
            .push(std::mem::replace(&mut self.current_frame, new_frame));

        let open = delimited_stream.open.clone();

        let tree = f(self);

        // pops the current frame off the stack
        let Some(new_frame) = self.stack.pop() else {
            return None;
        };

        // the current frame must be at the end
        if !self.current_frame.is_exhausted() {
            let expected = match self
                .current_frame
                .token_provider
                .as_delimited()
                .unwrap()
                .delimiter
            {
                Delimiter::Parenthesis => ')',
                Delimiter::Brace => '}',
                Delimiter::Bracket => ']',
            };

            handler.receive(Error {
                expected: SyntaxKind::Punctuation(expected),
                alternatives: Vec::new(),
                found: self.peek().into_token(),
            });
        }

        let close_punctuation = self
            .current_frame
            .token_provider
            .as_delimited()
            .unwrap()
            .close
            .clone();

        // replaces the current frame with the popped one
        self.current_frame = new_frame;

        Some(DelimitedTree {
            open,
            tree,
            close: close_punctuation,
        })
    }

    /// Tries to parse the given function, and if it fails, resets the current index to the
    /// `current_index` before the function call.
    pub fn try_parse<T>(&mut self, f: impl FnOnce(&mut Self) -> Option<T>) -> Option<T> {
        let current_frame_index = self.current_frame.current_index;

        let result = f(self);

        if result.is_none() {
            self.current_frame.current_index = current_frame_index;
        }

        result
    }
}
