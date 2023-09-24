//! Contaisn the definition of the [`Parser`].

use derive_more::{Deref, DerefMut, From};
use enum_as_inner::EnumAsInner;
use pernixc_lexical::{
    token::{Identifier, Keyword, KeywordKind, Punctuation, Token},
    token_stream::{Delimited, Delimiter, TokenStream, TokenTree},
};
use pernixc_system::diagnostic::Handler;

use crate::error::{Error as SyntacticError, SyntaxKind, UnexpectedSyntax};

/// Represents a source where the parser can get tokens from.
#[derive(Debug, Clone, Copy, EnumAsInner, From)]
pub enum TokenProvider<'a> {
    /// Top-level [`TokenStream`], not enclosed in any delimiters.
    TokenStream(&'a TokenStream),

    /// Delimited [`TokenStream`], enclosed in a pair of delimiters.
    Delimited(&'a Delimited),
}

impl<'a> TokenProvider<'a> {
    /// Gets the [`TokenStream`] from the [`TokenProvider`].
    #[must_use]
    pub fn token_stream(&self) -> &'a TokenStream {
        match self {
            TokenProvider::TokenStream(stream) => stream,
            TokenProvider::Delimited(delimited) => &delimited.token_stream,
        }
    }
}

/// Represents a subset of parsing logic that handles on a specific range of [`TokenTree`]s.
#[derive(Debug, Clone)]
pub struct Frame<'a> {
    /// The [`TokenProvider`] that provides the [`TokenStream`] to parse.
    pub token_provider: TokenProvider<'a>,
    current_index: usize,
}

impl<'a> Frame<'a> {
    pub(crate) fn get_actual_found_token(&self, found: Option<Token>) -> Option<Token> {
        match found {
            None => match self.token_provider {
                TokenProvider::TokenStream(..) => None,
                TokenProvider::Delimited(delimited) => {
                    Some(Token::Punctuation(delimited.close.clone()))
                }
            },
            found => found,
        }
    }

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

    fn to_token_output(token: &'a TokenTree) -> Token {
        match token {
            TokenTree::Token(token) => token.clone(),
            TokenTree::Delimited(token) => Token::Punctuation(token.open.clone()),
        }
    }

    fn last_token_output(&self) -> Option<Token> {
        match self.token_provider {
            TokenProvider::TokenStream(..) => None,
            TokenProvider::Delimited(delimited) => {
                Some(Token::Punctuation(delimited.close.clone()))
            }
        }
    }

    /// Returns a [`Token`] pointing by the `current_index` of the [`Frame`].
    #[must_use]
    pub fn peek(&self) -> Option<Token> {
        self.token_provider
            .token_stream()
            .get(self.current_index)
            .map_or_else(
                || self.last_token_output(),
                |token| Some(Self::to_token_output(token)),
            )
    }

    /// Returns a [`Token`] pointing by the `current_index` with the given index offset of the
    /// [`Frame`].
    #[must_use]
    pub fn peek_offset(&self, offset: isize) -> Option<Token> {
        let index = self.current_index.checked_add(offset.try_into().ok()?)?;

        self.token_provider.token_stream().get(index).map_or_else(
            || {
                if index == self.current_index {
                    self.last_token_output()
                } else {
                    None
                }
            },
            |token| Some(Self::to_token_output(token)),
        )
    }

    /// Returns a [`Token`] pointing by the `current_index` of the [`Frame`] and increments the
    /// `current_index` by 1.
    pub fn next_token(&mut self) -> Option<Token> {
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
    pub fn next_significant_token(&mut self) -> Option<Token> {
        let token = self.stop_at_significant();

        // increment the index
        self.forward();

        token
    }

    /// Makes the current [`Frame`] point to the significant [`Token`] if currently not.
    ///
    /// # Returns
    /// The significant [`Token`] if found, otherwise `None`.
    pub fn stop_at_significant(&mut self) -> Option<Token> {
        while !self.is_end() {
            let token = self.peek();

            if !matches!(token, Some(Token::WhiteSpaces(..) | Token::Comment(..))) {
                return token;
            }

            self.forward();
        }

        None
    }

    /// Makes the current position stops at the first token that satisfies the predicate.
    pub fn stop_at(&mut self, predicate: impl Fn(&Token) -> bool) -> Option<Token> {
        while !self.is_end() {
            let token = self.peek()?;

            if predicate(&token) {
                return Some(token);
            }

            self.current_index += 1;
        }

        None
    }
}

/// The parser of the compiler.
#[derive(Debug, Clone, Deref, DerefMut)]
pub struct Parser<'a> {
    #[deref]
    #[deref_mut]
    current_frame: Frame<'a>,
    stack: Vec<Frame<'a>>,
    trying_stack: Vec<usize>,
}

#[derive(Debug, Clone)]
pub struct DelimitedTree<T> {
    pub open: Punctuation,
    pub tree: Option<T>,
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
            trying_stack: Vec::new(),
        }
    }

    /// Steps into the [`Delimited`] token stream and parses the content within the delimiters.
    ///
    /// The parser's position must be at the delimited token stream.
    pub fn step_into<T>(
        &mut self,
        delimiter: Delimiter,
        f: impl FnOnce(&mut Self) -> Option<T>,
        handler: &impl Handler<SyntacticError>,
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
                    handler.receive(SyntacticError::UnexpectedSyntax(UnexpectedSyntax {
                        expected: SyntaxKind::Punctuation(expected),
                        found: Some(match found {
                            TokenTree::Token(token) => token.clone(),
                            TokenTree::Delimited(delimited_tree) => {
                                Token::Punctuation(delimited_tree.open.clone())
                            }
                        }),
                    }));

                    return None;
                }
            }
        } else {
            handler.receive(SyntacticError::UnexpectedSyntax(UnexpectedSyntax {
                expected: SyntaxKind::Punctuation(expected),
                found: self.get_actual_found_token(None),
            }));

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

        if let Some(threshold) = self.trying_stack.last().copied() {
            assert!(self.stack.len() > threshold);
        }

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

            handler.receive(SyntacticError::UnexpectedSyntax(UnexpectedSyntax {
                expected: SyntaxKind::Punctuation(expected),
                found: self.get_actual_found_token(self.peek()),
            }));
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

    /// Performs a rollback if the parsing fails.
    #[allow(clippy::missing_errors_doc)]
    pub fn try_parse<T>(&mut self, parser: impl FnOnce(&mut Self) -> Option<T>) -> Option<T> {
        let current_frame_copy = self.current_frame.clone();
        self.trying_stack.push(self.stack.len());

        if let Some(value) = parser(self) {
            self.trying_stack.pop();
            Some(value)
        } else {
            let stack_len = self.trying_stack.pop().unwrap();

            // the stack must be the same as before the call
            self.stack.truncate(stack_len);

            // restore the current Frame
            self.current_frame = current_frame_copy;

            None
        }
    }

    /// Expects the next [`Token`] to be an [`Identifier`], and returns it.
    ///
    /// # Errors
    /// If the next [`Token`] is not an [`Identifier`].
    pub fn parse_identifier(
        &mut self,
        handler: &impl Handler<SyntacticError>,
    ) -> Option<Identifier> {
        match self.next_significant_token() {
            Some(Token::Identifier(ident)) => Some(ident),
            found => {
                handler.receive(SyntacticError::UnexpectedSyntax(UnexpectedSyntax {
                    expected: SyntaxKind::Identifier,
                    found: self.get_actual_found_token(found),
                }));
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
        handler: &impl Handler<SyntacticError>,
    ) -> Option<Keyword> {
        match self.next_significant_token() {
            Some(Token::Keyword(keyword_token)) if keyword_token.keyword == expected => {
                Some(keyword_token)
            }
            found => {
                handler.receive(SyntacticError::UnexpectedSyntax(UnexpectedSyntax {
                    expected: SyntaxKind::Keyword(expected),
                    found: self.get_actual_found_token(found),
                }));
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
        handler: &impl Handler<SyntacticError>,
    ) -> Option<Punctuation> {
        match if skip_insignificant {
            self.next_significant_token()
        } else {
            self.next_token()
        } {
            Some(Token::Punctuation(punctuation_token))
                if punctuation_token.punctuation == expected =>
            {
                Some(punctuation_token)
            }
            found => {
                handler.receive(SyntacticError::UnexpectedSyntax(UnexpectedSyntax {
                    expected: SyntaxKind::Punctuation(expected),
                    found: self.get_actual_found_token(found),
                }));
                None
            }
        }
    }
}
