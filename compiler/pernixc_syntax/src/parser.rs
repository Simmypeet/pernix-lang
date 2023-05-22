//! Contaisn the definition of the [`Parser`].

use derive_more::{Deref, DerefMut};
use pernixc_lexical::{
    token::{Identifier, Keyword, KeywordKind, Punctuation, Token},
    token_stream::{Delimiter, TokenStream, TokenTree},
};
use pernixc_system::diagnostic::Handler;
use thiserror::Error;

use crate::error::{
    Error as SyntacticError, IdentifierExpected, KeywordExpected, PunctuationExpected,
};

/// Describes the delimiter of the [`Frame`].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DelimiterInfo {
    /// The delimiter of the [`Frame`].
    pub delimiter: Delimiter,

    /// The opening [`Token`] of the [`Frame`].
    pub open: Token,

    /// The closing [`Token`] of the [`Frame`].
    pub close: Token,
}

/// Represents a subset of parsing logic that handles on a specific range of [`TokenTree`]s.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Frame<'a> {
    delimiter_info: Option<DelimiterInfo>,
    token_stream: &'a TokenStream,
    current_index: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[error(
    "Is an error that occurs when encountering a fatal syntax error that stops the creation of \
     the syntax tree completely."
)]
#[allow(missing_docs)]
pub struct Error;

/// Is a specialized [`Result`] type for the parser.
pub type Result<T> = std::result::Result<T, Error>;

impl<'a> Frame<'a> {
    /// Returns a [`Token`] pointing by the `current_index` of the [`Frame`].
    #[must_use]
    pub fn peek(&self) -> Option<&'a TokenTree> { self.token_stream.get(self.current_index) }

    /// Returns a [`Token`] pointing by the `current_index` with the given index offset of the
    /// [`Frame`].
    #[must_use]
    pub fn peek_offset(&self, offset: isize) -> Option<&'a TokenTree> {
        let index = self.current_index.checked_add(offset.try_into().ok()?)?;
        self.token_stream.get(index)
    }

    /// Returns a [`Token`] pointing by the `current_index` of the [`Frame`] and increments the
    /// `current_index` by 1.
    #[must_use]
    pub fn next_token(&mut self) -> Option<&'a TokenTree> {
        let token = self.peek();
        if token.is_some() {
            self.current_index += 1;
        }
        token
    }

    /// Skips any insignificant [`Token`]s, returns the next significant [`Token`] found, and
    /// increments the `current_index` afterward.
    pub fn next_significant_token(&mut self) -> Option<&'a TokenTree> {
        let token = self.stop_at_significant();
        if token.is_some() {
            self.current_index += 1;
        }
        token
    }

    /// Makes the current [`Frame`] point to the significant [`TokenTree`] if currently not.
    ///
    /// # Returns
    /// The significant [`TokenTree`] if found, otherwise `None`.
    pub fn stop_at_significant(&mut self) -> Option<&'a TokenTree> {
        loop {
            match self.peek() {
                Some(TokenTree::Token(Token::WhiteSpace(_) | Token::Comment(_))) => {
                    self.current_index += 1;
                }
                found => return found,
            }
        }
    }

    /// Expects the next [`Token`] to be an [`Identifier`], and returns it.
    ///
    /// # Errors
    /// If the next [`Token`] is not an [`Identifier`].
    pub fn parse_identifier(
        &mut self,
        handler: &impl Handler<SyntacticError>,
    ) -> Result<Identifier> {
        match self.next_significant_token() {
            Some(TokenTree::Token(Token::Identifier(ident))) => Ok(ident.clone()),
            found => {
                handler.recieve(SyntacticError::IdentifierExpected(IdentifierExpected {
                    found: self.get_found_token_from_tree(found),
                }));
                Err(Error)
            }
        }
    }

    /// Expects the next [`Token`] to be a [`Keyword`] of specific kind, and returns it.
    ///
    /// # Errors
    /// If the next [`Token`] is not a [`Keyword`] of specific kind.
    pub fn parse_keyword(
        &mut self,
        keyword: KeywordKind,
        handler: &impl Handler<SyntacticError>,
    ) -> Result<Keyword> {
        match self.next_significant_token() {
            Some(TokenTree::Token(Token::Keyword(keyword_token)))
                if keyword_token.keyword == keyword =>
            {
                Ok(keyword_token.clone())
            }
            found => {
                handler.recieve(SyntacticError::KeywordExpected(KeywordExpected {
                    found: self.get_found_token_from_tree(found),
                    expected: keyword,
                }));
                Err(Error)
            }
        }
    }

    /// Expects the next [`Token`] to be a [`Punctuation`] of specific kind, and returns it.
    ///
    /// # Errors
    /// If the next [`Token`] is not a [`Punctuation`] of specific kind.
    pub fn parse_punctuation(
        &mut self,
        punctuation: char,
        skip_insignificant: bool,
        handler: &impl Handler<SyntacticError>,
    ) -> Result<Punctuation> {
        match if skip_insignificant {
            self.next_significant_token()
        } else {
            self.next_token()
        } {
            Some(TokenTree::Token(Token::Punctuation(punctuation_token)))
                if punctuation_token.punctuation == punctuation =>
            {
                Ok(punctuation_token.clone())
            }
            found => {
                handler.recieve(SyntacticError::PunctuationExpected(PunctuationExpected {
                    found: self.get_found_token_from_tree(found),
                    expected: punctuation,
                }));
                Err(Error)
            }
        }
    }

    pub fn try_parse<T>(&mut self, parser: impl FnOnce(&mut Self) -> Result<T>) -> Result<T> {
        let starting_index = self.current_index;
        match parser(self) {
            Ok(value) => Ok(value),
            Err(Error) => {
                self.current_index = starting_index;
                Err(Error)
            }
        }
    }

    fn get_found_token_from_tree(&self, tree: Option<&TokenTree>) -> Option<Token> {
        match tree {
            Some(TokenTree::Token(token)) => Some(token.clone()),
            Some(TokenTree::Delimited(delimited)) => Some(delimited.open.clone()),
            None => self.delimiter_info.as_ref().map(|x| x.close.clone()),
        }
    }
}

/// The parser of the compiler.
#[derive(Debug, Deref, DerefMut)]
pub struct Parser<'a> {
    #[deref]
    #[deref_mut]
    current_frame: Frame<'a>,
    stack: Vec<Frame<'a>>,
}

impl<'a> Parser<'a> {
    /// Creates a new parser from the given token stream.
    #[must_use]
    pub fn new(token_stream: &'a TokenStream) -> Self {
        Self {
            current_frame: Frame {
                delimiter_info: None,
                token_stream,
                current_index: 0,
            },
            stack: Vec::new(),
        }
    }

    /// If the current token is [`Delimited`], steps into the delimited token stream and pushes the
    /// current frame onto the stack.
    ///
    /// # Returns
    /// `true` if the current token is [`Delimited`], `false` otherwise.
    pub fn step_into(&mut self) -> bool {
        let Some(TokenTree::Delimited(delimited)) = self.current_frame.peek() else {
            return false;
        };

        // creates a new frame
        let new_frame = Frame {
            delimiter_info: Some(DelimiterInfo {
                delimiter: delimited.delimiter,
                open: delimited.open.clone(),
                close: delimited.close.clone(),
            }),
            token_stream: &delimited.token_stream,
            current_index: 0,
        };

        // pushes the current frame onto the stack and replaces the current frame with the new one
        self.stack
            .push(std::mem::replace(&mut self.current_frame, new_frame));

        true
    }

    /// Steps out from the current frame, replacing it with the frame on top of the stack.
    pub fn step_out(&mut self) -> bool {
        // pops the current frame off the stack
        let Some(new_frame) = self.stack.pop() else {
            return false;
        };

        // replaces the current frame with the popped one
        self.current_frame = new_frame;

        true
    }
}
