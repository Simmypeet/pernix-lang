//! Contaisn the definition of the [`Parser`].

use derive_more::{Deref, DerefMut};
use pernixc_lexical::{
    token::Token,
    token_stream::{Delimiter, TokenStream, TokenTree},
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

impl<'a> Frame<'a> {
    /// Returns a [`Token`] pointing by the `current_index` of the [`Frame`].
    #[must_use]
    pub fn peek(&self) -> Option<&'a TokenTree> { self.token_stream.get(self.current_index) }

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
