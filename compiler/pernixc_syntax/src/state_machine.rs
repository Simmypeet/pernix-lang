//! Contains the definition of [`StateMachine`] and its implementation.

use getset::CopyGetters;
use pernixc_lexical::token_stream::{Location, Node, TokenKind, Tree};

use crate::expect::Expected;

pub mod parse;

/// Represents the state machine used to scan the token stream input and produce
/// a syntax tree.
///
/// By default, all of the progression methods will skip insignificant tokens.
/// To include insignificant tokens, use the `_no_skip` variants of the methods.
#[derive(Debug, Clone, PartialEq, Eq, CopyGetters)]
pub struct StateMachine<'a> {
    /// The tree of tokens to scan.
    #[get_copy = "pub"]
    tree: &'a Tree<'a>,

    /// The location that the state machine is currently at
    #[get_copy = "pub"]
    location: Location,

    /// Represents the number of tokens that have been choosen by the parser.
    ///
    /// This is useful when branching and backtracking.
    #[get_copy = "pub"]
    eaten_tokens: usize,

    correct_expected_len: usize,
    expected: Vec<Expected>,
}

/// An error that occurs when trying to step into a delimited token stream.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    thiserror::Error,
    displaydoc::Display,
)]
pub enum StepIntoError {
    /// Thetoken is not [`Delimited`] and cannot be stepped into.
    NotDelimited,

    /// The token stream has reached the end of the stream.
    EndOfStream,
}

impl<'a> StateMachine<'a> {
    /// Creates a new token stream state machine.
    #[must_use]
    pub const fn new(tree: &'a Tree<'a>) -> Self {
        Self {
            tree,
            location: Location::new(0, 0),
            eaten_tokens: 0,
            expected: Vec::new(),
            correct_expected_len: 0,
        }
    }

    /// Finds the nearest **significant** token in the token stream and returns
    /// it along with its index.
    #[must_use]
    pub fn peek(&self) -> Option<(&'a TokenKind, usize)> {
        let mut current_tok_index = self.location.token_index;

        loop {
            let token = self.tree.get_token(&Location::new(
                self.location.node_index,
                current_tok_index,
            ))?;

            match token {
                TokenKind::Token(token_unit) if token_unit.is_significant() => {
                    return Some((token, current_tok_index));
                }

                TokenKind::Fragment(_) => {
                    return Some((token, current_tok_index));
                }

                TokenKind::Token(_) => {
                    current_tok_index += 1;
                }
            }
        }
    }

    /// Gets the current token in the token stream. If there are no more tokens,
    /// the function returns `None`.
    #[must_use]
    pub fn peek_no_skip(&self) -> Option<&'a TokenKind> {
        self.tree.get_token(&self.location)
    }

    /// Consumes the current token and moves to the next token.
    ///
    /// This counts as a token that has been choosen by the parser; therefore,
    /// the [`Self::eaten_tokens`] is incremented.
    pub fn next_no_skip(&mut self) -> Option<&'a TokenKind> {
        assert_eq!(
            self.expected.len(),
            self.correct_expected_len,
            "invariant violated: make sure that you haven't discarded the \
             error; expected: {:#?}",
            self.expected
        );

        // counts as eaten no matter what
        self.eaten_tokens += 1;

        let token = self.peek_no_skip()?;
        self.location.token_index += 1;

        Some(token)
    }

    /// Consumes the nearest **significant** token in the token stream and moves
    /// to the next token.
    ///
    /// This counts as a token that has been choosen by the parser; therefore,
    /// the [`Self::eaten_tokens`] is incremented.
    ///
    /// # Returns
    ///
    /// Returns `Some` with the token and its index if successful. Otherwise,
    /// returns `None`.
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Option<(&'a TokenKind, usize)> {
        assert_eq!(
            self.expected.len(),
            self.correct_expected_len,
            "invariant violated: make sure that you haven't discarded the \
             error; expected: {:#?}",
            self.expected
        );

        // counts as eaten no matter what
        self.eaten_tokens += 1;

        let (token, tok_index) = self.peek()?;

        self.location.token_index = tok_index + 1;

        Some((token, tok_index))
    }

    /// Steps into the nearest **significant** [`TokenKind::Delimited`] token
    /// in the token stream and invokes the given function.
    ///
    /// This counts as a token that has been choosen by the parser; therefore,
    /// the [`Self::eaten_tokens`] is incremented.
    ///
    /// # Returns
    ///
    /// Returns `Some` with the result of the closure if successful. Otherwise,
    /// returns `None`.
    ///
    /// # Errors
    ///
    /// See [`StepIntoError`] for more information.
    pub fn next_step_into<O>(
        &mut self,
        f: impl FnOnce(&mut Self, Location) -> O,
    ) -> Result<O, StepIntoError> {
        assert_eq!(
            self.expected.len(),
            self.correct_expected_len,
            "invariant violated: make sure that you haven't discarded the \
             error; expected: {:#?}",
            self.expected
        );

        // counts as eaten no matter what
        self.eaten_tokens += 1;

        let (token, tok_index) =
            self.peek().ok_or(StepIntoError::EndOfStream)?;

        let result = if let TokenKind::Fragment(_) = token {
            let delimited_node_index = self
                .tree
                .get_node(self.location.node_index)
                .unwrap()
                .child_ids_by_token_index[&tok_index];

            let mut step_state_machine = Self {
                tree: self.tree,
                location: Location::new(delimited_node_index, 0),
                eaten_tokens: 0,
                correct_expected_len: self.correct_expected_len,
                expected: self.take_expected(),
            };

            let result = f(
                &mut step_state_machine,
                Location::new(self.location.node_index, tok_index),
            );

            self.expected = step_state_machine.expected;
            self.correct_expected_len = step_state_machine.correct_expected_len;

            // accumulates the step state machine's eaten tokens
            self.eaten_tokens += step_state_machine.eaten_tokens;

            Ok(result)
        } else {
            self.location.token_index = tok_index + 1;
            return Err(StepIntoError::NotDelimited);
        };

        self.location.token_index = tok_index + 1;

        result
    }

    /// Returns the current node index of the state machine.
    #[must_use]
    pub const fn current_node_index(&self) -> usize { self.location.node_index }

    /// Returns the current token index of the state machine.
    #[must_use]
    pub const fn current_token_index(&self) -> usize {
        self.location.token_index
    }

    /// Gets the current [`Node`] that the state machine is at.
    #[must_use]
    pub fn current_node(&self) -> &'a Node<'a> {
        self.tree.get_node(self.location.node_index).unwrap()
    }

    /// Gets the length of expected tokens.
    #[must_use]
    pub fn expected_len(&self) -> usize { self.expected.len() }

    /// Takes the expected vector and returns it.
    pub fn take_expected(&mut self) -> Vec<Expected> {
        self.correct_expected_len = 0;
        std::mem::take(&mut self.expected)
    }
}
