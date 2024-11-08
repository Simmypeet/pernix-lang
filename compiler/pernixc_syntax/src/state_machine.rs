//! Contains the definition of [`StateMachine`] and its implementation.

use pernixc_lexical::token_stream::{Location, Node, TokenKind, Tree};

pub mod parse;

/// Represents the state machine used to scan the token stream input and produce
/// a syntax tree.
///
/// By default, all of the progression methods will skip insignificant tokens.
/// To include insignificant tokens, use the `_no_skip` variants of the methods.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StateMachine<'a> {
    /// The tree of tokens to scan.
    tree: &'a Tree<'a>,

    /// The location that the state machine is currently at
    location: Location,

    /// Represents the number of tokens that have been choosen by the parser.
    ///
    /// This is useful when branching and backtracking.
    eaten_tokens: usize,
}

impl<'a> StateMachine<'a> {
    /// Creates a new token stream state machine.
    #[must_use]
    pub fn new(tree: &'a Tree<'a>) -> Self {
        Self { tree, location: Location::new(0, 0), eaten_tokens: 0 }
    }

    /// Gets the tree of tokens that the state machine is scanning.
    pub fn tree(&self) -> &'a Tree<'a> { self.tree }

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

                TokenKind::Delimited(_) => {
                    return Some((token, current_tok_index));
                }

                _ => {
                    current_tok_index += 1;
                }
            }
        }
    }

    /// Gets the current token in the token stream. If there are no more tokens,
    /// the function returns `None`.
    pub fn peek_no_skip(&self) -> Option<&'a TokenKind> {
        self.tree.get_token(&self.location)
    }

    /// If the nearest **significant** token is a [TokenKind::Delimited], steps
    /// into this node and invokes the given function.
    ///
    /// # Returns
    ///
    /// Returns `true` if the current location is pointing to a
    /// [TokenKind::Delimited]. and the function was invoked. Otherwise,
    /// returns `false`.
    pub fn peek_into(
        &self,
        f: impl FnOnce(StateMachine<'a>, Location),
    ) -> bool {
        if let Some((TokenKind::Delimited(_), tok_index)) = self.peek() {
            let delimited_node_index = self
                .tree
                .get_node(self.location.node_index)
                .child_ids_by_token_index[&tok_index];

            f(
                Self {
                    tree: self.tree,
                    location: Location::new(delimited_node_index, 0),
                    eaten_tokens: 0,
                },
                Location::new(self.location.node_index, tok_index),
            );

            true
        } else {
            false
        }
    }

    /// Consumes the current token and moves to the next token.
    ///
    /// This counts as a token that has been choosen by the parser; therefore,
    /// the [Self::eaten_tokens] is incremented.
    pub fn next_no_skip(&mut self) -> Option<&'a TokenKind> {
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
    /// the [Self::eaten_tokens] is incremented.
    ///
    /// # Returns
    ///
    /// Returns `Some` with the token and its index if successful. Otherwise,
    /// returns `None`.
    pub fn next(&mut self) -> Option<(&'a TokenKind, usize)> {
        // counts as eaten no matter what
        self.eaten_tokens += 1;

        let (token, tok_index) = self.peek()?;

        self.location.token_index = tok_index + 1;

        Some((token, tok_index))
    }

    /// Steps into the nearest **significant** [TokenKind::Delimited] token in
    /// the token stream and invokes the given function.
    ///
    /// This counts as a token that has been choosen by the parser; therefore,
    /// the [Self::eaten_tokens] is incremented.
    ///
    /// # Returns
    ///
    /// Returns `Some` with the result of the closure if successful. Otherwise,
    /// returns `None`.
    pub fn next_step_into<O>(
        &mut self,
        f: impl FnOnce(&mut StateMachine<'a>, Location) -> O,
    ) -> Option<O> {
        // counts as eaten no matter what
        self.eaten_tokens += 1;

        let (token, tok_index) = self.peek()?;

        let result = if let TokenKind::Delimited(_) = token {
            let delimited_node_index = self
                .tree
                .get_node(self.location.node_index)
                .child_ids_by_token_index[&tok_index];

            let mut step_state_machine = Self {
                tree: self.tree,
                location: Location::new(delimited_node_index, 0),
                eaten_tokens: 0,
            };

            let result = f(
                &mut step_state_machine,
                Location::new(self.location.node_index, tok_index),
            );

            // accumulates the step state machine's eaten tokens
            self.eaten_tokens += step_state_machine.eaten_tokens;

            Some(result)
        } else {
            None
        };

        self.location.token_index = tok_index + 1;

        result
    }

    /// Returns the current node index of the state machine.
    #[must_use]
    pub fn current_node_index(&self) -> usize { self.location.node_index }

    /// Returns the current token index of the state machine.
    #[must_use]
    pub fn current_token_index(&self) -> usize { self.location.token_index }

    /// Gets the current [TokenStream::Node] that the state machine is at.
    pub fn current_node(&self) -> &'a Node<'a> {
        self.tree.get_node(self.location.node_index)
    }
}
