//! Contains the definition of [`StateMachine`] and its implementation.

use getset::CopyGetters;
use pernixc_lexical::{
    token::{Kind, Token},
    tree::{self, Branch, Location, Node, Tree},
};
use pernixc_source_file::GlobalSourceID;

/// Represents the state machine used to scan the token stream input and produce
/// a syntax tree.
#[derive(Debug, Clone, PartialEq, Eq, CopyGetters)]
pub struct StateMachine<'a> {
    /// The tree of tokens to scan.
    #[get_copy = "pub"]
    tree: &'a Tree<GlobalSourceID>,

    /// The location that the state machine is currently at
    #[get_copy = "pub"]
    location: Location<GlobalSourceID>,

    /// Determines whether the new line character is considered a significant
    /// token.
    new_line_significant: bool,
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
    /// The current node is not [`Node::Branch`] and cannot be stepped into.
    NotBranch,

    /// The token stream has reached the end of the stream.
    EndOfStream,
}

impl<'a> StateMachine<'a> {
    /// Creates a new token stream state machine.
    #[must_use]
    pub fn new(tree: &'a Tree<GlobalSourceID>) -> Self {
        Self {
            tree,
            location: Location::new(tree.root_id(), 0),
            new_line_significant: false,
        }
    }

    /// Finds the nearest **significant** token in the token stream and returns
    /// it along with its index.
    #[must_use]
    pub fn peek(&self) -> Option<(&'a Node<GlobalSourceID>, usize)> {
        let mut current_tok_index = self.location.node_index;

        loop {
            let token = self.tree.branches()[self.location.branch_id]
                .nodes
                .get(current_tok_index)?;

            match token {
                tree::Node::Leaf(Token { token: Kind::NewLine(_), .. }) => {
                    if self.new_line_significant {
                        return Some((token, current_tok_index));
                    }

                    current_tok_index += 1;
                }

                tree::Node::Branch(_) | tree::Node::Leaf(_) => {
                    return Some((token, current_tok_index));
                }
            }
        }
    }

    /// Gets the current token in the token stream. If there are no more tokens,
    /// the function returns `None`.
    #[must_use]
    pub fn peek_no_skip(&self) -> Option<&'a Node<GlobalSourceID>> {
        self.tree.branches()[self.location.branch_id]
            .nodes
            .get(self.location.node_index)
    }

    /// Consumes the current token and moves to the next token.
    pub fn next_no_skip(&mut self) -> Option<&'a Node<GlobalSourceID>> {
        let token = self.peek_no_skip()?;
        self.location.node_index += 1;

        Some(token)
    }

    /// Consumes the nearest **significant** token in the token stream and moves
    /// to the next token.
    ///
    /// # Returns
    ///
    /// Returns `Some` with the token and its index if successful. Otherwise,
    /// returns `None`.
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Option<(&'a Node<GlobalSourceID>, usize)> {
        let (token, node_index) = self.peek()?;

        self.location.node_index = node_index + 1;

        Some((token, node_index))
    }

    /// Steps into the nearest **significant** [`TokenKind::Fragment`] token
    /// in the token stream and invokes the given function.
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
        f: impl FnOnce(&mut Self, Location<GlobalSourceID>) -> O,
    ) -> Result<O, StepIntoError> {
        let (token, node_index) =
            self.peek().ok_or(StepIntoError::EndOfStream)?;

        let result = if let Node::Branch(branch_id) = token {
            let mut step_state_machine = Self {
                tree: self.tree,
                location: Location::new(*branch_id, 0),
                new_line_significant: false,
            };

            let result = f(
                &mut step_state_machine,
                Location::new(self.location.branch_id, node_index),
            );

            Ok(result)
        } else {
            self.location.node_index = node_index + 1;
            return Err(StepIntoError::NotBranch);
        };

        self.location.node_index = node_index + 1;

        result
    }

    /// Returns the current branch ID of the state machine.
    #[must_use]
    pub const fn current_branch_id(&self) -> usize { self.location.node_index }

    /// Returns the current token index of the state machine.
    #[must_use]
    pub const fn current_node_index(&self) -> usize { self.location.node_index }

    /// Gets the current [`Node`] that the state machine is at.
    #[must_use]
    pub fn current_branch(&self) -> &'a Branch<GlobalSourceID> {
        &self.tree.branches()[self.location.branch_id]
    }
}
