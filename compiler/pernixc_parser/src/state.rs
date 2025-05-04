//! Contains the definition of the [`State`].

use enum_as_inner::EnumAsInner;
use getset::CopyGetters;
use pernixc_arena::ID;
use pernixc_lexical::{kind::Kind, token::Token, tree::DelimiterKind};

use crate::{cache::Cache, concrete_tree::AstInfo, expect::Expected};

/// Represents the state machine of the parser. The parser will use this state
/// machine to scan the token tree and produce a syntax tree.
#[derive(Debug, CopyGetters)]
pub struct State<'a, 'cache> {
    /// The token tree that will be used to scan the source code.
    #[get_copy = "pub"]
    tree: &'a pernixc_lexical::tree::Tree,

    /// The branch of the token tree that the parser is currently in.
    #[get_copy = "pub"]
    branch: &'a pernixc_lexical::tree::Branch,

    /// The current cursor position in the token tree.
    #[get_copy = "pub"]
    cursor: Cursor,

    /// List of events that will be used to build the syntax tree at the end of
    /// parsing.
    events: Vec<Event>,

    /// Determines whether the new line character is considered a significant
    /// token.
    new_line_significant: bool,

    /// The current error that the parser encountered.
    ///
    /// # Invariant
    ///
    /// If the [`Error::expecteds`] is empty, it means that the parser doesn't
    /// encounter any error yet.
    current_error: Error,

    /// The memoize table
    cache: &'cache mut Cache,
}

/// Used for representing the position where the parser is in the token tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Cursor {
    /// In which branch of the token tree the parser is currently in.
    pub branch_id: ID<pernixc_lexical::tree::Branch>,

    /// The node index in the branch that the [`Cursor::branch_id`] points to.
    pub node_index: usize,
}

/// An enumeration of what kind of fragment that the state machine can step
/// into.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum FragmentKind {
    Indentation,
    Delimited(DelimiterKind),
}

/// Represents an error of encountering an unexpected token at a certain
/// possition at its possible expected tokens.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Error {
    /// The tokens that are expected at the cursor position.
    pub expecteds: Vec<Expected>,

    /// The cursor position where the error occurred.
    pub at: Cursor,
}

impl<'a, 'cache> State<'a, 'cache> {
    /// Creates a new parser state machine that starts at the root of the
    /// token tree and the first node.
    #[must_use]
    pub(crate) fn new(
        tree: &'a pernixc_lexical::tree::Tree,
        cache: &'cache mut Cache,
    ) -> Self {
        Self {
            events: Vec::with_capacity(
                tree[pernixc_lexical::tree::ROOT_BRANCH_ID].nodes.len(),
            ),
            tree,
            branch: &tree[pernixc_lexical::tree::ROOT_BRANCH_ID],
            cursor: Cursor {
                branch_id: pernixc_lexical::tree::ROOT_BRANCH_ID,
                node_index: 0,
            },
            new_line_significant: true,
            current_error: Error {
                expecteds: Vec::new(),
                at: Cursor {
                    branch_id: pernixc_lexical::tree::ROOT_BRANCH_ID,
                    node_index: 0,
                },
            },
            cache,
        }
    }

    /// Returns the token and its node index at the current cursor position.
    ///
    /// The state machine may skip over new line tokens if it's configured to
    /// do so.
    #[must_use]
    pub fn peek(&self) -> Option<(&'a pernixc_lexical::tree::Node, usize)> {
        let mut current_tok_index = self.cursor.node_index;

        loop {
            let token = self.branch.nodes.get(current_tok_index)?;

            match token {
                pernixc_lexical::tree::Node::Leaf(Token {
                    kind: Kind::NewLine(_),
                    ..
                }) => {
                    if self.new_line_significant {
                        return Some((token, current_tok_index));
                    }

                    current_tok_index += 1;
                }

                pernixc_lexical::tree::Node::Branch(_)
                | pernixc_lexical::tree::Node::Leaf(_) => {
                    return Some((token, current_tok_index));
                }
            }
        }
    }

    /// Sets the node index of the cursor to the given value.
    pub fn set_node_index(&mut self, node_index: usize) {
        assert!(node_index <= self.branch.nodes.len());

        self.cursor.node_index = node_index;
    }

    /// Adds an [`Event::Take`] event that takes the given number of tokens
    pub fn eat_token(&mut self, count: usize) {
        assert!(count > 0);

        // already in the last event, don't waste a new event memory
        if let Some(take) = self.events.last_mut().and_then(|x| x.as_take_mut())
        {
            *take += count;
        } else {
            self.events.push(Event::Take(count));
        }
    }

    /// The current branch id that the cursor is in.
    #[must_use]
    pub const fn branch_id(&self) -> ID<pernixc_lexical::tree::Branch> {
        self.cursor.branch_id
    }

    /// Gets the absolute byte index of the starting location of the token
    /// pointer by the cursor.
    #[must_use]
    pub fn start_location_of_cursor(&self, cursor: Cursor) -> usize {
        let branch = &self.tree[cursor.branch_id];

        assert!(
            cursor.node_index <= branch.nodes.len(),
            "Cursor node index is out of bounds"
        );

        if branch.nodes.len() == cursor.node_index {
            return self.tree.absoluate_end_byte_of(cursor.branch_id);
        }

        match &branch.nodes[cursor.node_index] {
            pernixc_lexical::tree::Node::Leaf(token) => {
                self.tree.absolute_location_of(token.start_location())
            }
            pernixc_lexical::tree::Node::Branch(id) => {
                self.tree.absolute_start_byte_of(*id)
            }
        }
    }

    /// Adds an error to the current error list.
    pub fn add_error(
        &mut self,
        errors: impl IntoIterator<Item = Expected>,
        at: Cursor,
    ) {
        // empty = no error, assign the current error
        if self.current_error.expecteds.is_empty() {
            self.current_error.at = at;
            let current_len = self.current_error.expecteds.len();
            self.current_error.expecteds.extend(errors);

            assert!(
                self.current_error.expecteds.len() > current_len,
                "Expected tokens should not be empty"
            );
            return;
        }

        // compare position between two, the one that is further will override
        // or if equal, merge the errors
        let current_offset =
            self.start_location_of_cursor(self.current_error.at);
        let new_offset = self.start_location_of_cursor(at);

        match current_offset.cmp(&new_offset) {
            std::cmp::Ordering::Less => {
                self.current_error.at = at;

                self.current_error.expecteds.clear();
                self.current_error.expecteds.extend(errors);

                assert!(
                    !self.current_error.expecteds.is_empty(),
                    "Expected tokens should not be empty"
                );
            }
            std::cmp::Ordering::Equal => {
                let current_len = self.current_error.expecteds.len();
                self.current_error.expecteds.extend(errors);

                assert!(
                    self.current_error.expecteds.len() > current_len,
                    "Expected tokens should not be empty"
                );
            }
            std::cmp::Ordering::Greater => {
                // do nothing, the current error is already the latest
                // error
            }
        }
    }

    /// Change the state's `new_line_significant` value to the given value
    /// and run the given operation. After the operation is done, the
    /// `new_line_significant` value will be restored to its original value.
    pub fn set_new_line_significant<T>(
        &mut self,
        new_line_significant: bool,
        op: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let old_new_line_significant = self.new_line_significant;
        self.new_line_significant = new_line_significant;

        let result = op(self);

        self.new_line_significant = old_new_line_significant;

        result
    }
}

/// The result of parsing operations. Instead of parsing the tree structure
/// directly in each parsing operation, the state machine will keep track of
/// the events that will be used to build the syntax tree at the end of
/// parsing operations.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
pub enum Event {
    /// Start a new node with the given [`AstInfo`] and push it onto the
    /// stack. If [`AstInfo::step_into_fragment`] present, the parser shall
    /// step into the fragment as well.
    NewNode(AstInfo),

    /// Take token to the top-most node in the stack.
    Take(usize),

    /// Pop the top-most node in the stack and creates a new node with the
    /// popped [`AstInfo`]. If the [`AstInfo::step_into_fragment`] present, the
    /// parser shall step into the fragment as well and, if there still tokens
    /// left in the stepping out fragment, the leftover tokens will be grouped
    /// into a new error node and is attached to the popped node.
    FinishNode,
}
