//! Contains the definition of the [`State`].

use std::sync::Arc;

use enum_as_inner::EnumAsInner;
use getset::CopyGetters;
use pernixc_arena::ID;
use pernixc_lexical::{kind::Kind, token::Token, tree::ROOT_BRANCH_ID};

use crate::{
    abstract_tree::AbstractTree,
    cache::Cache,
    concrete_tree::{self, AstInfo},
    error::Error,
    expect::{self, Expected},
};

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

/// Represents a snapshot of the [`State`] at a certain point in time that can
/// be restored later. This is used to implement the backtracking feature of
/// the parser.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Checkpoint {
    node_index: usize,
    new_line_significant: bool,
    event_index: (usize, Option<usize>),
}

/// Used for representing the position where the parser is in the token tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Cursor {
    /// In which branch of the token tree the parser is currently in.
    pub branch_id: ID<pernixc_lexical::tree::Branch>,

    /// The node index in the branch that the [`Cursor::branch_id`] points to.
    pub node_index: usize,
}

impl Default for Cursor {
    fn default() -> Self {
        Self { branch_id: ROOT_BRANCH_ID, node_index: Default::default() }
    }
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
            new_line_significant: false,
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

    /// Takes a snapshot of the current state and returns a [`Checkpoint`] which
    /// can be used to restore the state later using [`Self::restore`].
    #[must_use]
    pub fn checkpoint(&self) -> Checkpoint {
        Checkpoint {
            node_index: self.cursor.node_index,
            new_line_significant: self.new_line_significant,
            event_index: (
                self.events.len(),
                // if the last event is a take event, need to remember the
                // number of tokens taken
                self.events.last().and_then(|x| x.as_take().copied()),
            ),
        }
    }

    /// Restores the state to the given [`Checkpoint`]. This will restore the
    /// cursor position, the event list, and the error list to the state
    /// when the checkpoint was created.
    pub fn restore(&mut self, checkpoint: Checkpoint) {
        assert!(self.cursor.node_index >= checkpoint.node_index);
        assert!(self.events.len() >= checkpoint.event_index.0);
        assert!(checkpoint.event_index.1.is_none_or(|saved_count| self.events
            [checkpoint.event_index.0 - 1]
            .as_take()
            .is_some_and(|current_count| *current_count >= saved_count)));

        self.cursor.node_index = checkpoint.node_index;
        self.new_line_significant = checkpoint.new_line_significant;
        self.events.truncate(checkpoint.event_index.0);

        if let Some(take_count) = checkpoint.event_index.1 {
            *self
                .events
                .last_mut()
                .expect("should have an event")
                .as_take_mut()
                .expect("should be a take event") = take_count;
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

    /// Adds an [`Event::Take`] event that takes the given number of tokens
    pub fn eat_token(&mut self, count: usize) {
        // do nothing if count is 0
        if count == 0 {
            return;
        }

        // already in the last event, don't waste a new event memory
        if let Some(take) = self.events.last_mut().and_then(|x| x.as_take_mut())
        {
            *take += count;
        } else {
            self.events.push(Event::Take(count));
        }

        // update the cursor position
        self.cursor.node_index += count;
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

    /// Starts a new node with the given [`AstInfo`] and pushes it onto the
    /// stack. If [`AstInfo::step_into_fragment`] is present, the parser shall
    /// step into the fragment as well.
    pub fn start_ndoe<A: AbstractTree, T>(
        &mut self,
        op: impl for<'x> FnOnce(&mut State<'a, 'x>) -> T,
    ) -> Option<T> {
        // step into the fragment
        if let Some(some_step_info) = A::step_into_fragment() {
            let starting_node_index = self.cursor.node_index;
            let Some((node, node_index)) = self.peek() else {
                self.add_error(
                    std::iter::once(some_step_info.into()),
                    Cursor {
                        branch_id: self.branch_id(),
                        node_index: self.branch.nodes.len(),
                    },
                );
                return None;
            };

            // check if branch match the expected kind
            let Some((branch, branch_id)) =
                node.as_branch().copied().into_iter().find_map(|id| {
                    let branch = &self.tree[id];
                    let fragment = branch.kind.as_fragment()?;

                    let expected_kind = match &fragment.fragment_kind {
                        pernixc_lexical::tree::FragmentKind::Delimiter(
                            delimiter,
                        ) => expect::Fragment::Delimited(delimiter.delimiter),
                        pernixc_lexical::tree::FragmentKind::Indentation(_) => {
                            expect::Fragment::Indentation
                        }
                    };

                    (expected_kind == some_step_info).then_some((branch, id))
                })
            else {
                // not a branch
                self.add_error(
                    std::iter::once(some_step_info.into()),
                    Cursor { branch_id: self.branch_id(), node_index },
                );
                return None;
            };

            // eat the token up until the fragment
            self.eat_token(node_index - starting_node_index);

            // start the event
            self.events.push(Event::NewNode(Some(AstInfo {
                ast_type_id: std::any::TypeId::of::<A>(),
                step_into_fragment: Some(branch_id),
            })));

            let mut state = State {
                tree: self.tree,
                branch,
                cursor: Cursor { branch_id, node_index: 0 },
                events: Vec::with_capacity(branch.nodes.len()),
                new_line_significant: true,
                current_error: std::mem::take(&mut self.current_error),
                cache: self.cache,
            };

            // operate on the inner fragment state
            let result = op(&mut state);

            self.events.push(Event::Inline(state.events));
            self.current_error = state.current_error;
            self.cursor.node_index += 1; // step past the fragment

            // done event
            self.events.push(Event::FinishNode);

            Some(result)
        } else {
            // no step into fragment, just start the event
            self.events.push(Event::NewNode(Some(AstInfo {
                ast_type_id: std::any::TypeId::of::<A>(),
                step_into_fragment: None,
            })));

            // operate on the inner state
            let result = op(self);

            // done event
            self.events.push(Event::FinishNode);

            Some(result)
        }
    }

    pub(crate) fn try_take_error(&mut self) -> Option<Error> {
        // no error to take
        if self.current_error.expecteds.is_empty() {
            return None;
        }

        let current_at = self.start_location_of_cursor(self.cursor);
        let error_at = self.start_location_of_cursor(self.current_error.at);

        if current_at <= error_at {
            Some(Error {
                expecteds: std::mem::take(&mut self.current_error.expecteds),
                at: self.current_error.at,
            })
        } else {
            None
        }
    }

    pub(crate) fn finalize<A: AbstractTree>(
        mut self,
    ) -> (Option<A>, Vec<Error>) {
        assert!(!self.events.is_empty(), "No events to finalize");
        assert_eq!(
            self.branch_id(),
            ROOT_BRANCH_ID,
            "can only finalize the root branch"
        );

        let take_error = self.try_take_error();

        let mut events = FlattenEvent::new(self.events);
        let mut cursor = Cursor { branch_id: ROOT_BRANCH_ID, node_index: 0 };

        let first = events
            .next()
            .expect("should have a first event starting the node")
            .into_new_node()
            .expect("should be a new node event")
            .expect("should have ast info matching the `A` type");

        assert_eq!(first.ast_type_id, std::any::TypeId::of::<A>());

        let tree = Self::finalize_ast_node(
            self.tree,
            Some(first),
            &mut events,
            &mut cursor,
        );

        let tree = tree.map(|tree| {
            A::from_node(&concrete_tree::Node::Branch(Arc::new(tree)))
                .expect("should be able to convert the tree to the AST")
        });

        (tree, take_error.into_iter().collect())
    }

    pub(crate) fn finalize_ast_node(
        tree: &pernixc_lexical::tree::Tree,
        ast_info: Option<AstInfo>,
        events: &mut impl Iterator<Item = Event>,
        cursor: &mut Cursor,
    ) -> Option<concrete_tree::Tree> {
        let mut nodes = Vec::new();

        // step into the fragment if needed and memorize the cursor position
        // after finish the fragment
        let finish_cursor = if let Some(step_into_fragment) =
            ast_info.and_then(|x| x.step_into_fragment)
        {
            assert!(
                tree[cursor.branch_id].nodes[cursor.node_index]
                    .as_branch()
                    .is_some_and(|x| *x == step_into_fragment),
                "mismatched step into fragment node"
            );

            let current_branch_id = cursor.branch_id;
            let current_node_index = cursor.node_index;

            cursor.node_index = 0;
            cursor.branch_id = step_into_fragment;

            // step pass the fragment
            Some(Cursor {
                branch_id: current_branch_id,
                node_index: current_node_index + 1,
            })
        } else {
            None
        };

        let branch = &tree[cursor.branch_id];

        while let Some(event) = events.next() {
            match event {
                Event::NewNode(ast_info) => {
                    let Some(tree) =
                        Self::finalize_ast_node(tree, ast_info, events, cursor)
                    else {
                        continue;
                    };

                    nodes.push(concrete_tree::Node::Branch(Arc::new(tree)));
                }

                Event::Take(take) => {
                    // eat the tokens from the branch
                    nodes.extend(
                        branch.nodes[cursor.node_index..]
                            .iter()
                            .take(take)
                            .map(|x| match x {
                                pernixc_lexical::tree::Node::Leaf(token) => {
                                    concrete_tree::Node::Leaf(token.clone())
                                }
                                pernixc_lexical::tree::Node::Branch(id) => {
                                    concrete_tree::Node::SkipFragment(*id)
                                }
                            }),
                    );

                    cursor.node_index += take;
                }
                Event::FinishNode => {
                    if let Some(finish_cursor) = finish_cursor {
                        // pre-maturely stepping out, create error node
                        if cursor.node_index < branch.nodes.len() {
                            let errors = branch.nodes[cursor.node_index..]
                                .iter()
                                .map(|x| match x {
                                    pernixc_lexical::tree::Node::Leaf(
                                        token,
                                    ) => {
                                        concrete_tree::Node::Leaf(token.clone())
                                    }
                                    pernixc_lexical::tree::Node::Branch(id) => {
                                        concrete_tree::Node::SkipFragment(*id)
                                    }
                                })
                                .collect();

                            let error_node = concrete_tree::Node::Branch(
                                Arc::new(concrete_tree::Tree {
                                    ast_info: None,
                                    nodes: errors,
                                }),
                            );

                            nodes.push(error_node);
                        }

                        *cursor = finish_cursor;
                    }

                    // done with the current node, pop the stack
                    // if node has no tokens, return None
                    return (finish_cursor.is_some() || !nodes.is_empty())
                        .then_some(concrete_tree::Tree { ast_info, nodes });
                }

                Event::Inline(_) => {
                    unreachable!("should've been flattend")
                }
            }
        }

        panic!("missing finish node event")
    }
}

/// The result of parsing operations. Instead of parsing the tree structure
/// directly in each parsing operation, the state machine will keep track of
/// the events that will be used to build the syntax tree at the end of
/// parsing operations.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Event {
    /// Start a new node with the given [`AstInfo`] and push it onto the
    /// stack. If [`AstInfo::step_into_fragment`] present, the parser shall
    /// step into the fragment as well.
    NewNode(Option<AstInfo>),

    /// Take token to the top-most node in the stack.
    Take(usize),

    /// Pop the top-most node in the stack and creates a new node with the
    /// popped [`AstInfo`]. If the [`AstInfo::step_into_fragment`] present, the
    /// parser shall step into the fragment as well and, if there still tokens
    /// left in the stepping out fragment, the leftover tokens will be grouped
    /// into a new error node and is attached to the popped node.
    FinishNode,

    /// Just inlines the list of events into the current event list.
    Inline(Vec<Event>),
}

struct FlattenEvent {
    poll: Vec<Event>,
}

impl FlattenEvent {
    fn new(mut events: Vec<Event>) -> Self {
        events.reverse();
        Self { poll: events }
    }
}

impl Iterator for FlattenEvent {
    type Item = Event;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.poll.pop()? {
                Event::Inline(mut events) => {
                    events.reverse();

                    self.poll.append(&mut events);
                }

                event => {
                    // if the event is not an inline event, return it
                    return Some(event);
                }
            }
        }
    }
}
