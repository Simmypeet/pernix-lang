//! Responsible for structuring the stream of tokens into a tree-like structure.

use std::{
    alloc::handle_alloc_error,
    cell::RefMut,
    hash::{Hash, Hasher},
    iter::Peekable,
    ops::Range,
};

use derive_more::Deref;
use enum_as_inner::EnumAsInner;
use fnv::FnvHasher;
use pernixc_arena::{Arena, ID};
use pernixc_handler::Handler;
use pernixc_source_file::{AbsoluteSpan, ByteIndex, Span};
use serde::Serialize;
use strum_macros::EnumIter;

use crate::{
    error::{
        self, AvailableIndentation, ExpectIndentation, InvalidIndentation,
        InvalidNewIndentationLevel, MismatchedClosingDelimiter,
        UnexpectedClosingDelimiter,
    },
    kind,
    token::{Kind, NewLine, Punctuation, Token, Tokenizer},
};

/// Representing kinds of delimiter character pair that are used to enclose
/// a region of code.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumIter,
    Serialize,
)]
pub enum DelimiterKind {
    /// A parenthesis: `(`.
    Parenthesis,
    /// A brace: `{`.
    Brace,
    /// A bracket: `[`.
    Bracket,
}

/// The represents how the token nodes are structured in a fragment.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, EnumAsInner,
)]
pub enum FragmentKind {
    /// The token nodes are enclosed by a pair of delimiters.
    Delimiter(Delimiter),

    /// The token nodes are grouped by having the same indentation level.
    Indentation(Indentation),
}

/// Represents a delimiter pair such as `( ... )`, `{ ... }`, or `[ ... ]`
/// delimiters.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct Delimiter {
    /// The opening delimiter.
    pub open: Punctuation<RelativeSpan>,

    /// The closing delimiter.
    pub close: Punctuation<RelativeSpan>,

    /// The type of delimiter.
    pub delimiter: DelimiterKind,
}

/// The token stream is grouped by having the same indentation level. The
/// indentation group is started by a colon followed by a newline character.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct Indentation {
    /// The indentation level of the group (in space or tab characters count,
    /// not how many levels deep).
    pub indentation_size: usize,

    /// The colon character signifying the start of the indentation group.
    pub colon: Punctuation<RelativeSpan>,

    /// The new line character that follows the colon.
    ///
    /// The language expect that if a colon is immediately followed by a
    /// newline, then a new indentation level is started.
    pub new_line: NewLine<RelativeSpan>,
}

/// Specifiying the position in the branch (begin or end) that will be used for
/// calculating the relative offset of the token.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize,
)]
pub enum OffsetMode {
    /// Relative to the start byte of the branch.
    Start,

    /// Relative to the end byte of the branch.
    End,
}

/// A relative location in the source code that is relative to a branch in the
/// token tree.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize,
)]
pub struct RelativeLocation {
    /// The byte offset from the branch.
    pub offset: ByteIndex,

    /// Signifies where to start the offset from the branch (start or end).
    pub mode: OffsetMode,

    /// The branch ID that this relative location is relative to.
    pub relative_to: ID<Branch>,
}

/// A type alias for the [`Span`] that uses [`RelativeLocation`] as its source
/// location.
pub type RelativeSpan = Span<RelativeLocation>;

/// A tree node used for representing a particular token in the source code.
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
    EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Node {
    Leaf(Kind<RelativeSpan>),
    Branch(ID<Branch>),
}

/// Represents a branch that has a [`FragmentKind`] as its kind.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct FragmentBranch {
    /// The kind of fragment this branch represents.
    pub fragment_kind: FragmentKind,

    /// The ID to the parent branch of this fragment.
    pub parent: ID<Branch>,
}

/// Represents the kind of branch in the token tree.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[allow(missing_docs, clippy::large_enum_variant)]
pub enum BranchKind {
    Fragment(FragmentBranch),

    /// The root branch of the token tree.
    Root,
}

/// Represents a branch node in the token tree.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct Branch {
    /// The kind of branch this is.
    pub kind: BranchKind,

    /// The collection of nodes that are children of this branch.
    pub nodes: Vec<Node>,
}

/// The constant ID that every [`Tree`] will use as the root branch ID.
const ROOT_BRANCH_ID: ID<Branch> = ID::new(0);

/// Represents the token tree where each of the branches are the fragments
/// in the token stream.
///
/// This is useful for easy traversal of the tree and for incremental
/// compilation compatibility.
#[derive(Debug, Clone, PartialEq, Eq, Deref, Serialize)]
pub struct Tree(Arena<Branch>);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct DelimiterMarker {
    delimiter: DelimiterKind,
    location: AbsoluteSpan,
    open_puncutation_index: usize,
    starting_indentation_level: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
enum IndentationSize {
    /// Found colons and new lines, but hasn't figured out the size yet.
    Unassigned,

    /// Found colons and new lines, and has a valid new indentation size.
    Assigned(usize),

    /// Found a new invalid indentation size, stop the search.
    Errored,
}

struct IndentationMarker {
    indentation_size: Option<usize>,
    colon_span: AbsoluteSpan,
    colon_index: usize,
}

struct Converter<'a, 'handler> {
    tokenizer: Peekable<Tokenizer<'a, 'handler>>,
    source: &'a str,
    handler: &'handler dyn Handler<error::Error>,

    delimiter_stack: Vec<DelimiterMarker>,
    indentation_stack: Vec<IndentationMarker>,

    current_nodes: Vec<Node>,
    tree: Tree,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum LastWas {
    NewLine,
    Colon,
    Any,
}

impl Converter<'_, '_> {
    const TAB_INDENT_SIZE: usize = 4;

    #[allow(clippy::too_many_lines)]
    fn forward(&mut self) -> bool {
        if let Some(token) = self.tokenizer.next() {
            let last_was = self.current_nodes.last().map(|x| {
                x.as_leaf().map_or(LastWas::Any, |x| match x.kind {
                    kind::Kind::NewLine(_) => LastWas::NewLine,
                    kind::Kind::Punctuation(':') => LastWas::Colon,
                    _ => LastWas::Any,
                })
            });

            self.current_nodes.push(Node::Leaf(Token {
                kind: token.kind,
                span: RelativeSpan {
                    start: RelativeLocation {
                        offset: token.span.start,
                        mode: OffsetMode::Start,
                        relative_to: ROOT_BRANCH_ID,
                    },
                    end: RelativeLocation {
                        offset: token.span.end,
                        mode: OffsetMode::End,
                        relative_to: ROOT_BRANCH_ID,
                    },
                    source_id: token.span.source_id,
                },
                prior_insignificant: token.prior_insignificant.map(|prior| {
                    RelativeSpan {
                        start: RelativeLocation {
                            offset: prior.start,
                            mode: OffsetMode::Start,
                            relative_to: ROOT_BRANCH_ID,
                        },
                        end: RelativeLocation {
                            offset: prior.end,
                            mode: OffsetMode::End,
                            relative_to: ROOT_BRANCH_ID,
                        },
                        source_id: prior.source_id,
                    }
                }),
            }));

            // closing delimiter
            if let Token {
                kind: kind::Kind::Punctuation(punc @ (')' | '}' | ']')),
                ..
            } = self.current_nodes.last().unwrap().as_leaf().unwrap()
            {
                self.handle_closing_delimiter(*punc, token.span);
                return true;
            }

            // check for indentation marker
            let indentation_range = self.current_indentation_range();

            //  handle indentation
            if last_was == Some(LastWas::NewLine)
                && token.kind != kind::Kind::NewLine(kind::NewLine)
                && self.indentation_stack.len() > indentation_range.start
            {
                self.handle_indentation(
                    indentation_range,
                    token.span,
                    token.prior_insignificant,
                );
            }

            true
        } else {
            false
        }
    }

    fn handle_indentation(
        &mut self,
        indentation_range: Range<usize>,
        token_span: AbsoluteSpan,
        prior_insignificant: Option<AbsoluteSpan>,
    ) {
        // calculate the indentation size
        let indentation_size = prior_insignificant.map_or(0usize, |x| {
            self.source[x.start..x.end]
                .chars()
                .map(|x| if x == '\t' { Self::TAB_INDENT_SIZE } else { 1 })
                .sum()
        });

        // need to assign the indentation size
        let mut new_indentation = false;
        if self.indentation_stack.last().unwrap().indentation_size.is_none() {
            let prior_indentation = self.indentation_stack
                [indentation_range.clone()]
            .iter()
            .rev()
            .find(|x| x.indentation_size.is_some());

            // must be deeper indentation
            if indentation_size == 0 {
                self.handler.receive(error::Error::ExpectIndentation(
                    ExpectIndentation {
                        span: token_span,
                        indentation_start: self
                            .indentation_stack
                            .last()
                            .unwrap()
                            .colon_span,
                    },
                ));

                // TODO: pop the indentation marker
            } else if let Some(prior_indentation) = prior_indentation {
                // must be deeper indentation
                if indentation_size
                    <= prior_indentation.indentation_size.unwrap()
                {
                    self.handler.receive(
                        error::Error::InvalidNewIndentationLevel(
                            InvalidNewIndentationLevel {
                                span: token_span,
                                previous_indentation_span: prior_indentation
                                    .colon_span,
                                latest_indentation: prior_indentation
                                    .indentation_size
                                    .unwrap(),
                                found_indentation: indentation_size,
                            },
                        ),
                    );

                    // TODO: pop the indentation marker
                }

                // assign the indentation size
                self.indentation_stack.last_mut().unwrap().indentation_size =
                    Some(indentation_size);
                new_indentation = true;
            } else {
                // assign the indentation size
                self.indentation_stack.last_mut().unwrap().indentation_size =
                    Some(indentation_size);
                new_indentation = true;
            }
        }

        if !new_indentation {
            return;
        }

        // check if the indentation size is valid
        let pop_count = if indentation_size == 0 {
            // pop all indentation markers
            self.indentation_stack.len() - indentation_range.start
        } else if let Some((indentation, index)) = self.indentation_stack
            [indentation_range.clone()]
        .iter()
        .zip(indentation_range.clone())
        .rev()
        .min_by_key(|(indent, _)| {
            indent.indentation_size.map_or(0, |x| x.abs_diff(indentation_size))
        }) {
            // mismatched indentation size
            if indentation.indentation_size.unwrap().abs_diff(indentation_size)
                > 0
            {
                self.handler.receive(error::Error::InvalidIndentation(
                    InvalidIndentation {
                        span: token_span,
                        found_indentation: indentation_size,
                        available_indentations: self.indentation_stack
                            [indentation_range.clone()]
                        .iter()
                        .map(|x| AvailableIndentation {
                            colon_span: x.colon_span,
                            indentation_size: x.indentation_size.unwrap(),
                        })
                        .collect(),
                    },
                ));
            }

            indentation_range.end - index - 1
        } else {
            unreachable!()
        };

        // pop the indentation markers
        if pop_count > 0 {
            self.pop_indentation_marker(pop_count);
        }
    }

    fn handle_closing_delimiter(
        &mut self,
        punc: char,
        token_span: AbsoluteSpan,
    ) {
        let expected_delimiter = match punc {
            ')' => DelimiterKind::Parenthesis,
            '}' => DelimiterKind::Brace,
            ']' => DelimiterKind::Bracket,
            _ => unreachable!(),
        };

        if let Some(last) = self.delimiter_stack.pop() {
            // mismatched closing delimiter
            if last.delimiter != expected_delimiter {
                self.handler.receive(error::Error::MismatchedClosingDelimiter(
                    MismatchedClosingDelimiter {
                        span: token_span,
                        opening_span: last.location,
                        closing_delimiter: expected_delimiter,
                        opening_delimiter: last.delimiter,
                    },
                ));
            }

            // force pop all indentations
            let pop_count =
                self.indentation_stack.len() - last.starting_indentation_level;

            if pop_count > 0 {
                self.pop_indentation_marker(pop_count);
            }
        } else {
            // unexpected closing delimiter
            self.handler.receive(error::Error::UnexpectedClosingDelimiter(
                UnexpectedClosingDelimiter { span: token_span },
            ));
        }
    }

    fn has_indentation(&self) -> bool {
        let i = self
            .delimiter_stack
            .last()
            .map_or(0, |x| x.starting_indentation_level);

        i == self.indentation_stack.len()
    }

    fn current_indentation_range(&self) -> Range<usize> {
        self.delimiter_stack.last().map_or(0, |x| x.starting_indentation_level)
            ..self.indentation_stack.len()
    }

    fn current_indentation_marker(&self) -> Option<&IndentationMarker> {
        let i = self
            .delimiter_stack
            .last()
            .map_or(0, |x| x.starting_indentation_level);

        if i == self.indentation_stack.len() {
            None
        } else {
            self.indentation_stack.last()
        }
    }

    fn pop_indentation_marker(&mut self, pop_count: usize) {}

    fn pop_invalid_indentation_marker(&mut self) {
        let indentation_marker = self.indentation_stack.pop().unwrap();
        assert!(indentation_marker.indentation_size.is_none());

        let mut iter = self.current_nodes.splice(
            indentation_marker.colon_index
                ..(indentation_marker.colon_index + 2),
            std::iter::empty(),
        );

        let colon = iter.next().unwrap();
        let new_line = iter.next().unwrap();

        assert!(colon
            .as_leaf()
            .and_then(|x| x.kind.as_punctuation())
            .is_some_and(|x| *x == ':'));
        assert!(new_line.as_leaf().is_some_and(|x| x.kind.is_new_line()));
        assert!(iter.next().is_none());

        drop(iter);

        let colon = colon.into_leaf().unwrap();
        let new_line = new_line.into_leaf().unwrap();

        let branch_kind = BranchKind::Fragment(FragmentBranch {
            fragment_kind: FragmentKind::Indentation(Indentation {
                indentation_size: indentation_marker.indentation_size.unwrap(),
                colon: Token {
                    kind: colon.kind.into_punctuation().unwrap(),
                    span: colon.span,
                    prior_insignificant: colon.prior_insignificant,
                },
                new_line: Token {
                    kind: colon.kind.into_new_line().unwrap(),
                    span: new_line.span,
                    prior_insignificant: new_line.prior_insignificant,
                },
            }),
            parent: ROOT_BRANCH_ID,
        });
        let branch = Branch { kind: branch_kind, nodes: Vec::new() };

        // calculate the branch ID
        let mut hash = {
            let hasher = FnvHasher::default();
            hasher.finish()
        };

        // avoiding hash collision
        while hash == ROOT_BRANCH_ID.index()
            || self.tree.contains_id(ID::new(hash))
        {
            hash = hash.wrapping_add(1);
        }

        // create the branch
        self.tree.0.insert_with_id(ID::new(hash), branch).unwrap();
        self.current_nodes.insert(
            indentation_marker.colon_index,
            Node::Branch(ID::new(hash)),
        );
    }
}
