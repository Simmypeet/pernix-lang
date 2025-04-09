//! Contains the definition of the [`Tree`] struct

use pernixc_arena::{Arena, ID};
use pernixc_source_file::{ByteIndex, Span};
use serde::{Deserialize, Serialize};

use crate::{token::Token, token_stream::FragmentKind};

/// Specifiying the position in the branch (begin or end) that will be used for
/// calculating the relative offset of the token.
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
    Deserialize,
)]
pub enum RelativeMode {
    /// Relative to the start byte of the branch.
    Start,

    /// Relative to the end byte of the branch.
    End,
}

/// Used for represnting a particular location in the source code that may use
/// relative offsets from the token node to represent the location of a token.
///
/// This is primarily beneficial for incremental compilation and parsing.
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
    Deserialize,
)]
pub struct RelativeSpan<I: 'static> {
    /// The start byte of the span range.
    pub start: ByteIndex,

    /// The end byte of the span range (exclusive).
    pub end: ByteIndex,

    /// The mode of the relative span (start or end).
    pub mode: RelativeMode,

    /// The branch ID that this relative span is relative to.
    pub to: ID<Branch<I>>,

    /// The source ID of the token.
    pub source_id: I,
}

/// A tree node used for representing a particular token in the source code.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[allow(missing_docs)]
pub enum Node<I: 'static> {
    Leaf(Token<RelativeSpan<I>>),
    Branch(ID<Branch<I>>),
}

/// Represents a branch that has a [`FragmentKind`] as its kind.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct FragmentBranch<I: 'static> {
    /// The kind of fragment this branch represents.
    pub fragment_kind: FragmentKind<RelativeSpan<I>>,

    /// The ID to the parent branch of this fragment.
    pub parent: ID<Branch<I>>,
}

/// Represents the kind of branch in the token tree.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[allow(missing_docs)]
pub enum BranchKind<I: 'static> {
    Fragment(FragmentBranch<I>),

    /// The root branch of the token tree.
    Root,
}

/// Represents a branch node in the token tree.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Branch<I: 'static> {
    /// The kind of branch this is.
    pub kind: BranchKind<I>,

    /// The collection of nodes that are children of this branch.
    pub nodes: Vec<Node<I>>,
}

/// Represents the token tree where each of the branches are the fragments
/// in the token stream.
///
/// This is useful for easy traversal of the tree and for incremental
/// compilation compatibility.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Tree<I: 'static> {
    branches: Arena<Branch<I>>,
    root: ID<Branch<I>>,
}

impl<I: Clone + 'static> Tree<I> {
    /// Gets the absolute start byte of the given branch ID.
    #[must_use]
    pub fn absolute_start_byte_of(&self, id: ID<Branch<I>>) -> ByteIndex {
        match &self.branches[id].kind {
            BranchKind::Fragment(fragment_branch) => {
                match &fragment_branch.fragment_kind {
                    FragmentKind::Delimiter(delimiter) => {
                        self.absolute_span(&delimiter.open.span).end
                    }

                    FragmentKind::Indentation(indentation) => {
                        self.absolute_span(&indentation.colon.span).end
                    }
                }
            }
            BranchKind::Root => 0,
        }
    }

    /// Gets the absolute end byte of the given branch ID.
    #[must_use]
    pub fn absoluate_end_byte_of(&self, mut id: ID<Branch<I>>) -> ByteIndex {
        loop {
            let branch = &self.branches[id];
            match &branch.kind {
                BranchKind::Fragment(fragment_branch) => match &fragment_branch
                    .fragment_kind
                {
                    FragmentKind::Delimiter(delimiter) => {
                        return self.absolute_span(&delimiter.close.span).end
                    }

                    FragmentKind::Indentation(indentation) => {
                        if let Some(x) = branch.nodes.last() {
                            match x {
                                Node::Leaf(leaf) => {
                                    return self.absolute_span(leaf.span()).end;
                                }
                                Node::Branch(new_id) => {
                                    id = *new_id;
                                }
                            }
                        } else {
                            return self
                                .absolute_span(&indentation.new_line.span)
                                .end;
                        }
                    }
                },

                BranchKind::Root => match branch.nodes.last() {
                    Some(node) => match node {
                        Node::Leaf(leaf) => {
                            return self.absolute_span(leaf.span()).end
                        }

                        Node::Branch(new_id) => {
                            id = *new_id;
                        }
                    },
                    None => return 0,
                },
            }
        }
    }

    /// Calculates the absolute span of the given relative span.
    #[must_use]
    pub fn absolute_span(&self, relative_span: &RelativeSpan<I>) -> Span<I> {
        let offset = match relative_span.mode {
            RelativeMode::Start => {
                self.absolute_start_byte_of(relative_span.to)
            }
            RelativeMode::End => self.absoluate_end_byte_of(relative_span.to),
        };

        Span::new(
            relative_span.start + offset,
            relative_span.end + offset,
            relative_span.source_id.clone(),
        )
    }
}
