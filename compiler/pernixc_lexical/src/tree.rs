//! Contains the definition of the [`Tree`] struct

use std::hash::{Hash as _, Hasher};

use fnv::FnvHasher;
use pernixc_arena::{Arena, ID};
use pernixc_source_file::{ByteIndex, Span};
use serde::{Deserialize, Serialize};

use crate::{
    token::{
        Character, Identifier, Keyword, Kind, NewLine, Numeric, Punctuation,
        String as StringToken, Token, WithInsignificant,
    },
    token_stream::{
        self, Delimiter, DelimiterKind, FragmentKind, Indentation, TokenStream,
    },
};

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
pub enum OffsetMode {
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
    pub offset_mode: OffsetMode,

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
    root_id: ID<Branch<I>>,
}

fn absolute_end_byte_of<I: Clone + 'static>(
    branches: &Arena<Branch<I>>,
    mut id: ID<Branch<I>>,
) -> ByteIndex {
    loop {
        let branch = &branches[id];
        match &branch.kind {
            BranchKind::Fragment(fragment_branch) => {
                match &fragment_branch.fragment_kind {
                    FragmentKind::Delimiter(delimiter) => {
                        return absolute_span_of(
                            branches,
                            &delimiter.close.span,
                        )
                        .end
                    }

                    FragmentKind::Indentation(indentation) => {
                        if let Some(x) = branch.nodes.last() {
                            match x {
                                Node::Leaf(leaf) => {
                                    return absolute_span_of(
                                        branches,
                                        leaf.span(),
                                    )
                                    .end;
                                }
                                Node::Branch(new_id) => {
                                    id = *new_id;
                                }
                            }
                        } else {
                            return absolute_span_of(
                                branches,
                                &indentation.new_line.span,
                            )
                            .end;
                        }
                    }
                }
            }

            BranchKind::Root => match branch.nodes.last() {
                Some(node) => match node {
                    Node::Leaf(leaf) => {
                        return absolute_span_of(branches, leaf.span()).end
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

fn absolute_start_byte_of<I: Clone + 'static>(
    branches: &Arena<Branch<I>>,
    id: ID<Branch<I>>,
) -> ByteIndex {
    match &branches[id].kind {
        BranchKind::Fragment(fragment_branch) => {
            match &fragment_branch.fragment_kind {
                FragmentKind::Delimiter(delimiter) => {
                    absolute_span_of(branches, &delimiter.open.span).end
                }

                FragmentKind::Indentation(indentation) => {
                    absolute_span_of(branches, &indentation.colon.span).end
                }
            }
        }
        BranchKind::Root => 0,
    }
}

fn absolute_span_of<I: Clone + 'static>(
    branches: &Arena<Branch<I>>,
    relative_span: &RelativeSpan<I>,
) -> Span<I> {
    let offset = match relative_span.offset_mode {
        OffsetMode::Start => absolute_start_byte_of(branches, relative_span.to),
        OffsetMode::End => absolute_end_byte_of(branches, relative_span.to),
    };

    Span::new(
        relative_span.start + offset,
        relative_span.end + offset,
        relative_span.source_id.clone(),
    )
}

impl<I: Clone + 'static> Tree<I> {
    /// Gets the absolute start byte of the given branch ID.
    #[must_use]
    pub fn absolute_start_byte_of(&self, id: ID<Branch<I>>) -> ByteIndex {
        absolute_start_byte_of(&self.branches, id)
    }

    /// Gets the absolute end byte of the given branch ID.
    #[must_use]
    pub fn absoluate_end_byte_of(&self, id: ID<Branch<I>>) -> ByteIndex {
        absolute_end_byte_of(&self.branches, id)
    }

    /// Calculates the absolute span of the given relative span.
    #[must_use]
    pub fn absolute_span_of(&self, relative_span: &RelativeSpan<I>) -> Span<I> {
        absolute_span_of(&self.branches, relative_span)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct OffsetInfo<I: 'static> {
    mode: OffsetMode,
    pos: ByteIndex,
    branch: ID<Branch<I>>,
}

impl<I: 'static> OffsetInfo<I> {
    fn to_relative_span(&self, span: Span<I>) -> RelativeSpan<I> {
        RelativeSpan {
            start: span.start - self.pos,
            end: span.end - self.pos,
            offset_mode: self.mode,
            to: self.branch,
            source_id: span.source_id,
        }
    }

    fn to_relative_punctuation(
        &self,
        punc: WithInsignificant<Punctuation<Span<I>>, Span<I>>,
    ) -> WithInsignificant<Punctuation<RelativeSpan<I>>, RelativeSpan<I>> {
        WithInsignificant {
            token: Punctuation {
                span: self.to_relative_span(punc.token.span),
                punctuation: punc.token.punctuation,
            },
            prior_insignificant: punc
                .prior_insignificant
                .map(|x| self.to_relative_span(x)),
        }
    }

    fn to_relative_newline(
        &self,
        punc: WithInsignificant<NewLine<Span<I>>, Span<I>>,
    ) -> WithInsignificant<NewLine<RelativeSpan<I>>, RelativeSpan<I>> {
        WithInsignificant {
            token: NewLine { span: self.to_relative_span(punc.token.span) },
            prior_insignificant: punc
                .prior_insignificant
                .map(|x| self.to_relative_span(x)),
        }
    }

    fn to_relative_token(
        &self,
        token: Token<Span<I>>,
    ) -> Token<RelativeSpan<I>> {
        WithInsignificant {
            token: match token.token {
                Kind::Identifier(identifier) => Kind::Identifier(Identifier {
                    span: self.to_relative_span(identifier.span),
                }),
                Kind::Keyword(keyword) => Kind::Keyword(Keyword {
                    span: self.to_relative_span(keyword.span),
                    kind: keyword.kind,
                }),
                Kind::Punctuation(punctuation) => {
                    Kind::Punctuation(Punctuation {
                        span: self.to_relative_span(punctuation.span),
                        punctuation: punctuation.punctuation,
                    })
                }
                Kind::Numeric(numeric) => Kind::Numeric(Numeric {
                    span: self.to_relative_span(numeric.span),
                }),
                Kind::Character(character) => Kind::Character(Character {
                    span: self.to_relative_span(character.span),
                    value: character.value,
                }),
                Kind::String(str) => Kind::String(StringToken {
                    span: self.to_relative_span(str.span),
                    is_valid: str.is_valid,
                }),
                Kind::NewLine(new_line) => Kind::NewLine(NewLine {
                    span: self.to_relative_span(new_line.span),
                }),
            },
            prior_insignificant: token
                .prior_insignificant
                .map(|x| self.to_relative_span(x)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum GeneralBranchKind {
    Root,
    Delimited(DelimiterKind),
    Indentation,
}

/// Calculates the hash of a branch based on its kind and the tokens it
/// contains.
fn calculate_branch_hash<I: 'static>(
    nodes: &[token_stream::Node<Span<I>>],
    kind: GeneralBranchKind,
    source: &str,
) -> u64 {
    let mut hasher = FnvHasher::default();

    for node in nodes {
        if let token_stream::Node::Leaf(token) = node {
            let range = token.prior_insignificant.as_ref().map_or_else(
                || token.token.span().range(),
                |x| {
                    let start = x.range();
                    let end = token.token.span().range();

                    start.start..end.end
                },
            );

            source[range].hash(&mut hasher);
        }
    }

    kind.hash(&mut hasher);

    hasher.finish()
}

fn calculate_next_branch_id<I: 'static>(
    arena: &Arena<Branch<I>>,
    nodes: &[token_stream::Node<Span<I>>],
    kind: GeneralBranchKind,
    source: &str,
) -> ID<Branch<I>> {
    let mut hash = calculate_branch_hash(nodes, kind, source);

    while arena.contains_id(ID::new(hash)) {
        hash = hash.wrapping_add(1);
    }

    ID::new(hash)
}

fn create_tree_from_fragment<I: Clone + 'static>(
    branches: &mut Arena<Branch<I>>,
    token_nodes: Vec<token_stream::Node<Span<I>>>,
    fragment_kind: Option<token_stream::FragmentKind<Span<I>>>,
    mut offset_info: OffsetInfo<I>,
    source: &str,
) -> ID<Branch<I>> {
    let branch_id = calculate_next_branch_id(
        branches,
        &token_nodes,
        match &fragment_kind {
            Some(token_stream::FragmentKind::Delimiter(d)) => {
                GeneralBranchKind::Delimited(d.delimiter)
            }
            Some(token_stream::FragmentKind::Indentation(_)) => {
                GeneralBranchKind::Indentation
            }
            None => GeneralBranchKind::Root,
        },
        source,
    );

    if fragment_kind.is_none() {
        offset_info =
            OffsetInfo { mode: OffsetMode::Start, pos: 0, branch: branch_id };
    }

    branches
        .insert_with_id(branch_id, Branch {
            kind: fragment_kind.map_or_else(
                || BranchKind::Root,
                |kind| match kind {
                    FragmentKind::Delimiter(delimiter) => {
                        let new_offset = delimiter.open.span.end;

                        let new_open =
                            offset_info.to_relative_punctuation(delimiter.open);

                        offset_info = OffsetInfo {
                            mode: OffsetMode::Start,
                            pos: new_offset,
                            branch: branch_id,
                        };

                        let new_close = offset_info
                            .to_relative_punctuation(delimiter.close);

                        BranchKind::Fragment(FragmentBranch {
                            fragment_kind: FragmentKind::Delimiter(Delimiter {
                                open: new_open,
                                close: new_close,
                                delimiter: delimiter.delimiter,
                            }),
                            parent: offset_info.branch,
                        })
                    }
                    FragmentKind::Indentation(indentation) => {
                        let new_offset = indentation.colon.span.end;

                        let new_colon = offset_info
                            .to_relative_punctuation(indentation.colon);

                        offset_info = OffsetInfo {
                            mode: OffsetMode::Start,
                            pos: new_offset,
                            branch: branch_id,
                        };

                        let new_new_line = offset_info
                            .to_relative_newline(indentation.new_line);

                        BranchKind::Fragment(FragmentBranch {
                            fragment_kind: FragmentKind::Indentation(
                                Indentation {
                                    indentation_size: indentation
                                        .indentation_size,
                                    colon: new_colon,
                                    new_line: new_new_line,
                                    ending_token: indentation.ending_token.map(
                                        |x| offset_info.to_relative_token(x),
                                    ),
                                },
                            ),
                            parent: offset_info.branch,
                        })
                    }
                },
            ),
            nodes: Vec::new(),
        })
        .unwrap_or_else(|_| panic!("Failed to insert root branch"));

    let mut nodes = Vec::new();

    for token in token_nodes {
        match token {
            token_stream::Node::Leaf(token) => {
                nodes.push(Node::Leaf(offset_info.to_relative_token(token)));
            }

            token_stream::Node::Fragment(fragment) => {
                let new_branch_id = create_tree_from_fragment(
                    branches,
                    fragment.token_stream.0,
                    Some(fragment.kind),
                    offset_info.clone(),
                    source,
                );

                let new_offset = absolute_end_byte_of(branches, new_branch_id);
                offset_info = OffsetInfo {
                    mode: OffsetMode::End,
                    pos: new_offset,
                    branch: new_branch_id,
                };

                nodes.push(Node::Branch(new_branch_id));
            }
        }
    }

    branches[branch_id].nodes = nodes;

    branch_id
}

impl<I: Clone + 'static> Tree<I> {
    /// Creates a new [`Tree`] from the given [`TokenStream`].
    #[must_use]
    pub fn from_token_stream(
        token_stream: TokenStream<Span<I>>,
        source: &str,
    ) -> Self {
        let mut branches = Arena::new();
        let root_id = create_tree_from_fragment(
            &mut branches,
            token_stream.0,
            None,
            OffsetInfo { mode: OffsetMode::Start, pos: 0, branch: ID::new(0) },
            source,
        );

        Self { branches, root_id }
    }
}
