//! Responsible for structuring the stream of tokens into a tree-like structure.

use std::{
    hash::{Hash, Hasher},
    ops::Range,
};

use derive_more::Deref;
use enum_as_inner::EnumAsInner;
use fnv::FnvHasher;
use pernixc_arena::{Arena, ID};
use pernixc_handler::Handler;
use pernixc_source_file::{
    AbsoluteSpan, ByteIndex, GlobalSourceID, Location, SourceFile, Span,
};
use serde::Serialize;
use strum_macros::EnumIter;

use crate::{
    error::{
        self, AvailableIndentation, ExpectIndentation, InvalidIndentation,
        InvalidNewIndentationLevel, MismatchedClosingDelimiter,
        UndelimitedDelimiter, UnexpectedClosingDelimiter,
    },
    kind,
    token::{Kind, NewLine, Punctuation, Token, Tokenizer},
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

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
    pub open: Punctuation<RelativeLocation>,

    /// The closing delimiter.
    pub close: Punctuation<RelativeLocation>,

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
    pub colon: Punctuation<RelativeLocation>,

    /// The new line character that follows the colon.
    ///
    /// The language expect that if a colon is immediately followed by a
    /// newline, then a new indentation level is started.
    pub new_line: NewLine<RelativeLocation>,
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
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Node {
    Leaf(Kind<RelativeLocation>),
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
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, EnumAsInner,
)]
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

impl Tree {
    /// Creates a new [`Tree`] from the given source code and source ID.
    ///
    /// The `source_id` will be assigned to all the spans in the tree.
    pub fn from_source(
        source: &str,
        source_id: GlobalSourceID,
        handler: &dyn Handler<error::Error>,
    ) -> Self {
        let mut converter = Converter {
            tokenizer: Tokenizer::new(source, source_id, handler),
            source,
            handler,
            delimiter_stack: Vec::new(),
            indentation_stack: Vec::new(),
            current_nodes: Vec::new(),
            tree: Self(Arena::new()),
        };

        while converter.forward() {}

        // pop all indentation markers
        if !converter.indentation_stack.is_empty() {
            converter.pop_indentation_marker(
                converter.indentation_stack.len(),
                true,
            );
        }

        Converter::make_branch_relative(
            &mut converter.tree.0,
            ROOT_BRANCH_ID,
            0,
            &mut converter.current_nodes,
        );

        converter
            .tree
            .0
            .insert_with_id(ROOT_BRANCH_ID, Branch {
                kind: BranchKind::Root,
                nodes: converter.current_nodes,
            })
            .unwrap();

        // report all of the enclosed opening delimiters
        for delimiter in converter.delimiter_stack {
            handler.receive(error::Error::UndelimitedDelimiter(
                UndelimitedDelimiter {
                    opening_span: delimiter.location,
                    delimiter: delimiter.delimiter,
                },
            ));
        }

        converter.tree
    }

    /// Gets the `prior_insignificant` span of the first token in the fragment.
    #[must_use]
    pub fn first_fragment_token_insignificant_span(
        &self,
        branch_id: ID<Branch>,
    ) -> Option<&RelativeSpan> {
        let branch = &self.0[branch_id];
        match &branch.kind {
            BranchKind::Fragment(fragment_branch) => {
                match &fragment_branch.fragment_kind {
                    FragmentKind::Delimiter(delimiter) => {
                        delimiter.open.prior_insignificant.as_ref()
                    }
                    FragmentKind::Indentation(indentation) => {
                        indentation.colon.prior_insignificant.as_ref()
                    }
                }
            }

            BranchKind::Root => branch.nodes.first().and_then(|x| match x {
                Node::Leaf(token) => token.prior_insignificant.as_ref(),
                Node::Branch(id) => {
                    self.first_fragment_token_insignificant_span(*id)
                }
            }),
        }
    }
}

fn absolute_end_byte_of(
    branches: &Arena<Branch>,
    mut id: ID<Branch>,
) -> ByteIndex {
    loop {
        let branch = &branches[id];
        match &branch.kind {
            BranchKind::Fragment(fragment_branch) => {
                match &fragment_branch.fragment_kind {
                    FragmentKind::Delimiter(delimiter) => {
                        return absolute_location_of(
                            branches,
                            &delimiter.close.span.end,
                        )
                    }

                    FragmentKind::Indentation(indentation) => {
                        if let Some(x) = branch.nodes.last() {
                            match x {
                                Node::Leaf(leaf) => {
                                    return absolute_location_of(
                                        branches,
                                        &leaf.span.end,
                                    )
                                }
                                Node::Branch(new_id) => {
                                    id = *new_id;
                                }
                            }
                        } else {
                            return absolute_location_of(
                                branches,
                                &indentation.new_line.span.end,
                            );
                        }
                    }
                }
            }

            BranchKind::Root => match branch.nodes.last() {
                Some(node) => match node {
                    Node::Leaf(leaf) => {
                        return absolute_location_of(branches, &leaf.span.end)
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

fn absolute_start_byte_of(
    branches: &Arena<Branch>,
    id: ID<Branch>,
) -> ByteIndex {
    match &branches[id].kind {
        BranchKind::Fragment(fragment_branch) => {
            match &fragment_branch.fragment_kind {
                FragmentKind::Delimiter(delimiter) => {
                    absolute_location_of(branches, &delimiter.open.span.end)
                }

                FragmentKind::Indentation(indentation) => {
                    absolute_location_of(branches, &indentation.colon.span.end)
                }
            }
        }
        BranchKind::Root => 0,
    }
}

fn absolute_location_of(
    branches: &Arena<Branch>,
    relative_location: &RelativeLocation,
) -> ByteIndex {
    let offset = match relative_location.mode {
        OffsetMode::Start => {
            absolute_start_byte_of(branches, relative_location.relative_to)
        }
        OffsetMode::End => {
            absolute_end_byte_of(branches, relative_location.relative_to)
        }
    };

    relative_location.offset + offset
}

fn absolute_span_of(
    branches: &Arena<Branch>,
    relative_location: &RelativeSpan,
) -> AbsoluteSpan {
    let start = absolute_location_of(branches, &relative_location.start);
    let end = absolute_location_of(branches, &relative_location.end);

    AbsoluteSpan { start, end, source_id: relative_location.source_id }
}

impl Tree {
    /// Gets the absolute start byte of the given branch ID.
    #[must_use]
    pub fn absolute_start_byte_of(&self, id: ID<Branch>) -> ByteIndex {
        absolute_start_byte_of(&self.0, id)
    }

    /// Gets the absolute end byte of the given branch ID.
    #[must_use]
    pub fn absoluate_end_byte_of(&self, id: ID<Branch>) -> ByteIndex {
        absolute_end_byte_of(&self.0, id)
    }

    /// Calculates the absolute span of the given relative span.
    #[must_use]
    pub fn absolute_span_of(
        &self,
        relative_span: &RelativeSpan,
    ) -> AbsoluteSpan {
        absolute_span_of(&self.0, relative_span)
    }

    /// Calculates the absolute location of the given relative location.
    #[must_use]
    pub fn absolute_location_of(
        &self,
        relative_location: &RelativeLocation,
    ) -> ByteIndex {
        absolute_location_of(&self.0, relative_location)
    }
}

impl Location<&Tree> for RelativeLocation {
    fn to_absolute_index(&self, _: &SourceFile, context: &Tree) -> ByteIndex {
        context.absolute_location_of(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct DelimiterMarker {
    delimiter: DelimiterKind,
    location: AbsoluteSpan,
    open_puncutation_index: usize,
    starting_indentation_level: usize,
}

#[derive(Debug)]
struct IndentationMarker {
    indentation_size: Option<usize>,
    colon_span: AbsoluteSpan,
    colon_index: usize,
}

struct Converter<'source, 'handler> {
    tokenizer: Tokenizer<'source, 'handler>,
    source: &'source str,
    handler: &'handler dyn Handler<error::Error>,

    delimiter_stack: Vec<DelimiterMarker>,
    indentation_stack: Vec<IndentationMarker>,

    current_nodes: Vec<Node>,
    tree: Tree,
}

impl std::fmt::Debug for Converter<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Converter")
            .field("tokenizer", &self.tokenizer)
            .field("source", &self.source)
            .field("delimiter_stack", &self.delimiter_stack)
            .field("indentation_stack", &self.indentation_stack)
            .field("current_nodes", &self.current_nodes)
            .field("tree", &self.tree)
            .finish_non_exhaustive()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum LastWas {
    NewLine,
    Colon,
    Any,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum GeneralBranchKind {
    Delimited(DelimiterKind),
    Indented,
}

fn calculate_branch_hash<'a>(
    tokens: impl Iterator<Item = &'a Kind<RelativeLocation>>,
    node_kind: GeneralBranchKind,
    source: &str,
    arena: &Arena<Branch>,
) -> ID<Branch> {
    let mut hasher = FnvHasher::default();
    node_kind.hash(&mut hasher);

    // hash the string content of the tokens
    for token in tokens {
        let start_byte = token
            .prior_insignificant
            .as_ref()
            .map_or_else(|| token.span.start.offset, |x| x.start.offset);

        let end_byte = token.span.end.offset;
        let str = &source[start_byte..end_byte];

        str.hash(&mut hasher);
    }

    let mut attempt = 0;
    loop {
        // for some reason, FnvHasher doesn't implement `Clone` trait.
        // this is the work around to clone
        let mut final_hasher = FnvHasher::with_key(hasher.finish());
        attempt.hash(&mut final_hasher);

        let candidate_branch_id = ID::new(final_hasher.finish());

        // avoid hash collision
        if candidate_branch_id != ROOT_BRANCH_ID
            && !arena.contains_id(candidate_branch_id)
        {
            return candidate_branch_id;
        }

        attempt += 1;
    }
}

impl Converter<'_, '_> {
    const TAB_INDENT_SIZE: usize = 4;

    #[allow(clippy::too_many_lines)]
    fn forward(&mut self) -> bool {
        if let Some(token) = self.tokenizer.next() {
            let last_was = self.current_nodes.last().map(|x| {
                x.as_leaf().map_or(LastWas::Any, |x| match x.kind {
                    kind::Kind::NewLine(_) => LastWas::NewLine,
                    kind::Kind::Punctuation(kind::Punctuation(':')) => {
                        LastWas::Colon
                    }
                    _ => LastWas::Any,
                })
            });
            let is_new_line = token.kind.is_new_line();
            let puncutation = token.kind.as_punctuation().map(|x| **x);

            // for now, everything is relative to the root branch and is a leaf
            // node.
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
                kind:
                    kind::Kind::Punctuation(kind::Punctuation(
                        punc @ (')' | '}' | ']'),
                    )),
                ..
            } = self.current_nodes.last().unwrap().as_leaf().unwrap()
            {
                self.handle_closing_delimiter(*punc, token.span);
                return true;
            }

            // check for indentation marker
            let indentation_range = self.current_indentation_range();

            //  handle new indentation line
            if last_was == Some(LastWas::NewLine)
                && !is_new_line
                && self.indentation_stack.len() > indentation_range.start
            {
                self.handle_indentation(
                    indentation_range,
                    token.span,
                    token.prior_insignificant,
                );
            }

            if let Some(open @ ('(' | '{' | '[')) = puncutation {
                // opening delimiter
                let delimiter = match open {
                    '(' => DelimiterKind::Parenthesis,
                    '{' => DelimiterKind::Brace,
                    '[' => DelimiterKind::Bracket,
                    _ => unreachable!(),
                };

                // push the delimiter marker
                self.delimiter_stack.push(DelimiterMarker {
                    delimiter,
                    location: token.span,
                    open_puncutation_index: self.current_nodes.len() - 1,
                    starting_indentation_level: self.indentation_stack.len(),
                });
            }
            // check for the new indentation marker
            else if last_was == Some(LastWas::Colon) && is_new_line {
                // push the new indentation marker
                let colon = self
                    .current_nodes
                    .get(self.current_nodes.len() - 2)
                    .unwrap()
                    .as_leaf()
                    .unwrap();

                assert!(colon
                    .kind
                    .as_punctuation()
                    .is_some_and(|x| **x == ':'));
                assert!(colon.span.start.relative_to == ROOT_BRANCH_ID);
                assert!(colon.span.end.relative_to == ROOT_BRANCH_ID);

                self.indentation_stack.push(IndentationMarker {
                    indentation_size: None,
                    colon_span: AbsoluteSpan {
                        start: colon.span.start.offset,
                        end: colon.span.end.offset,
                        source_id: colon.span.source_id,
                    },
                    colon_index: self.current_nodes.len() - 2,
                });
            }

            true
        } else {
            false
        }
    }

    #[allow(clippy::too_many_lines)]
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

                self.pop_invalid_indentation_marker();
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

                    self.pop_invalid_indentation_marker();
                } else {
                    // assign the indentation size
                    self.indentation_stack
                        .last_mut()
                        .unwrap()
                        .indentation_size = Some(indentation_size);
                    new_indentation = true;
                }
            } else {
                // assign the indentation size
                self.indentation_stack.last_mut().unwrap().indentation_size =
                    Some(indentation_size);
                new_indentation = true;
            }
        }

        if new_indentation {
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
            let mut minus_one = true;
            if indentation.indentation_size.unwrap().abs_diff(indentation_size)
                > 0
            {
                // if in the delimiter, then the indentation size must be
                // smaller than the outer-most indentation
                // captured in the delimiter
                let error = self.delimiter_stack.last().is_none_or(|x| {
                    if x.starting_indentation_level
                        == self.indentation_stack.len()
                    {
                        return true;
                    }

                    self.indentation_stack[x.starting_indentation_level]
                        .indentation_size
                        .unwrap()
                        < indentation_size
                });

                if error {
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
                } else {
                    minus_one = false;
                }
            }

            indentation_range.end - index - usize::from(minus_one)
        } else {
            unreachable!()
        };

        // pop the indentation markers
        if pop_count > 0 {
            self.pop_indentation_marker(pop_count, false);
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
                self.pop_indentation_marker(pop_count, false);
            }

            // extract the closing delimiter and the opening delimiter
            let closing_delimiter = self
                .current_nodes
                .pop()
                .unwrap()
                .into_leaf()
                .unwrap()
                .map_kind(|x| x.into_punctuation().unwrap());

            assert_eq!(closing_delimiter.kind, kind::Punctuation(punc));

            let mut drain =
                self.current_nodes.drain(last.open_puncutation_index..);

            let opening_delimiter = drain.next().unwrap();
            let opening_delimiter = opening_delimiter
                .into_leaf()
                .unwrap()
                .map_kind(|x| x.into_punctuation().unwrap());

            assert!(matches!(
                opening_delimiter.kind,
                kind::Punctuation('[' | '{' | '(')
            ));

            // collect the nodes
            let mut nodes = drain.collect::<Vec<_>>();
            let branch_id = calculate_branch_hash(
                nodes.iter().filter_map(Node::as_leaf),
                GeneralBranchKind::Delimited(expected_delimiter),
                self.source,
                &self.tree,
            );

            Self::make_branch_relative(
                &mut self.tree.0,
                branch_id,
                opening_delimiter.span.end.offset,
                &mut nodes,
            );

            self.tree
                .0
                .insert_with_id(branch_id, Branch {
                    kind: BranchKind::Fragment(FragmentBranch {
                        fragment_kind: FragmentKind::Delimiter(Delimiter {
                            open: opening_delimiter,
                            close: closing_delimiter,
                            delimiter: expected_delimiter,
                        }),
                        parent: ROOT_BRANCH_ID,
                    }),
                    nodes,
                })
                .unwrap();

            // insert the branch ID
            self.current_nodes
                .insert(last.open_puncutation_index, Node::Branch(branch_id));
        } else {
            // unexpected closing delimiter
            self.handler.receive(error::Error::UnexpectedClosingDelimiter(
                UnexpectedClosingDelimiter { span: token_span },
            ));
        }
    }

    fn current_indentation_range(&self) -> Range<usize> {
        self.delimiter_stack.last().map_or(0, |x| x.starting_indentation_level)
            ..self.indentation_stack.len()
    }

    fn pop_indentation_marker(
        &mut self,
        pop_count: usize,
        was_pop_at_eof: bool,
    ) {
        // find the index (+1) of the last token that is in the indentation
        // block
        let mut end_index = self
            .current_nodes
            .iter()
            .enumerate()
            .rev()
            .skip(usize::from(!was_pop_at_eof))
            .find_map(|(index, node)| {
                node.as_leaf()
                    .is_none_or(|x| !x.kind.is_new_line())
                    .then_some(index)
            })
            .unwrap()
            + 1;

        for _ in 0..pop_count {
            let marker = self.indentation_stack.pop().unwrap();

            let mut iter =
                self.current_nodes.drain(marker.colon_index..end_index);

            // extract colon and new line
            let colon = iter
                .next()
                .unwrap()
                .into_leaf()
                .unwrap()
                .map_kind(|x| x.into_punctuation().unwrap());
            assert_eq!(*colon.kind, ':');
            let new_line = iter
                .next()
                .unwrap()
                .into_leaf()
                .unwrap()
                .map_kind(|x| x.into_new_line().unwrap());

            let mut nodes = iter.collect::<Vec<_>>();
            let branch_hash = calculate_branch_hash(
                nodes.iter().filter_map(Node::as_leaf),
                GeneralBranchKind::Indented,
                self.source,
                &self.tree,
            );

            Self::make_branch_relative(
                &mut self.tree.0,
                branch_hash,
                colon.span.end.offset,
                &mut nodes,
            );

            let branch = Branch {
                kind: BranchKind::Fragment(FragmentBranch {
                    fragment_kind: FragmentKind::Indentation(Indentation {
                        indentation_size: marker.indentation_size.unwrap(),
                        colon,
                        new_line,
                    }),
                    parent: ROOT_BRANCH_ID,
                }),
                nodes,
            };

            // insert the branch
            self.tree.0.insert_with_id(branch_hash, branch).unwrap();

            self.current_nodes
                .insert(marker.colon_index, Node::Branch(branch_hash));

            // update the end index
            end_index = marker.colon_index + 1;
        }
    }

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
            .is_some_and(|x| **x == ':'));
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
                    kind: new_line.kind.into_new_line().unwrap(),
                    span: new_line.span,
                    prior_insignificant: new_line.prior_insignificant,
                },
            }),
            parent: ROOT_BRANCH_ID,
        });
        let branch = Branch { kind: branch_kind, nodes: Vec::new() };

        let branch_id = calculate_branch_hash(
            std::iter::empty(),
            GeneralBranchKind::Indented,
            self.source,
            &self.tree,
        );

        // create the branch
        self.tree.0.insert_with_id(branch_id, branch).unwrap();
        self.current_nodes
            .insert(indentation_marker.colon_index, Node::Branch(branch_id));
    }

    fn make_branch_relative(
        arena: &mut Arena<Branch>,
        parent_branch_id: ID<Branch>,
        mut offset: ByteIndex,
        nodes: &mut [Node],
    ) {
        fn make_token_relative<T>(
            mode: OffsetMode,
            offset: usize,
            offset_to_branch: ID<Branch>,
            token: &mut Token<T, RelativeLocation>,
        ) {
            let make_relative = |span: &mut RelativeSpan| {
                span.start.offset -= offset;
                span.start.mode = mode;
                span.start.relative_to = offset_to_branch;

                span.end.offset -= offset;
                span.end.mode = mode;
                span.end.relative_to = offset_to_branch;
            };

            if let Some(prior) = &mut token.prior_insignificant {
                make_relative(prior);
            }

            make_relative(&mut token.span);
        }

        let mut mode = OffsetMode::Start;
        let mut last_was_branch = false;
        let mut offset_to_branch = parent_branch_id;

        for node in nodes {
            if last_was_branch {
                // the token following after a branch will have a
                // relative offset to the preceeding branch and the
                // will have offset starting with 0
                offset = match node {
                    Node::Leaf(token) => token.start_location().offset,
                    Node::Branch(id) => {
                        let branch = &arena[*id];
                        match &branch.kind {
                            BranchKind::Fragment(fragment_branch) => {
                                match &fragment_branch.fragment_kind {
                                    FragmentKind::Delimiter(delimiter) => {
                                        delimiter.open.start_location().offset
                                    }
                                    FragmentKind::Indentation(indentation) => {
                                        indentation
                                            .colon
                                            .start_location()
                                            .offset
                                    }
                                }
                            }
                            BranchKind::Root => 0,
                        }
                    }
                }
            }

            match node {
                Node::Leaf(token) => {
                    make_token_relative(mode, offset, offset_to_branch, token);

                    last_was_branch = false;
                }

                Node::Branch(branch_id) => {
                    let branch = &mut arena[*branch_id];
                    let fragment = branch.kind.as_fragment_mut().unwrap();

                    // correct the parent branch
                    fragment.parent = parent_branch_id;

                    match &mut fragment.fragment_kind {
                        FragmentKind::Delimiter(delimiter) => {
                            let delimiter_start =
                                delimiter.open.span.end.offset;

                            make_token_relative(
                                mode,
                                offset,
                                offset_to_branch,
                                &mut delimiter.open,
                            );

                            make_token_relative(
                                OffsetMode::Start,
                                delimiter_start,
                                *branch_id,
                                &mut delimiter.close,
                            );
                        }
                        FragmentKind::Indentation(indentation) => {
                            let delimiter_start =
                                indentation.colon.span.end.offset;

                            make_token_relative(
                                mode,
                                offset,
                                offset_to_branch,
                                &mut indentation.colon,
                            );

                            make_token_relative(
                                OffsetMode::Start,
                                delimiter_start,
                                *branch_id,
                                &mut indentation.new_line,
                            );
                        }
                    }

                    mode = OffsetMode::End;
                    offset_to_branch = *branch_id;
                    last_was_branch = true;
                }
            }
        }
    }
}

#[cfg(test)]
mod test;
