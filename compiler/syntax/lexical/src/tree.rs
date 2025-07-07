//! Responsible for structuring the stream of tokens into a tree-like structure.

use std::{
    hash::{Hash, Hasher},
    ops::Range,
};

use derive_more::Deref;
use enum_as_inner::EnumAsInner;
use fnv::FnvHasher;
use getset::CopyGetters;
use pernixc_arena::{Arena, ID};
use pernixc_handler::Handler;
use pernixc_hash::DashMap;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::{
    AbsoluteSpan, ByteIndex, GlobalSourceID, Location, SourceFile, Span,
};
use pernixc_stable_hash::StableHash;
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
    Deserialize,
    StableHash,
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
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    EnumAsInner,
    StableHash,
)]
pub enum FragmentKind {
    /// The token nodes are enclosed by a pair of delimiters.
    Delimiter(Delimiter),

    /// The token nodes are grouped by having the same indentation level.
    Indentation(Indentation),
}

/// Represents a delimiter pair such as `( ... )`, `{ ... }`, or `[ ... ]`
/// delimiters.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
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
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
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
    StableHash,
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
    StableHash,
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
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    EnumAsInner,
    StableHash,
)]
#[allow(missing_docs)]
pub enum Node {
    Leaf(Kind<RelativeLocation>),
    Branch(ID<Branch>),
}

/// Represents a branch that has a [`FragmentKind`] as its kind.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct FragmentBranch {
    /// The kind of fragment this branch represents.
    pub fragment_kind: FragmentKind,

    /// The starting location of this branch in the source code.
    pub starting_location: RelativeLocation,

    /// The ID to the parent branch of this fragment.
    pub parent: ID<Branch>,
}

/// Represents the kind of branch in the token tree.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    EnumAsInner,
    StableHash,
)]
#[allow(missing_docs, clippy::large_enum_variant)]
pub enum BranchKind {
    Fragment(FragmentBranch),

    /// The root branch of the token tree.
    Root,
}

/// Represents a branch node in the token tree.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct Branch {
    /// The kind of branch this is.
    pub kind: BranchKind,

    /// The collection of nodes that are children of this branch.
    pub nodes: Vec<Node>,
}

impl Branch {
    /// Gets the ID of the parent branch of this branch.
    #[must_use]
    pub const fn parent(&self) -> Option<ID<Self>> {
        match &self.kind {
            BranchKind::Fragment(fragment) => Some(fragment.parent),
            BranchKind::Root => None,
        }
    }
}

/// The constant ID that every [`Tree`] will use as the root branch ID.
pub const ROOT_BRANCH_ID: ID<Branch> = ID::new(0);

/// Represents the token tree where each of the branches are the fragments
/// in the token stream.
///
/// This is useful for easy traversal of the tree and for incremental
/// compilation compatibility.
#[derive(Debug, Clone, Deref, Serialize, Deserialize, CopyGetters)]
pub struct Tree {
    #[deref]
    arena: Arena<Branch>,

    /// The source ID of the source code that this tree is built from.
    #[get_copy = "pub"]
    source_id: GlobalSourceID,

    #[serde(skip)]
    end_location_cache: DashMap<ID<Branch>, ByteIndex>,
}

impl StableHash for Tree {
    fn stable_hash<H: pernixc_stable_hash::StableHasher + ?Sized>(
        &self,
        state: &mut H,
    ) {
        self.source_id.stable_hash(state);
        self.arena.stable_hash(state);
    }
}

impl PartialEq for Tree {
    fn eq(&self, other: &Self) -> bool {
        self.source_id == other.source_id && self.arena == other.arena
    }
}

impl Eq for Tree {}

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
            tree: Self {
                source_id,
                arena: Arena::new(),
                end_location_cache: DashMap::default(),
            },
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
            &mut converter.tree.arena,
            ROOT_BRANCH_ID,
            0,
            &mut converter.current_nodes,
            None,
        );

        converter
            .tree
            .arena
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
        let branch = &self[branch_id];
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
    end_cache: Option<&DashMap<ID<Branch>, ByteIndex>>,
) -> ByteIndex {
    loop {
        if let Some(end_cache) = end_cache {
            if let Some(end) = end_cache.get(&id) {
                return *end;
            }
        }

        let branch = &branches[id];
        let result = match &branch.kind {
            BranchKind::Fragment(fragment_branch) => {
                match &fragment_branch.fragment_kind {
                    FragmentKind::Delimiter(delimiter) => absolute_location_of(
                        branches,
                        &delimiter.close.span.end,
                        end_cache,
                    ),

                    FragmentKind::Indentation(indentation) => {
                        if let Some(x) = branch.nodes.last() {
                            match x {
                                Node::Leaf(leaf) => absolute_location_of(
                                    branches,
                                    &leaf.span.end,
                                    end_cache,
                                ),
                                Node::Branch(new_id) => {
                                    id = *new_id;
                                    continue;
                                }
                            }
                        } else {
                            absolute_location_of(
                                branches,
                                &indentation.new_line.span.end,
                                end_cache,
                            )
                        }
                    }
                }
            }

            BranchKind::Root => match branch.nodes.last() {
                Some(node) => match node {
                    Node::Leaf(leaf) => absolute_location_of(
                        branches,
                        &leaf.span.end,
                        end_cache,
                    ),

                    Node::Branch(new_id) => {
                        id = *new_id;
                        continue;
                    }
                },
                None => 0,
            },
        };

        if let Some(end_cache) = end_cache {
            end_cache.insert(id, result);
        }

        return result;
    }
}

fn absolute_start_byte_of(
    branches: &Arena<Branch>,
    id: ID<Branch>,
    end_cache: Option<&DashMap<ID<Branch>, ByteIndex>>,
) -> ByteIndex {
    match &branches[id].kind {
        BranchKind::Fragment(fragment) => absolute_location_of(
            branches,
            &fragment.starting_location,
            end_cache,
        ),
        BranchKind::Root => 0,
    }
}

fn absolute_location_of(
    branches: &Arena<Branch>,
    relative_location: &RelativeLocation,
    end_cache: Option<&DashMap<ID<Branch>, ByteIndex>>,
) -> ByteIndex {
    let offset = match relative_location.mode {
        OffsetMode::Start => absolute_start_byte_of(
            branches,
            relative_location.relative_to,
            end_cache,
        ),
        OffsetMode::End => absolute_end_byte_of(
            branches,
            relative_location.relative_to,
            end_cache,
        ),
    };

    relative_location.offset + offset
}

impl Tree {
    /// Gets the absolute start byte of the given branch ID.
    #[must_use]
    pub fn absolute_start_byte_of(&self, id: ID<Branch>) -> ByteIndex {
        if id == ROOT_BRANCH_ID {
            return 0;
        }

        absolute_start_byte_of(&self.arena, id, Some(&self.end_location_cache))
    }

    /// Gets the absolute end byte of the given branch ID.
    #[must_use]
    pub fn absoluate_end_byte_of(&self, id: ID<Branch>) -> ByteIndex {
        absolute_end_byte_of(&self.arena, id, Some(&self.end_location_cache))
    }

    /// Calculates the absolute span of the given relative span.
    #[must_use]
    pub fn absolute_span_of(
        &self,
        relative_span: &RelativeSpan,
    ) -> AbsoluteSpan {
        let start = self.absolute_location_of(&relative_span.start);
        let end = self.absolute_location_of(&relative_span.end);

        AbsoluteSpan { start, end, source_id: relative_span.source_id }
    }

    /// Calculates the absolute location of the given relative location.
    #[must_use]
    pub fn absolute_location_of(
        &self,
        relative_location: &RelativeLocation,
    ) -> ByteIndex {
        let offset = match relative_location.mode {
            OffsetMode::Start => {
                self.absolute_start_byte_of(relative_location.relative_to)
            }
            OffsetMode::End => {
                self.absoluate_end_byte_of(relative_location.relative_to)
            }
        };

        relative_location.offset + offset
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

            let mut fragment_kind = FragmentKind::Delimiter(Delimiter {
                open: opening_delimiter,
                close: closing_delimiter,
                delimiter: expected_delimiter,
            });

            Self::make_branch_relative(
                &mut self.tree.arena,
                branch_id,
                opening_delimiter.start_location().offset,
                &mut nodes,
                Some(&mut fragment_kind),
            );

            self.tree
                .arena
                .insert_with_id(branch_id, Branch {
                    kind: BranchKind::Fragment(FragmentBranch {
                        fragment_kind,
                        starting_location: RelativeLocation {
                            offset: opening_delimiter.start_location().offset,
                            mode: OffsetMode::Start,
                            relative_to: ROOT_BRANCH_ID,
                        },
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

            let mut fragment_kind = FragmentKind::Indentation(Indentation {
                indentation_size: marker.indentation_size.unwrap(),
                colon,
                new_line,
            });
            Self::make_branch_relative(
                &mut self.tree.arena,
                branch_hash,
                colon.start_location().offset,
                &mut nodes,
                Some(&mut fragment_kind),
            );

            let branch = Branch {
                kind: BranchKind::Fragment(FragmentBranch {
                    fragment_kind,
                    starting_location: RelativeLocation {
                        offset: colon.start_location().offset,
                        mode: OffsetMode::Start,
                        relative_to: ROOT_BRANCH_ID,
                    },
                    parent: ROOT_BRANCH_ID,
                }),
                nodes,
            };

            // insert the branch
            self.tree.arena.insert_with_id(branch_hash, branch).unwrap();

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
            starting_location: RelativeLocation {
                offset: colon.start_location().offset,
                mode: OffsetMode::Start,
                relative_to: ROOT_BRANCH_ID,
            },
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
        self.tree.arena.insert_with_id(branch_id, branch).unwrap();
        self.current_nodes
            .insert(indentation_marker.colon_index, Node::Branch(branch_id));
    }

    const fn make_span_relative(
        span: &mut RelativeSpan,
        offset: usize,
        mode: OffsetMode,
        offset_to_branch: ID<Branch>,
    ) {
        span.start.offset -= offset;
        span.start.mode = mode;
        span.start.relative_to = offset_to_branch;

        span.end.offset -= offset;
        span.end.mode = mode;
        span.end.relative_to = offset_to_branch;
    }

    const fn make_token_relative<T>(
        mode: OffsetMode,
        offset: usize,
        offset_to_branch: ID<Branch>,
        token: &mut Token<T, RelativeLocation>,
    ) {
        if let Some(prior) = &mut token.prior_insignificant {
            Self::make_span_relative(prior, offset, mode, offset_to_branch);
        }

        Self::make_span_relative(
            &mut token.span,
            offset,
            mode,
            offset_to_branch,
        );
    }

    #[allow(clippy::too_many_lines, clippy::needless_pass_by_value)]
    fn make_branch_relative(
        arena: &mut Arena<Branch>,
        parent_branch_id: ID<Branch>,
        mut offset: ByteIndex,
        nodes: &mut [Node],
        fragmnet_kind: Option<&mut FragmentKind>,
    ) {
        let mut mode = OffsetMode::Start;
        let mut last_was_branch = false;
        let mut offset_to_branch = parent_branch_id;

        for node in nodes.iter_mut() {
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
                    Self::make_token_relative(
                        mode,
                        offset,
                        offset_to_branch,
                        token,
                    );

                    last_was_branch = false;
                }

                Node::Branch(branch_id) => {
                    let branch = &mut arena[*branch_id];
                    let fragment = branch.kind.as_fragment_mut().unwrap();

                    // correct the parent branch
                    fragment.parent = parent_branch_id;

                    fragment.starting_location.offset -= offset;
                    fragment.starting_location.mode = mode;
                    fragment.starting_location.relative_to = offset_to_branch;

                    match &mut fragment.fragment_kind {
                        FragmentKind::Delimiter(delimiter) => {
                            let delimiter_start =
                                delimiter.open.start_location().offset;

                            assert_eq!(
                                delimiter.open.start_location().relative_to,
                                ROOT_BRANCH_ID
                            );

                            Self::make_token_relative(
                                OffsetMode::Start,
                                delimiter_start,
                                *branch_id,
                                &mut delimiter.open,
                            );
                        }
                        FragmentKind::Indentation(indentation) => {
                            let indentation_start =
                                indentation.colon.start_location().offset;

                            assert_eq!(
                                indentation.colon.start_location().relative_to,
                                ROOT_BRANCH_ID
                            );

                            Self::make_token_relative(
                                OffsetMode::Start,
                                indentation_start,
                                *branch_id,
                                &mut indentation.colon,
                            );

                            Self::make_token_relative(
                                OffsetMode::Start,
                                indentation_start,
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

        if let Some(FragmentKind::Delimiter(delimiter)) = fragmnet_kind {
            if last_was_branch {
                offset = delimiter.close.start_location().offset;
            }

            Self::make_token_relative(
                mode,
                offset,
                offset_to_branch,
                &mut delimiter.close,
            );
        }
    }
}

#[cfg(test)]
mod test;
