//! Responsible for structuring the stream of tokens into a tree-like structure.

use std::iter::Peekable;

use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use pernixc_arena::{Arena, ID};
use pernixc_handler::Handler;
use pernixc_source_file::{ByteIndex, Span};
use serde::Serialize;
use strum_macros::EnumIter;

use crate::{
    error, kind,
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

    /// The new line character or line comment that follows the colon.
    ///
    /// The colon can be followed by a line comment as well since the line
    /// comment is also ended by a new line character.
    pub new_line: NewLine<RelativeSpan>,

    /// The token that marks an end of the indentation group.
    ///
    /// This can either be the last new line/line comment or the closing
    /// delimiter if the indentation group is enclosed by a pair of delimiters.
    /// Alternatively, it can be `None` if the indentation group appears last
    /// in the file (EOF).
    pub ending_token: Option<Kind<RelativeSpan>>,
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
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

/// Represents the token tree where each of the branches are the fragments
/// in the token stream.
///
/// This is useful for easy traversal of the tree and for incremental
/// compilation compatibility.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Getters, CopyGetters)]
pub struct Tree {
    /// The collection of branches in the token tree.
    #[get = "pub"]
    branches: Arena<Branch>,

    /// The ID of the root branch in the token tree.
    #[get_copy = "pub"]
    root_id: ID<Branch>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct DelimiterMarker {
    open: Punctuation<RelativeSpan>,
    location: RelativeLocation,
}

struct Converter<'a, 'h> {
    tokenizer: Peekable<Tokenizer<'a, 'h>>,
    handler: &'h dyn Handler<error::Error>,
    delimiter_stack: Vec<DelimiterMarker>,

    current_nodes: Vec<Node>,
    tree: Tree,
}

impl Converter<'_, '_> {
    fn forward(&mut self) -> bool {
        if let Some(token) = self.tokenizer.next() {
            match token {
                left @ Token {
                    kind: kind::Kind::Punctuation('{' | '(' | '['),
                    ..
                } => {}
            }
        }

        false
    }
}
