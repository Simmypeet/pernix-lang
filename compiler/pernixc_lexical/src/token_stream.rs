//! Contains the [`TokenStream`] struct and its related types.

use std::{collections::HashMap, ops::Index, sync::Arc};

use derive_more::{Deref, From};
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_handler::Handler;
use pernixc_source_file::SourceFile;
use proptest::bits::usize;

use crate::{
    error::{self, InvalidIndentation, UndelimitedDelimiter},
    token::{self, Comment, CommentKind, Kind, Punctuation},
};

pub mod strategy;

/// The represents how the [`TokenStream`] is structured in a fragment.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum FragmentKind {
    /// The [`TokenStream`] is enclosed by a pair of delimiters.
    Delimiter(Delimiter),

    /// The [`TokenStream`] is grouped by having the same indentation level.
    Indentation(Indentation),
}

/// Represents a delimiter pair such as `( ... )`, `{ ... }`, or `[ ... ]`
/// delimiters.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Delimiter {
    /// The opening delimiter.
    pub open: Punctuation,

    /// The closing delimiter.
    pub close: Punctuation,

    /// The type of delimiter.
    pub delimiter: DelimiterKind,
}

/// The token stream is grouped by having the same indentation level. The
/// indentation group is started by a colon followed by a newline character.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Indentation {
    /// The indentation level of the group (in space or tab characters count,
    /// not how many levels deep).
    pub indentation_size: usize,

    /// The colon character signifying the start of the indentation group.
    pub colon: Punctuation,

    /// List of insignificant tokens that appear before the
    /// [`Self::new_line_or_line_comment`].
    ///
    /// For example, `: /*comment*/ \n` would have `:` as the colon,
    /// `/*comment*/` as the `prior_new_line_tokens`, and `\n` as the
    /// `new_line_or_line_comment`.
    pub prior_new_line_tokens: Vec<Kind>,

    /// The new line character or line comment that follows the colon.
    ///
    /// The colon can be followed by a line comment as well since the line
    /// comment is also ended by a new line character.
    pub new_line_or_line_comment: Kind,

    /// The token that marks an end of the indentation group.
    ///
    /// This can either be the last new line/line comment or the closing
    /// delimiter if the indentation group is enclosed by a pair of delimiters.
    /// Alternatively, it can be `None` if the indentation group appears last
    /// in the file (EOF).
    pub ending_token: Option<Kind>,
}

/// Is an enumeration of the different types of delimiters in the [`Delimiter`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum DelimiterKind {
    Parenthesis,
    Brace,
    Bracket,
}

/// Represents a list of tokens enclosed by a pair of delimiters.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Fragment {
    /// The kind of this fragment.
    pub kind: FragmentKind,

    /// The stream of tokens inside the delimiter.
    pub token_stream: TokenStream,
}

/// Is an enumeration of either a [`Token`] or a [`Delimited`].
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From,
)]
#[allow(missing_docs)]
pub enum TokenKind {
    Token(Kind),
    Fragment(Fragment),
}

/// Is a list of well structured tokens in a tree-like structure.
///
/// The [`TokenStream`] consists of a list of [`TokenKind`]s. It's structured
/// similarly to a tree where [`TokenKind::Token`] represents a leaf node and
/// [`TokenKind::Fragment`] represents a branch node.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Deref, Getters,
)]
pub struct TokenStream {
    #[deref]
    tokens: Vec<TokenKind>,

    /// The source file used to generate the tokens.
    #[get = "pub"]
    source_file: Arc<SourceFile>,
}

impl Index<usize> for TokenStream {
    type Output = TokenKind;

    fn index(&self, index: usize) -> &Self::Output { &self.tokens[index] }
}

/// Represents a kind of node in the tree; either the root or a child fragment
/// node.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
pub enum NodeKind<'a> {
    /// The root of the tree. Always has the index `0`.
    Root(&'a TokenStream),

    /// The child node, which is a fragment node.
    Fragment {
        /// The fragment node
        fragment: &'a Fragment,

        /// The index of the parent node.
        parent: usize,
    },
}

impl<'a> NodeKind<'a> {
    /// Gets the index of the parent node. Returns `None` if the node is the
    /// root node.
    #[must_use]
    pub const fn get_parent(&self) -> Option<usize> {
        match &self {
            NodeKind::Root(_) => None,
            NodeKind::Fragment { parent, .. } => Some(*parent),
        }
    }

    /// Gets the token stream of the node.
    #[must_use]
    pub const fn token_stream(&self) -> &'a TokenStream {
        match &self {
            NodeKind::Root(token_stream) => token_stream,
            NodeKind::Fragment { fragment: delimited, .. } => {
                &delimited.token_stream
            }
        }
    }
}

/// Represents a location to a particular token in the token stream [`Tree`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location {
    /// The index of the node in the [`Tree`].
    pub node_index: usize,

    /// The index of the token in the token stream.
    pub token_index: usize,
}

impl Location {
    /// Constructs a new location.
    #[must_use]
    pub const fn new(node_index: usize, token_index: usize) -> Self {
        Self { node_index, token_index }
    }
}

/// Represents a node in the tree.
#[derive(Debug, Clone, PartialEq, Eq, derive_more::Deref)]
pub struct Node<'a> {
    /// The kind of node.
    #[deref]
    pub kind: NodeKind<'a>,

    /// Maps the index of the token in this node [`TokenStream`] to the index
    /// of the child node in the [`Tree`].
    pub child_ids_by_token_index: HashMap<usize, usize>,
}

/// Represents a tree of [`TokenStream`]s. This allows easier traversal of the
/// [`TokenStream`] tree.
///
/// The tree is stored in pre-order traversal. The first node is always the
/// root node.
///
/// The node is uniquely identified by its index in the tree. The indices used
/// in identifying the nodes are assumed to be always valid when used in the
/// various methods of this struct; therefore, it is the responsibility of the
/// caller to ensure that the indices are valid or else a panic will occur.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tree<'a> {
    /// The nodes are stored in pre-order traversal. The first node is always
    /// the root node.
    nodes: Vec<Node<'a>>,
}

impl<'a> Tree<'a> {
    /// Constructs a new tree from the given [`TokenStream`] as the root node.
    #[must_use]
    pub fn new(token_stream: &'a TokenStream) -> Self {
        let mut result = Self { nodes: Vec::new() };

        result.nodes.push(Node {
            kind: NodeKind::Root(token_stream),
            child_ids_by_token_index: HashMap::new(),
        });

        result.add(token_stream, 0);

        result
    }

    fn add(&mut self, token_stream: &'a TokenStream, node_index: usize) {
        for (tok_index, tree) in token_stream.iter().enumerate() {
            let TokenKind::Fragment(delimited) = tree else {
                continue;
            };

            let child_index = self.nodes.len();
            self.nodes.push(Node {
                kind: NodeKind::Fragment {
                    fragment: delimited,
                    parent: node_index,
                },
                child_ids_by_token_index: HashMap::new(),
            });

            assert!(self.nodes[node_index]
                .child_ids_by_token_index
                .insert(tok_index, child_index)
                .is_none());

            self.add(&delimited.token_stream, child_index);
        }
    }

    /// Returns the node at the given index.
    ///
    /// # Returns
    ///
    /// The node at the given index. [`None`] if the index is out of bounds.
    #[must_use]
    pub fn get_node(&self, index: usize) -> Option<&Node<'a>> {
        self.nodes.get(index)
    }

    /// Returns the number of nodes in the tree.
    #[must_use]
    pub fn len(&self) -> usize { self.nodes.len() }

    /// Returns `true` if the tree is empty.
    #[must_use]
    pub fn is_empty(&self) -> bool { self.nodes.is_empty() }

    /// Returns the root node of the tree.
    #[must_use]
    pub fn root_node(&self) -> &Node<'a> { &self.nodes[0] }

    /// Returns the [`TokenStream`] of the root node.
    #[must_use]
    pub fn root_token_stream(&self) -> &'a TokenStream {
        self.root_node().kind.token_stream()
    }

    /// Returns the location of the given token in the tree.
    ///
    /// # Returns
    ///
    /// The location of the token in the tree. [`None`] if the
    /// [`Location::token_index`] is out of bounds or if the
    /// [`Location::node_index`] is out of bounds.
    #[must_use]
    pub fn get_token(&self, location: &Location) -> Option<&'a TokenKind> {
        let node = self.get_node(location.node_index)?;
        let token_stream = node.kind.token_stream();

        token_stream.get(location.token_index)
    }
}

#[cfg(test)]
pub(crate) mod test;
