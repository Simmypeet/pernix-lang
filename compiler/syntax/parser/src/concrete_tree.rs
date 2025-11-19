//! Contains the definition of the [`Tree`] and [`Node`] representing the
//! concrete tree which contains the full source code fidelity.

use std::sync::Arc;

use enum_as_inner::EnumAsInner;
#[cfg(debug_assertions)]
use flexstr::SharedStr;
use pernixc_arena::ID;
use pernixc_lexical::{
    token,
    tree::{OffsetMode, RelativeLocation, RelativeSpan},
};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::{GlobalSourceID, SourceElement, Span};
use pernixc_stable_hash::StableHash;
use pernixc_stable_type_id::StableTypeID;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

/// An enumeration of the different types of nodes that can be found in
/// the concrete syntax tree.
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
#[allow(clippy::large_enum_variant)]
pub enum Node {
    /// Terminal/leaf node representing a singular token.
    Leaf(token::Kind<RelativeLocation>), /* TODO: optimize this variant to
                                          * smaller size */

    /// Another tree node that is a child of this node.
    Branch(Arc<Tree>),

    /// A fragment of the tree that isn't stepped into.
    SkipFragment(ID<pernixc_lexical::tree::Branch>, GlobalSourceID),
}

impl Node {
    /// Retrieves the [`RelativeSpan`] of the node.
    #[must_use]
    pub fn span(&self) -> RelativeSpan {
        match self {
            Self::Leaf(token) => token.span,
            Self::Branch(tree) => tree.span(),
            Self::SkipFragment(id, source_id) => RelativeSpan {
                start: RelativeLocation {
                    offset: 0,
                    mode: OffsetMode::Start,
                    relative_to: *id,
                },
                end: RelativeLocation {
                    offset: 0,
                    mode: OffsetMode::End,
                    relative_to: *id,
                },
                source_id: *source_id,
            },
        }
    }
}

/// The information used by the [`Tree`] to determine which AST created
/// that particular tree node.
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
#[allow(missing_copy_implementations)]
pub struct AstInfo {
    /// The [`TypeId`] that implements the AST trait for this node. This type
    /// ID is primarily used to cast the concrete [`Tree`] to the
    /// [`crate::abstract_tree::AbstractTree`].
    pub ast_type_id: StableTypeID,

    /// The name of the AST that created this tree node.
    #[cfg(debug_assertions)]
    pub ast_name: SharedStr,

    /// The id of the branch that this node steps into before start parsing.
    pub step_into_fragment:
        Option<(ID<pernixc_lexical::tree::Branch>, GlobalSourceID)>,
}

/// A typeless concrete syntax tree built by the parser.
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
pub struct Tree {
    /// The info of where which AST created this tree.
    ///
    /// If [`None`] this tree node is an error node where the tokens are
    /// skipped.
    pub ast_info: Option<AstInfo>,

    /// List of nodes this tree contains.
    ///
    /// # Invariants
    ///
    /// The [`Self::nodes`] must not be empty
    pub nodes: Vec<Node>,
}

impl Drop for Tree {
    fn drop(&mut self) {
        std::mem::take(&mut self.nodes).into_par_iter().for_each(drop);
    }
}

impl Tree {
    /// Retrieves the token that contains the given byte index, if any.
    #[must_use]
    pub fn get_pointing_token(
        &self,
        token_tree: &pernixc_lexical::tree::Tree,
        byte_index: pernixc_source_file::ByteIndex,
    ) -> Option<token::Kind<RelativeLocation>> {
        // binary search for the token that contains the byte index
        let mut left = 0;
        let mut right = self.nodes.len();

        while left < right {
            let mid = usize::midpoint(left, right);
            let node = &self.nodes[mid];

            let span = node.span();
            let abs_span = token_tree.absolute_span_of(&span);

            if abs_span.range().contains(&byte_index) {
                // found the token
                return match node {
                    Node::Leaf(token) => Some(token.clone()),
                    Node::Branch(tree) => {
                        // recurse into the branch
                        tree.get_pointing_token(token_tree, byte_index)
                    }
                    Node::SkipFragment(_, _) => None,
                };
            } else if byte_index < abs_span.start {
                right = mid;
            } else {
                left = mid + 1;
            }
        }

        None
    }
}

impl SourceElement for Tree {
    type Location = RelativeLocation;

    fn span(&self) -> Span<Self::Location> {
        if let Some(step_into_fragment) =
            self.ast_info.as_ref().and_then(|x| x.step_into_fragment)
        {
            return Span {
                start: RelativeLocation {
                    offset: 0,
                    mode: OffsetMode::Start,
                    relative_to: step_into_fragment.0,
                },
                end: RelativeLocation {
                    offset: 0,
                    mode: OffsetMode::End,
                    relative_to: step_into_fragment.0,
                },
                source_id: step_into_fragment.1,
            };
        }

        let first = self.nodes.first().unwrap();
        let last = self.nodes.last().unwrap();

        let first_span = first.span();
        let last_span = last.span();

        Span {
            start: first_span.start,
            end: last_span.end,
            source_id: first_span.source_id,
        }
    }
}
