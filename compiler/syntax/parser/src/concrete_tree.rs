//! Contains the definition of the [`Tree`] and [`Node`] representing the
//! concrete tree which contains the full source code fidelity.

use std::sync::Arc;

use enum_as_inner::EnumAsInner;
use pernixc_arena::ID;
use pernixc_lexical::{
    token,
    tree::{OffsetMode, RelativeLocation, RelativeSpan},
};
use pernixc_source_file::{GlobalSourceID, SourceElement, Span};
use qbice::{Decode, Encode, StableHash, stable_type_id::StableTypeID};
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::abstract_tree::AbstractTree;

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
    Encode,
    Decode,
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
    /// Retrieves the token that contains the given byte index, if any.
    ///
    /// # Parameters
    ///
    /// - `token_tree`: The token tree that this concrete tree was built from.
    /// - `byte_index`: The byte index to look for.
    #[must_use]
    pub fn get_pointing_token(
        mut self: &Self,
        token_tree: &pernixc_lexical::tree::Tree,
        byte_index: pernixc_source_file::ByteIndex,
    ) -> Option<token::Kind<RelativeLocation>> {
        let span = self.span();
        let abs_span = token_tree.absolute_span_of(&span);
        if !abs_span.range().contains(&byte_index) {
            return None;
        }

        'recurse: loop {
            match self {
                Self::Leaf(token) => {
                    return Some(token.clone());
                }

                Self::Branch(tree) => {
                    // binary search for the child node that contains the byte
                    // index, then we test that node.
                    //
                    // if the byte index is in node, we continue recursing into
                    // that node.
                    let mut left = 0;
                    let mut right = tree.nodes.len();

                    while left < right {
                        let mid = usize::midpoint(left, right);
                        let node = &tree.nodes[mid];

                        let span = node.span();
                        let abs_span = token_tree.absolute_span_of(&span);

                        // found the node, continue recursing into this node
                        if abs_span.range().contains(&byte_index) {
                            self = node;
                            continue 'recurse;
                        } else if byte_index < abs_span.start {
                            right = mid;
                        } else {
                            left = mid + 1;
                        }
                    }

                    // no child node contains the byte index, return None
                    return None;
                }
                Self::SkipFragment(_, _) => {
                    return None;
                }
            }
        }
    }

    /// Retrieves the deepest AST of type `T` that contains the given byte
    /// index.
    #[must_use]
    pub fn get_deepest_ast<T: AbstractTree>(
        mut self: &Self,
        token_tree: &pernixc_lexical::tree::Tree,
        byte_index: pernixc_source_file::ByteIndex,
    ) -> Option<T> {
        // check if self contains the byte index
        let span = self.span();
        let abs_span = token_tree.absolute_span_of(&span);
        if !abs_span.range().contains(&byte_index) {
            return None;
        }

        // attempt to cast to AST T
        let mut current_node = T::from_node(self);

        'recurse: loop {
            // check if self contains the byte index
            match self {
                // no more to recurse into
                Self::SkipFragment(_, _) | Self::Leaf(_) => {
                    return current_node;
                }

                Self::Branch(tree) => {
                    // binary search for the child node that contains the byte
                    // index, then we test that node.
                    //
                    // if the byte index is in node, we attempt to update the
                    // deepest AST found so far, and continue recursing into
                    // that node.
                    let mut left = 0;
                    let mut right = tree.nodes.len();

                    while left < right {
                        let mid = usize::midpoint(left, right);
                        let node = &tree.nodes[mid];

                        let span = node.span();
                        let abs_span = token_tree.absolute_span_of(&span);

                        // found the node, attempt to cast to T
                        if abs_span.range().contains(&byte_index) {
                            if let Some(casted) = T::from_node(node) {
                                current_node = Some(casted);
                            }

                            // continue recursing into this node
                            self = node;
                            continue 'recurse;
                        } else if byte_index < abs_span.start {
                            right = mid;
                        } else {
                            left = mid + 1;
                        }
                    }

                    // no child node contains the byte index, return current
                    // found AST
                    return current_node;
                }
            }
        }
    }
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
    Encode,
    Decode,
    StableHash,
)]
#[allow(missing_copy_implementations)]
pub struct AstInfo {
    /// The [`StableTypeID`] that implements the AST trait for this node. This
    /// type ID is primarily used to cast the concrete [`Tree`] to the
    /// [`crate::abstract_tree::AbstractTree`].
    pub ast_type_id: StableTypeID,

    /// The name of the AST that created this tree node.
    #[cfg(debug_assertions)]
    pub ast_name: String,

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
    Encode,
    Decode,
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

impl Tree {}

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
