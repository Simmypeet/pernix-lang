//! Contains the definition of the [`Tree`] and [`Node`] representing the
//! concrete tree which contains the full source code fidelity.

use std::{any::TypeId, sync::Arc};

use enum_as_inner::EnumAsInner;
use pernixc_arena::ID;
use pernixc_lexical::{token, tree::RelativeLocation};

/// An enumeration of the different types of nodes that can be found in
/// the concrete syntax tree.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(clippy::large_enum_variant)]
pub enum Node {
    /// Terminal/leaf node representing a singular token.
    Leaf(token::Kind<RelativeLocation>), /* TODO: optimize this variant to
                                          * smaller size */

    /// Another tree node that is a child of this node.
    Branch(Arc<Tree>),

    /// A fragment of the tree that isn't stepped into.
    SkipFragment(ID<pernixc_lexical::tree::Branch>),
}

/// The information used by the [`Tree`] to determine which AST created
/// that particular tree node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AstInfo {
    /// The [`TypeId`] that implements the AST trait for this node. This type
    /// ID is primarily used to cast the concrete [`Tree`] to the
    /// [`crate::abstract_tree::AbstractTree`].
    pub ast_type_id: TypeId,

    /// The id of the branch that this node steps into before start parsing.
    pub step_into_fragment: Option<ID<pernixc_lexical::tree::Branch>>,
}

/// A typeless concrete syntax tree built by the parser.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tree {
    /// The info of where which AST created this tree.
    ///
    /// If [`None`] this tree node is an error node where the tokens are
    /// skipped.
    pub ast_info: Option<AstInfo>,

    /// List of nodes this tree contains.
    pub nodes: Vec<Node>,
}
