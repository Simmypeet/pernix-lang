//! Contains the definition of [`FromNode`] trait

use pernixc_lexical::{token, tree::RelativeSpan};

use crate::concrete_tree;

/// Allows conversion from [`concrete_tree::Node`] to a specific type.
pub trait FromNode: Sized {
    /// Converts the given [`concrete_tree::Node`] to the type implementing
    /// this trait.
    fn from_node(node: &concrete_tree::Node) -> Option<Self>;
}

impl FromNode for token::Kind<RelativeSpan> {
    fn from_node(node: &concrete_tree::Node) -> Option<Self> {
        node.as_leaf().cloned()
    }
}
