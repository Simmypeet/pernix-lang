use pernixc_lexical::{token, tree::RelativeSpan};

use crate::concrete_tree;

pub trait FromNode: Sized {
    fn from_node(node: &concrete_tree::Node) -> Option<Self>;
}

impl FromNode for token::Kind<RelativeSpan> {
    fn from_node(node: &concrete_tree::Node) -> Option<Self> {
        node.as_leaf().cloned()
    }
}
