use std::sync::Arc;

use pernixc_lexical::{
    token,
    tree::{RelativeLocation, RelativeSpan},
};

use crate::{concrete_tree, from_node::FromNode};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct One;

impl Extract for One {
    type Result<'nodes, F> = Option<F>;

    fn extract<F: FromNode>(
        nodes: &[concrete_tree::Node],
    ) -> Self::Result<'_, F> {
        nodes.iter().find_map(F::from_node)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Multiple;

impl Extract for Multiple {
    type Result<'nodes, F> = std::iter::FilterMap<
        std::slice::Iter<'nodes, concrete_tree::Node>,
        fn(&'nodes concrete_tree::Node) -> Option<F>,
    >;

    fn extract<F: FromNode>(
        nodes: &[concrete_tree::Node],
    ) -> Self::Result<'_, F> {
        nodes.iter().filter_map(F::from_node)
    }
}

pub trait Extract {
    type Result<'nodes, F>;

    fn extract<F: FromNode>(
        nodes: &[concrete_tree::Node],
    ) -> Self::Result<'_, F>;
}

pub trait Verify {
    type Extract: Extract;
    type Output: FromNode;
}
