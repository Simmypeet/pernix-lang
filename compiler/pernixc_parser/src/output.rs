//! Contains utility traits related to extracting output from the nodes inside
//! the tree.

use crate::{concrete_tree, from_node::FromNode};

/// Extracts a single matching node from the tree by returning an
/// [`Some`] on the first matching element, [`None`] if no matching
/// element is found.
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

/// Extracts multiple matching nodes from the tree by returning an iterator
/// of matching elements.
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

/// A trait implemented by [`Multiple`] and [`One`] to extract nodes from the
/// tree.
pub trait Extract {
    /// The result of the extraction element from the tree.
    ///
    /// For example, it can be an iterator of nodes or a single node.
    type Result<'nodes, F>;

    /// Extracts the nodes from the tree.
    fn extract<F: FromNode>(
        nodes: &[concrete_tree::Node],
    ) -> Self::Result<'_, F>;
}

/// A utility trait implemented by some parsers used to specify the output 
/// type of the parser.
pub trait Verify {
    /// Determines how to extract the output from the tree.
    type Extract: Extract;

    /// The output type of the parser.
    type Output: FromNode;
}
