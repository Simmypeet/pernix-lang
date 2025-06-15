//! Contains utility traits related to extracting output from the nodes inside
//! the tree.

use crate::concrete_tree;

/// Extracts a single matching node from the tree by returning an
/// [`Some`] on the first matching element, [`None`] if no matching
/// element is found.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct One;

/// Extracts multiple matching nodes from the tree by returning an iterator
/// of matching elements.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Multiple;

/// A utility trait implemented by some parsers used to extract the output
/// from the syntax tree node after finishing the parsing process.
pub trait Output {
    /// The kind of extraction to be used.
    type Extract;

    /// The result of the extraction.
    type Output<'a>;

    /// Extracts the output from the concrete tree node.
    fn output<'a>(
        &self,
        node: &'a concrete_tree::Node,
    ) -> Option<Self::Output<'a>>;
}

/// Extracts a first single matching node from the tree by returning an
/// [`Option`] on the first matching element, [`None`] if no matching
/// element is found.
#[inline]
pub fn extract_one<O: Output<Extract = One>>(
    extract_parser: O,
    node: &[concrete_tree::Node],
) -> Option<O::Output<'_>> {
    node.iter().find_map(move |node| extract_parser.output(node))
}

/// Extracts multiple matching nodes from the tree by returning an iterator
/// of matching elements.
#[inline]
pub fn extract_multiple<O: Output<Extract = Multiple>>(
    extract_parser: O,
    node: &[concrete_tree::Node],
) -> impl Iterator<Item = O::Output<'_>> {
    node.iter().filter_map(move |node| extract_parser.output(node))
}
