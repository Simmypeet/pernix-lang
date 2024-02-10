use pernixc_syntax::syntax_tree;

use super::Symbol;
use crate::symbol::AdtImplementationType;

impl Symbol for AdtImplementationType {
    type SyntaxTree = syntax_tree::item::ImplementationFunction;
    type Flag = ();
    type Data = ();
}
