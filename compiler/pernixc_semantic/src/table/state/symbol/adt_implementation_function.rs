use pernixc_syntax::syntax_tree;

use super::Symbol;
use crate::symbol::AdtImplementationFunction;

impl Symbol for AdtImplementationFunction {
    type SyntaxTree = syntax_tree::item::ImplementationFunction;
    type Flag = ();
    type Data = ();
}