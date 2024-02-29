use pernixc_syntax::syntax_tree;

use super::Symbol;
use crate::symbol::AdtImplementation;

impl Symbol for AdtImplementation {
    type SyntaxTree = syntax_tree::item::Function;
    type Flag = ();
    type Data = ();
}
