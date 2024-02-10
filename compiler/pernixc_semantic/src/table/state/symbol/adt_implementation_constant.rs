use pernixc_syntax::syntax_tree;

use super::Symbol;
use crate::symbol::AdtImplementationConstant;

impl Symbol for AdtImplementationConstant {
    type SyntaxTree = syntax_tree::item::ImplementationConstant;
    type Flag = ();
    type Data = ();
}
