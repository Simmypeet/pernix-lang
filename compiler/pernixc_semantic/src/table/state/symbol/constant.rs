use pernixc_syntax::syntax_tree;

use super::Symbol;
use crate::symbol::Constant;

impl Symbol for Constant {
    type SyntaxTree = syntax_tree::item::Constant;
    type Flag = ();
    type Data = ();
}
