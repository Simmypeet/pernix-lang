use pernixc_syntax::syntax_tree;

use super::Symbol;
use crate::symbol::Function;

impl Symbol for Function {
    type SyntaxTree = syntax_tree::item::Function;
    type Flag = ();
    type Data = ();
}
