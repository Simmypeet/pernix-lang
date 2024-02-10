use pernixc_syntax::syntax_tree;

use super::Symbol;
use crate::symbol::Struct;

impl Symbol for Struct {
    type SyntaxTree = syntax_tree::item::Struct;
    type Flag = ();
    type Data = ();
}
