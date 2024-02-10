use pernixc_syntax::syntax_tree;

use super::Symbol;
use crate::symbol::Variant;

impl Symbol for Variant {
    type SyntaxTree = syntax_tree::item::Variant;
    type Flag = ();
    type Data = ();
}
