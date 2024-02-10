use pernixc_syntax::syntax_tree;

use super::Symbol;
use crate::symbol::Type;

impl Symbol for Type {
    type SyntaxTree = syntax_tree::item::Type;
    type Flag = ();
    type Data = ();
}
