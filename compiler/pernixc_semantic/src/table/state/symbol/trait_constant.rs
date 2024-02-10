use pernixc_syntax::syntax_tree;

use super::Symbol;
use crate::symbol::TraitConstant;

impl Symbol for TraitConstant {
    type SyntaxTree = syntax_tree::item::TraitConstant;
    type Flag = ();
    type Data = ();
}
