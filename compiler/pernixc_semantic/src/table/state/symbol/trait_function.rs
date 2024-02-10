use pernixc_syntax::syntax_tree;

use super::Symbol;
use crate::symbol::TraitFunction;

impl Symbol for TraitFunction {
    type SyntaxTree = syntax_tree::item::TraitFunction;
    type Flag = ();
    type Data = ();
}
