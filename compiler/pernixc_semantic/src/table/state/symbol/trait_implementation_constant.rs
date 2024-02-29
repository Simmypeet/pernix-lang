use pernixc_syntax::syntax_tree;

use super::Symbol;
use crate::symbol::TraitImplementationConstant;

impl Symbol for TraitImplementationConstant {
    type SyntaxTree = syntax_tree::item::Constant;
    type Flag = ();
    type Data = ();
}
