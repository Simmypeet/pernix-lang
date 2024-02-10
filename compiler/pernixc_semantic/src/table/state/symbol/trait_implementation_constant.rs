use pernixc_syntax::syntax_tree;

use super::Symbol;
use crate::symbol::TraitImplementationConstant;

impl Symbol for TraitImplementationConstant {
    type SyntaxTree = syntax_tree::item::ImplementationConstant;
    type Flag = ();
    type Data = ();
}
