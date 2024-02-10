use pernixc_syntax::syntax_tree;

use super::Symbol;
use crate::symbol::TraitType;

impl Symbol for TraitType {
    type SyntaxTree = syntax_tree::item::TraitType;
    type Flag = ();
    type Data = ();
}
