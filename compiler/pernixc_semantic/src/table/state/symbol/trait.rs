use pernixc_syntax::syntax_tree;

use super::Symbol;
use crate::symbol::Trait;

impl Symbol for Trait {
    type SyntaxTree = syntax_tree::item::TraitSignature;
    type Flag = ();
    type Data = ();
}
