use pernixc_syntax::syntax_tree;

use super::Symbol;
use crate::symbol::TraitImplementationType;

impl Symbol for TraitImplementationType {
    type SyntaxTree = syntax_tree::item::Type;
    type Flag = ();
    type Data = ();
}
