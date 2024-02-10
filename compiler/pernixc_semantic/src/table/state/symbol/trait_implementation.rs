use pernixc_syntax::syntax_tree;

use super::Symbol;
use crate::symbol::TraitImplementation;

impl Symbol for TraitImplementation {
    type SyntaxTree = syntax_tree::item::ImplementationSignature;
    type Flag = ();
    type Data = ();
}
