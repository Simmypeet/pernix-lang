use pernixc_syntax::syntax_tree;

use super::Symbol;
use crate::symbol::NegativeTraitImplementation;

impl Symbol for NegativeTraitImplementation {
    type SyntaxTree = syntax_tree::item::ImplementationSignature;
    type Flag = ();
    type Data = ();
}
