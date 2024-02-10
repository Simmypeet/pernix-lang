use pernixc_syntax::syntax_tree;

use super::Symbol;
use crate::symbol::TraitImplementationFunction;

impl Symbol for TraitImplementationFunction {
    type SyntaxTree = syntax_tree::item::ImplementationFunction;
    type Flag = ();
    type Data = ();
}
