use pernixc_syntax::syntax_tree;

use super::State;
use crate::symbol::TraitImplementationFunction;

impl State for TraitImplementationFunction {
    type SyntaxTree = syntax_tree::item::ImplementationFunction;
}
