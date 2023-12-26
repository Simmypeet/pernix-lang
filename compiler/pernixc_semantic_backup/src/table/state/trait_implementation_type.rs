use pernixc_syntax::syntax_tree;

use super::State;
use crate::symbol::TraitImplementationType;

impl State for TraitImplementationType {
    type SyntaxTree = syntax_tree::item::ImplementationFunction;
}
