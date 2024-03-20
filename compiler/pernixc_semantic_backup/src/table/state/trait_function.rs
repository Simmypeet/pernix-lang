use pernixc_syntax::syntax_tree;

use super::State;
use crate::symbol::TraitFunction;

impl State for TraitFunction {
    type SyntaxTree = syntax_tree::item::TraitFunction;
}
