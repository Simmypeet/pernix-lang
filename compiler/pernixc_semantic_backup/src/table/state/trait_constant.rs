use pernixc_syntax::syntax_tree;

use super::State;
use crate::symbol::TraitConstant;

impl State for TraitConstant {
    type SyntaxTree = syntax_tree::item::TraitConstant;
}
