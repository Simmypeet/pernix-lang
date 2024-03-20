use pernixc_syntax::syntax_tree;

use super::State;
use crate::symbol::TraitType;

impl State for TraitType {
    type SyntaxTree = syntax_tree::item::TraitType;
}
