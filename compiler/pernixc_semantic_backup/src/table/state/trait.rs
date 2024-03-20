use pernixc_syntax::syntax_tree;

use super::State;
use crate::symbol::Trait;

impl State for Trait {
    type SyntaxTree = syntax_tree::item::TraitSignature;
}
