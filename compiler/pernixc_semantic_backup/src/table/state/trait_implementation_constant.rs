use pernixc_syntax::syntax_tree;

use super::State;
use crate::symbol::TraitImplementationConstant;

impl State for TraitImplementationConstant {
    type SyntaxTree = syntax_tree::item::ImplementationConstant;
}
