use pernixc_syntax::syntax_tree;

use super::State;
use crate::symbol::AdtImplementationType;

impl State for AdtImplementationType {
    type SyntaxTree = syntax_tree::item::ImplementationType;
}
