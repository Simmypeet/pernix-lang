use pernixc_syntax::syntax_tree;

use super::State;
use crate::symbol::AdtImplementationConstant;

impl State for AdtImplementationConstant {
    type SyntaxTree = syntax_tree::item::ImplementationConstant;
}
