use pernixc_syntax::syntax_tree;

use super::State;
use crate::symbol::Constant;

impl State for Constant {
    type SyntaxTree = syntax_tree::item::Constant;
}
