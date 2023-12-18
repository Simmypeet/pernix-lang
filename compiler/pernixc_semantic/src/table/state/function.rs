use pernixc_syntax::syntax_tree;

use super::State;
use crate::symbol::Function;

impl State for Function {
    type SyntaxTree = syntax_tree::item::Function;
}
