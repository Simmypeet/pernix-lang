use pernixc_syntax::syntax_tree;

use super::State;
use crate::symbol::Enum;

impl State for Enum {
    type SyntaxTree = syntax_tree::item::Enum;
}
