use pernixc_syntax::syntax_tree;

use super::State;
use crate::symbol::Struct;

impl State for Struct {
    type SyntaxTree = syntax_tree::item::Struct;
}
