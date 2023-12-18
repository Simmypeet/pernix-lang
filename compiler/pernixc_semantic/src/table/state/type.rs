use pernixc_syntax::syntax_tree;

use super::State;
use crate::symbol::Type;

impl State for Type {
    type SyntaxTree = syntax_tree::item::Type;
}
