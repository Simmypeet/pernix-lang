use pernixc_syntax::syntax_tree;

use super::State;
use crate::symbol::Variant;

impl State for Variant {
    type SyntaxTree = syntax_tree::item::Type;
}
