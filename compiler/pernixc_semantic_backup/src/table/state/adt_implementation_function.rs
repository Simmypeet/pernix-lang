use pernixc_syntax::syntax_tree;

use super::State;
use crate::symbol::AdtImplementationFunction;

impl State for AdtImplementationFunction {
    type SyntaxTree = syntax_tree::item::ImplementationFunction;
}
