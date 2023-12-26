use pernixc_syntax::syntax_tree;

use super::State;
use crate::symbol::AdtImplementation;

impl State for AdtImplementation {
    type SyntaxTree = syntax_tree::item::ImplementationSignature;
}
