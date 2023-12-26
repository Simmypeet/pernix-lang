use pernixc_syntax::syntax_tree;

use super::State;
use crate::symbol::NegativeTraitImplementation;

impl State for NegativeTraitImplementation {
    type SyntaxTree = syntax_tree::item::ImplementationSignature;
}
