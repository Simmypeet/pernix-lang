use pernixc_syntax::syntax_tree;

use super::State;
use crate::symbol::TraitImplementation;

impl State for TraitImplementation {
    type SyntaxTree = syntax_tree::item::ImplementationSignature;
}
