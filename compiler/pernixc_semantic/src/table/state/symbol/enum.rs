use pernixc_syntax::syntax_tree;

use super::Symbol;
use crate::symbol::Enum;

impl Symbol for Enum {
    type SyntaxTree = syntax_tree::item::EnumSignature;
    type Flag = ();
    type Data = ();
}
