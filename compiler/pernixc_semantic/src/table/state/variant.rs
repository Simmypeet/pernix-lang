use parking_lot::RwLock;
use pernixc_syntax::syntax_tree;

use super::Symbol;
use crate::{
    arena::{Arena, ID},
    symbol::Variant,
    table::Table,
};

super::build_flag!(
    pub(super) enum Flag {
        Drafted,
        Definition,
        Check,
    }
);

impl Symbol for Variant {
    type Data = ();
    type Flag = Flag;
    type SyntaxTree = syntax_tree::item::Variant;

    fn get_arena(table: &Table) -> &Arena<RwLock<Self>, ID<Self>> { &table.variants }

    fn get_arena_mut(table: &mut Table) -> &mut Arena<RwLock<Self>, ID<Self>> {
        &mut table.variants
    }
}
