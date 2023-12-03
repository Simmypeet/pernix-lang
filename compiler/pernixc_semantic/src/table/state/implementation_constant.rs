use parking_lot::RwLock;
use pernixc_syntax::syntax_tree;

use super::Symbol;
use crate::{
    arena::{Arena, ID},
    symbol::ImplementationConstant,
    table::Table,
};

super::build_flag!(
    pub(super) enum Flag {
        Drafted,
        Built,
        Check,
    }
);

impl Symbol for ImplementationConstant {
    type Data = ();
    type Flag = Flag;
    type SyntaxTree = syntax_tree::item::ImplementationConstant;

    fn get_arena(table: &Table) -> &Arena<RwLock<Self>, ID<Self>> {
        &table.implementation_constants
    }

    fn get_arena_mut(table: &mut Table) -> &mut Arena<RwLock<Self>, ID<Self>> {
        &mut table.implementation_constants
    }
}
