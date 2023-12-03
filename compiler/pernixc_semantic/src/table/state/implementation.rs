use parking_lot::RwLock;
use pernixc_syntax::syntax_tree;

use super::Symbol;
use crate::{
    arena::{Arena, ID},
    symbol::Implementation,
    table::Table,
};

super::build_flag!(
    pub(super) enum Flag {
        Drafted,
        GenericParameter,
        WhereClause,
        Check,
    }
);

impl Symbol for Implementation {
    type Data = ();
    type Flag = Flag;
    type SyntaxTree = syntax_tree::item::ImplementationSignature;

    fn get_arena(table: &Table) -> &Arena<RwLock<Self>, ID<Self>> { &table.implementations }

    fn get_arena_mut(table: &mut Table) -> &mut Arena<RwLock<Self>, ID<Self>> {
        &mut table.implementations
    }
}
