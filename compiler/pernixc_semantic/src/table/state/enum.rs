use parking_lot::RwLock;
use pernixc_syntax::syntax_tree;

use super::Symbol;
use crate::{
    arena::{Arena, ID},
    symbol::Enum,
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

impl Symbol for Enum {
    type Data = ();
    type Flag = Flag;
    type SyntaxTree = syntax_tree::item::EnumSignature;

    fn get_arena(table: &Table) -> &Arena<RwLock<Self>, ID<Self>> { &table.enums }

    fn get_arena_mut(table: &mut Table) -> &mut Arena<RwLock<Self>, ID<Self>> { &mut table.enums }
}
