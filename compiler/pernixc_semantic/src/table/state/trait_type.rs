use parking_lot::RwLock;
use pernixc_syntax::syntax_tree;

use super::Symbol;
use crate::{
    arena::{Arena, ID},
    symbol::TraitType,
    table::Table,
};

super::build_flag!(
    pub(super) enum Flag {
        Drafted,
        GenericParameter,
        Body,
        WhereClause,
        Check,
    }
);

impl Symbol for TraitType {
    type Data = ();
    type Flag = Flag;
    type SyntaxTree = syntax_tree::item::TraitType;

    fn get_arena(table: &Table) -> &Arena<RwLock<Self>, ID<Self>> { &table.trait_types }

    fn get_arena_mut(table: &mut Table) -> &mut Arena<RwLock<Self>, ID<Self>> {
        &mut table.trait_types
    }
}
