use parking_lot::RwLock;
use pernixc_syntax::syntax_tree;

use super::Symbol;
use crate::{
    arena::{Arena, ID},
    symbol::Function,
    table::Table,
};

super::build_flag!(
    pub(super) enum Flag {
        Drafted,
        GenericParameter,
        Signature,
        WhereClause,
        BodyAndCheck,
    }
);

impl Symbol for Function {
    type Data = ();
    type Flag = Flag;
    type SyntaxTree = syntax_tree::item::Function;

    fn get_arena(table: &Table) -> &Arena<RwLock<Self>, ID<Self>> { &table.functions }

    fn get_arena_mut(table: &mut Table) -> &mut Arena<RwLock<Self>, ID<Self>> {
        &mut table.functions
    }
}
