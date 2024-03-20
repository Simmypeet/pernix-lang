use std::collections::HashMap;

use parking_lot::RwLock;
use pernixc_syntax::syntax_tree;

use crate::{
    arena::{Arena, ID},
    symbol::Constant,
    table::{
        state::{build_flag, Builder, State, Symbol},
        Table,
    },
};

build_flag!(
    pub(in crate::table) enum Flag {
        Drafted,
        Built,
        Check,
    }
);

impl Symbol for Constant {
    type Data = ();
    type Flag = Flag;
    type SyntaxTree = syntax_tree::item::Constant;

    fn get_arena(table: &Table) -> &Arena<RwLock<Self>, ID<Self>> {
        &table.constants
    }

    fn get_arena_mut(table: &mut Table) -> &mut Arena<RwLock<Self>, ID<Self>> {
        &mut table.constants
    }

    fn get_states(builder: &Builder) -> &HashMap<ID<Self>, State<Self>> {
        &builder.states_by_constant_id
    }

    fn get_states_mut(
        builder: &mut Builder,
    ) -> &mut HashMap<ID<Self>, State<Self>> {
        &mut builder.states_by_constant_id
    }
}
