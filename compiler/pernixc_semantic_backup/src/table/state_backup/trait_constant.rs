use std::collections::HashMap;

use parking_lot::RwLock;
use pernixc_syntax::syntax_tree;

use super::{Builder, State, Symbol};
use crate::{
    arena::{Arena, ID},
    symbol::TraitConstant,
    table::Table,
};

super::build_flag!(
    pub(in crate::table) enum Flag {
        Drafted,
        Built,
        Check,
    }
);

impl Symbol for TraitConstant {
    type Data = ();
    type Flag = Flag;
    type SyntaxTree = syntax_tree::item::TraitConstant;

    fn get_arena(table: &Table) -> &Arena<RwLock<Self>, ID<Self>> {
        &table.trait_constants
    }

    fn get_arena_mut(table: &mut Table) -> &mut Arena<RwLock<Self>, ID<Self>> {
        &mut table.trait_constants
    }

    fn get_states(builder: &Builder) -> &HashMap<ID<Self>, State<Self>> {
        &builder.states_by_trait_constant_id
    }

    fn get_states_mut(
        builder: &mut Builder,
    ) -> &mut HashMap<ID<Self>, State<Self>> {
        &mut builder.states_by_trait_constant_id
    }
}
