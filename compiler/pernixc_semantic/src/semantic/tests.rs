use crate::{
    symbol::GlobalID,
    table::{self, NoContainer, Table},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct State;

impl table::State for State {
    type Container = NoContainer;

    fn on_global_id_resolved(_: &Table<Self>, _: GlobalID, _: GlobalID) {}

    fn on_resolved(
        _: &Table<Self>,
        _: table::resolution::Resolution,
        _: GlobalID,
    ) {
    }
}
