use pernixc_base::diagnostic::Handler;

use super::model::Model;
use crate::{
    error,
    symbol::GlobalID,
    table::{self, NoContainer, Table},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct State;

impl table::State for State {
    type Container = NoContainer;

    fn on_global_id_resolved(
        _: &Table<Self>,
        _: GlobalID,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
    ) {
    }

    fn on_resolved<M: Model>(
        _: &Table<Self>,
        _: table::resolution::Resolution<M>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
    ) {
    }
}
