use crate::table::{self, NoContainer};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct State;

impl table::State for State {
    type Container = NoContainer;
}
