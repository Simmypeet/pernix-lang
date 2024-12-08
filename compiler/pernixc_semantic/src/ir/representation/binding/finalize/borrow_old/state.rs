use crate::{
    arena::Arena,
    ir::representation::{binding::finalize::memory, borrow::Origin},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct State {
    pub memory_state: memory::state::Stack,
    pub origins: Arena<Origin>,
}

impl State {
    pub fn new(
        memory_state: memory::state::Stack,
        origins: Arena<Origin>,
    ) -> Self {
        Self { memory_state, origins }
    }
}
