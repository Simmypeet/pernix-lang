use super::{environment::Environment, state::Stack};

/// Contains both memory state and borrow origins.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Context {
    /// Contains the memory state.
    pub stack: Stack,

    /// Contains the information related to borrow checking.
    pub environment: Environment,
}

impl Context {
    /// Merges the other environment into this one.
    ///
    /// The modification is done in place on the `self` environment.
    pub fn merge(&mut self, other: &Self) {
        self.stack.min_merge(&other.stack);
        self.environment.merge(&other.environment);
    }
}
