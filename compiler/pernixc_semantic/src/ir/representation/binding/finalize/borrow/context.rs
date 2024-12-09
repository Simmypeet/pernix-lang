use pernixc_base::source_file::Span;

use super::{environment::Environment, state::Stack};
use crate::{
    ir::{address::Address, representation::borrow::Model as BorrowModel},
    type_system::term::r#type::Qualifier,
};

/// Represents an access to a particular memory location. This is subjected to
/// check by the borrow checker whether it is safe or not.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Access {
    /// The address of the memory location.
    pub address: Address<BorrowModel>,

    /// The qualifier of the access.
    pub qualifier: Qualifier,

    /// The span of the access.
    pub span: Span,
}

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
