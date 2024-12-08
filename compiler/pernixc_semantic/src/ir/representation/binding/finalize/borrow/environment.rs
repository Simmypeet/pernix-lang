use pernixc_base::source_file::Span;

use super::state;
use crate::{
    arena::Arena,
    ir::{
        address::Address,
        representation::borrow::{Model as BorrowModel, Origin},
    },
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
pub struct Environment {
    /// Contains the memory state.
    pub stack: state::Stack,

    /// Contains the borrow origins.
    pub origins: Arena<Origin>,

    /// The accesses that have not been decided yet whether they are safe or
    /// not.
    pub unchecked_accesses: Vec<Access>,
}

impl Environment {
    /// Merges the other environment into this one.
    ///
    /// The modification is done in place on the `self` environment.
    pub fn merge(&mut self, other: &Self) {
        self.stack.min_merge(&other.stack);

        for id in other.origins.ids() {
            self.origins
                .get_mut(id)
                .unwrap()
                .loans
                .extend(other.origins.get(id).unwrap().loans.iter().cloned());
        }

        self.unchecked_accesses
            .extend(other.unchecked_accesses.iter().cloned());
    }
}
