use pernixc_type::{substitution::Substitution, r#type::Type};
use qbice::storage::intern::Interned;

use crate::{
    constraints::Constraints,
    solver::{Agree, Solver},
};

impl Agree for (Substitution, Constraints) {
    fn agree(&self, other: &Self) -> bool {
        self.0 == other.0 && self.1 == other.1
    }
}

impl Solver<'_> {
    /// Computes a substitution `S` such that `S(head) == subject)`, if one
    /// exists, and the associated lifetime constraints.
    ///
    /// Lifetime constraints are generated if two lifetimes mismatch, for
    /// example, lifetime `a` and `b` such that `a != b` would generate the
    /// constraint `a: 'b` and `b: 'a`.
    pub fn match_types(
        &mut self,
        head: &Interned<Type>,
        subject: &Interned<Type>,
    ) -> Option<(Substitution, Constraints)> {
        todo!()
    }
}
