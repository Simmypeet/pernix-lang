use super::contains_error;
use crate::{
    instance::InstanceAssociated, instantiation::Instantiation, r#type::Type,
};

/// A predicate establishing equality between an instance-associated type and a
/// concrete type.
///
/// This is analogous to Rust's associated type bounds like
/// `Iterator<Item = u32>`. In Pernix, this predicate asserts that a specific
/// instance-associated type (e.g., `I::Item` where `I` is an instance of the
/// `Iterator` trait) is equal to a given type.
pub type InstanceAssociatedTypeEquality =
    crate::predicate::Compatible<InstanceAssociated, Type>;

impl InstanceAssociatedTypeEquality {
    /// Checks if any term contains an error.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        self.lhs.contains_error() || contains_error(&self.rhs)
    }

    /// Applies the instantiation to the predicate.
    pub fn instantiate(&mut self, instantiation: &Instantiation) {
        self.lhs.instantiate(instantiation);
        instantiation.instantiate(&mut self.rhs);
    }
}
