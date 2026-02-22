use std::fmt::Write;

use derive_new::new;
use qbice::{Decode, Encode, StableHash};

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
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    new,
)]
pub struct InstanceAssociatedTypeEquality {
    /// The instance-associated type (e.g., `I::Item`).
    instance_associated_type: InstanceAssociated,

    /// The type that the instance-associated type is equal to.
    r#type: Type,
}

impl InstanceAssociatedTypeEquality {
    /// Checks if any term contains an error.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        self.instance_associated_type.contains_error()
            || contains_error(&self.r#type)
    }

    /// Applies the instantiation to the predicate.
    pub fn instantiate(&mut self, instantiation: &Instantiation) {
        self.instance_associated_type.instantiate(instantiation);
        instantiation.instantiate(&mut self.r#type);
    }
}

impl crate::display::Display for InstanceAssociatedTypeEquality {
    async fn fmt(
        &self,
        engine: &pernixc_qbice::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        self.instance_associated_type.fmt(engine, formatter).await?;
        write!(formatter, " = ")?;
        self.r#type.fmt(engine, formatter).await
    }
}
