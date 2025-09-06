use std::fmt::Write;

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use super::contains_error;
use crate::{instantiation::Instantiation, r#type::Type};

/// Represents a type can be used as a type of a compile-time constant value.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct ConstantType(pub Type);

impl ConstantType {
    /// Checks if the type contains a `forall` lifetime.
    #[must_use]
    pub fn contains_error(&self) -> bool { contains_error(&self.0) }

    /// Applies the instantiation to the type.
    pub fn instantiate(&mut self, instantiation: &Instantiation) {
        instantiation.instantiate(&mut self.0);
    }
}

impl crate::display::Display for ConstantType {
    async fn fmt(
        &self,
        engine: &pernixc_query::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        self.0.fmt(engine, formatter).await?;
        write!(formatter, ": const")
    }
}
