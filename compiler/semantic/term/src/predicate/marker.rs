use std::fmt::Write;

use derive_new::new;
use pernixc_symbol::name::get_qualified_name;
use pernixc_target::Global;
use qbice::{Decode, Encode, StableHash};

use crate::{
    generic_arguments::GenericArguments, instantiation::Instantiation,
};

/// Predicates specifying that the marker is satisfied.
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
pub struct Positive {
    /// The id of the marker.
    pub marker_id: Global<pernixc_symbol::ID>,

    /// The generic arguments supplied to the marker.
    pub generic_arguments: GenericArguments,
}

impl Positive {
    /// Checks if there's an error in the generic arguments.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        self.generic_arguments.contains_error()
    }

    /// Applies the instantiation to the generic arguments.
    pub fn instantiate(&mut self, instantiation: &Instantiation) {
        self.generic_arguments.instantiate(instantiation);
    }
}

impl crate::display::Display for Positive {
    async fn fmt(
        &self,
        engine: &pernixc_qbice::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        let qualified_name = engine.get_qualified_name(self.marker_id).await;
        write!(formatter, "marker {qualified_name}")?;
        self.generic_arguments.fmt(engine, formatter).await
    }
}

/// The predicate specifying that the marker will never be satisfied.
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
pub struct Negative {
    /// The id of the marker.
    pub marker_id: Global<pernixc_symbol::ID>,

    /// The generic arguments supplied to the marker.
    pub generic_arguments: GenericArguments,
}

impl Negative {
    /// Checks if there's an error in the generic arguments.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        self.generic_arguments.contains_error()
    }

    /// Applies the instantiation to the generic arguments.
    pub fn instantiate(&mut self, instantiation: &Instantiation) {
        self.generic_arguments.instantiate(instantiation);
    }
}

impl crate::display::Display for Negative {
    async fn fmt(
        &self,
        engine: &pernixc_qbice::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        let qualified_name = engine.get_qualified_name(self.marker_id).await;
        write!(formatter, "marker not {qualified_name}")?;
        self.generic_arguments.fmt(engine, formatter).await
    }
}
