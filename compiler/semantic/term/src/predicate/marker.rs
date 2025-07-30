use derive_new::new;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

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
    Serialize,
    Deserialize,
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
    Serialize,
    Deserialize,
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
