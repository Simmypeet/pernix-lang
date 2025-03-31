//! Contains the definition of [`Implementation`] cmoponent.

use pernixc_semantic::component::Derived;
use pernixc_term::{generic_arguments::GenericArguments, Default};
use serde::{Deserialize, Serialize};

/// A **presistent-derived** component representing the implementation signature
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
)]
pub struct Implementation {
    /// The generic arguments supplied to the symbol being implemented.
    pub generic_arguments: GenericArguments<Default>,
}

impl Derived for Implementation {
    fn component_name() -> &'static str { "implementation signature" }
}
