//! Contains the definition of [`Lifetime`] term.

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::{generic_parameters::LifetimeParameterID, inference::Inference};

/// Represents a lifetime term.
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
#[allow(missing_docs)]
pub enum Lifetime {
    Inference(pernixc_arena::ID<Inference<Self>>),
    Parameter(LifetimeParameterID),
    Static,
    Erased,
}
