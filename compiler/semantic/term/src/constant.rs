//! Contains the definition of [`Constant`] term.

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::{generic_parameters::ConstantParameterID, inference::Inference};

/// Represents a compile-time constant term.
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
pub enum Constant {
    Inference(pernixc_arena::ID<Inference<Self>>),
    Parameter(ConstantParameterID),
}
