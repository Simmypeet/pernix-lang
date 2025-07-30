//! Contains the definition of the [`Error`] term.

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

/// Represents an errornuos term. Used for representing errors in the type
/// system.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct Error;
