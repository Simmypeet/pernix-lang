//! Defines the `Effect` and `Unit` types, which represent effect sets and
//! individual effects in the type system.

use std::collections::BTreeSet;

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::generic_arguments::Symbol;

/// Represents a single `effect Fizz` in a set of effects.
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
    derive_more::From,
    derive_more::Deref,
    derive_more::DerefMut,
)]
pub struct Unit(pub Symbol);

/// Represents a set of effects, such as `effect Fizz + effect Buzz`. It's
/// composed of multiple effect `Unit`s.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct Effect {
    /// The individual effects that compose this set of effects.
    pub effects: BTreeSet<Unit>,
}
