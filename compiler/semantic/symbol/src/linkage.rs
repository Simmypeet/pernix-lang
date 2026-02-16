//! Defines the linkage of functions and static variables.

use pernixc_target::Global;
use qbice::{Decode, Encode, Identifiable, Query, StableHash};

use crate::ID;

/// Represents the linkage of a function or static variable.
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
    Encode,
    Decode,
    Identifiable,
)]
#[allow(missing_docs)]
pub enum Linkage {
    C(C),
    Unknown,
}

/// Represents the C linkage options.
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
    Encode,
    Decode,
)]
pub struct C {
    /// Whether the function is variadic.
    pub variadic: bool,
}

/// The key type used with [`TrackedEngine`] to access the linkage of a
/// function or static variable.
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
    Encode,
    Decode,
    Query,
)]
#[value(Linkage)]
#[extend(name = get_linkage, by_val)]
pub struct Key {
    /// The global ID of the symbol to get the linkage for.
    pub symbol_id: Global<ID>,
}
