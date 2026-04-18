//! Error marker term used for recovery.

use qbice::{Decode, Encode, StableHash};

/// Represents an erroneous term.
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
pub struct Error;
