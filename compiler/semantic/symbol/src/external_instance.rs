//! Contains the query determining whether the `implements` is external or not.

use pernixc_target::Global;
use qbice::{Decode, Encode, Query, StableHash};

use crate::ID;

/// The query used for determining whether the `instance` is external or not.
///
/// If the instance is external, it means that this particular instance will not
/// be included in the candidate list for instance resolution.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    Query,
    StableHash,
)]
#[value(bool)]
#[extend(name = is_external_instance, by_val)]
pub struct Key {
    /// The global ID of the symbol to get the accessibility for.
    pub symbol_id: Global<ID>,
}
