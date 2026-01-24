//! A query for retrieving the declaration order of a variant in the enum.

use pernixc_target::Global;
use qbice::{Decode, Encode, Query, StableHash};

use crate::ID;

/// A query for retrieving the declaration order of a variant in the enum.
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
    StableHash,
    Query,
)]
#[value(usize)]
#[extend(name = get_variant_declaration_order, by_val)]
pub struct Key {
    /// The global ID of the variant to get the declaration order for.
    pub variant_id: Global<ID>,
}
