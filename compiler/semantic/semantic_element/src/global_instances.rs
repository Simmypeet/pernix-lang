//! Defines a query for retrieving all the `instances` IDs that are associated
//! with a `trait` symbol.
//!
//! This will only include the `instance` IDs that "ARE NOT EXTERNAL".

use pernixc_hash::HashSet;
use pernixc_target::Global;
use qbice::{Decode, Encode, Query, StableHash, storage::intern::Interned};

/// A query for retrieving all the `instance` IDs that are associated with a
/// `trait` symbol.
///
/// This will only include the `instance` IDs that "ARE NOT EXTERNAL".
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
#[value(Interned<HashSet<Global<pernixc_symbol::ID>>>)]
#[extend(name = get_global_instances_of, by_val)]
pub struct Key {
    /// The global ID of the symbol to get implementations for.
    pub symbol_id: Global<pernixc_symbol::ID>,

    /// The target ID to search for implementations in.
    pub target_id: pernixc_target::TargetID,
}

/// A query for retrieving all the `implements` IDs that implements this symbol
/// in a specific target.
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
#[value(Interned<HashSet<Global<pernixc_symbol::ID>>>)]
pub struct InTargetKey {
    /// The ID of the implementable (trait or marker).
    pub implementable_id: Global<pernixc_symbol::ID>,

    /// The target ID to search for implementations in.
    pub target_id: pernixc_target::TargetID,
}
