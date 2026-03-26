//! Defines a query for retrieving all the `instances` IDs that are associated
//! with a `trait` symbol.
//!
//! This will only include the `instance` IDs that "ARE NOT EXTERNAL".

use pernixc_hash::FxHashSet;
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
#[value(Interned<FxHashSet<Global<pernixc_symbol::SymbolID>>>)]
#[extend(name = get_global_instances_of, by_val)]
pub struct Key {
    /// The trait symbol to get the instances of.
    pub trait_id: Global<pernixc_symbol::SymbolID>,

    /// The target ID to search for implementations in.
    pub target_id: pernixc_target::TargetID,
}

/// A query for retrieving all the `instance` IDs that are associated with a
/// `trait` symbol in a specific target.
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
#[value(Interned<FxHashSet<Global<pernixc_symbol::SymbolID>>>)]
pub struct InTargetKey {
    /// The trait symbol to get the instances of.
    pub trait_id: Global<pernixc_symbol::SymbolID>,

    /// The target ID to search for implementations in.
    pub target_id: pernixc_target::TargetID,
}
