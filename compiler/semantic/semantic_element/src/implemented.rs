//! Defines a query for retrieving all the `implements` IDs that are associated
//! with a given symbol.

use pernixc_hash::FxHashSet;
use pernixc_target::{Global, TargetID};
use qbice::{Decode, Encode, Query, StableHash, storage::intern::Interned};

/// A query for retrieving all the `implements`/`instance` IDs that implements
/// this symbol.
///
/// This will include all the `implements` IDs that implemented the `symbol_id`
/// in all the downstream targets.
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
#[extend(name = get_implemented, by_val)]
pub struct Key {
    /// The global ID of the symbol to get implementations for.
    pub symbol_id: Global<pernixc_symbol::SymbolID>,

    /// The target ID where the query was made.
    ///
    /// This is used to scan for all downstream targets to find all the
    /// implementations of this symbol.
    pub target_id: TargetID,
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
#[value(Interned<FxHashSet<Global<pernixc_symbol::SymbolID>>>)]
pub struct InTargetKey {
    /// The ID of the implementable (trait or marker).
    pub implementable_id: Global<pernixc_symbol::SymbolID>,

    /// The target ID to search for implementations in.
    pub target_id: pernixc_target::TargetID,
}
