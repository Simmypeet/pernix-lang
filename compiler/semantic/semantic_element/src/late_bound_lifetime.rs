//! Contains the query definition for the list of lifetime parameters that
//! are considered late bound.

use pernixc_arena::ID;
use pernixc_hash::HashSet;
use pernixc_target::Global;
use pernixc_term::generic_parameters::LifetimeParameter;
use qbice::{Decode, Encode, Query, StableHash, storage::intern::Interned};

/// A query for retrieving the list of lifetime parameters that are considered
/// late bound.
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
#[value(Interned<HashSet<ID<LifetimeParameter>>>)]
#[extend(name = get_late_bound_lifetimes, by_val)]
pub struct Key {
    /// The global ID of the function symbol.
    pub symbol_id: Global<pernixc_symbol::ID>,
}
