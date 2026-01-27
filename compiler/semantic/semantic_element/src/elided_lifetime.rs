//! Contains the query definition for the list of elided lifetimes used in the
//! function signature.

use pernixc_arena::Arena;
use pernixc_target::Global;
use pernixc_term::lifetime::ElidedLifetime;
use qbice::{Decode, Encode, Query, StableHash, storage::intern::Interned};

/// A query for retrieving the list of elided lifetimes used in the function
/// signature.
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
#[value(Interned<Arena<ElidedLifetime>>)]
#[extend(name = get_elided_lifetimes, by_val)]
pub struct Key {
    /// The global ID of the function symbol.
    pub symbol_id: Global<pernixc_symbol::ID>,
}
