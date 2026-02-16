//! Defines a query for retrieving the symbol ID being implemented by the
//! `implements`.

use pernixc_target::Global;
use qbice::{Decode, Encode, Query, StableHash};

/// A query for retrieving what symbol is this `implements` is implemented for.
/// It can be a `struct`, `enum`, `trait`, or `marker`.
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
#[value(Option<Global<pernixc_symbol::ID>>)]
#[extend(name = get_implements, by_val)]
pub struct Key {
    /// The global ID of the implements symbol.
    pub symbol_id: Global<pernixc_symbol::ID>,
}
