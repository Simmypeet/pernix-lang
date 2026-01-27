//! Defines a query for retrieving the effects that a function may perform.

use pernixc_arena::OrderedArena;
use pernixc_symbol::ID;
use pernixc_target::Global;
use pernixc_term::effect;
use qbice::{Decode, Encode, Query, StableHash, storage::intern::Interned};

/// A query for retrieving a set of effects that the function may perform.
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
#[value(Interned<OrderedArena<effect::Unit>>)]
#[extend(name = get_effect_annotation, by_val)]
pub struct Key {
    /// The global ID of the function symbol.
    pub symbol_id: Global<ID>,
}
