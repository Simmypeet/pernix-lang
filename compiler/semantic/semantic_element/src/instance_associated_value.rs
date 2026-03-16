//! Contains the query definition for the return type of the function symbol.

use pernixc_target::Global;
use pernixc_term::instance::Instance;
use qbice::{Decode, Encode, Query, StableHash, storage::intern::Interned};

/// A query for retrieving the instance associated instance value.
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
#[value(Interned<Instance>)]
#[extend(name = get_instance_associated_value, by_val)]
pub struct Key {
    /// The global ID of the function symbol.
    pub symbol_id: Global<pernixc_symbol::ID>,
}
