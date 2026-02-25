//! Contains the [`Key`] query definition.

use pernixc_target::Global;
use pernixc_term::instance::TraitRef;
use qbice::{Decode, Encode, Query, StableHash, storage::intern::Interned};

/// A query for retrieving the [`TraitRef`] of a the `instance` symbol.
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
#[value(Interned<TraitRef>)]
#[extend(name = get_trait_ref, by_val)]
pub struct Key {
    /// The global ID of the `instance` symbol.
    pub symbol_id: Global<pernixc_symbol::ID>,
}
