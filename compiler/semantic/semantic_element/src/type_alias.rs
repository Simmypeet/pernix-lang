//! Defines a query for retrieving type alias term from the type alias symbols.

use pernixc_target::Global;
use pernixc_term::r#type::Type;
use qbice::{Decode, Encode, Query, StableHash, storage::intern::Interned};

/// A key for retrieving the type alias of various kinds of type alias symbols.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    StableHash,
    Encode,
    Decode,
    Query,
)]
#[value(Interned<Type>)]
#[extend(name = get_type_alias, by_val)]
pub struct Key {
    /// The global ID of the type alias symbol.
    pub symbol_id: Global<pernixc_symbol::ID>,
}
