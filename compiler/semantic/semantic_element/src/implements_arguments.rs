//! Contains the query definition for retrieving `implements` generic arguments.

use std::sync::Arc;

use pernixc_target::Global;
use pernixc_term::generic_arguments::GenericArguments;
use qbice::{Decode, Encode, Query, StableHash};

/// A query for retrieving the generic arguments supplied to the `implements`
/// item (`implements[GENERIC_PARAMETERS] symbol[IMPLEMENTS_ARGUMENTS]`)
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
#[value(Option<Arc<GenericArguments>>)]
#[extend(name = get_implements_argument, by_val)]
pub struct Key {
    /// The global ID of the implements symbol.
    pub symbol_id: Global<pernixc_symbol::ID>,
}
