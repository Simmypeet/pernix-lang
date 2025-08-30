//! Defines a query for retrieving all the `implements` IDs that are associated
//! with a given symbol.

use std::sync::Arc;

use pernixc_hash::HashSet;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

/// A query for retrieving all the `implements` IDs that implements this symbol.
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
    Serialize,
    Deserialize,
    pernixc_query::Key,
)]
#[value(Arc<HashSet<Global<pernixc_symbol::ID>>>)]
#[extend(method(get_implemented))]
pub struct Key(pub Global<pernixc_symbol::ID>);
