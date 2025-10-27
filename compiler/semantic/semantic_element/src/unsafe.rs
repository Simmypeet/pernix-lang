//! Defines a query for retrieving whether a function is marked as unsafe.

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

/// A query for retrieving whether a function is marked as unsafe.
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
#[value(bool)]
#[extend(method(is_unsafe), no_cyclic)]
pub struct Key(pub Global<pernixc_symbol::ID>);
