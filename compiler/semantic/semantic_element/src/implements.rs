//! Defines a query for retrieving the symbol ID being implemented by the
//! `implements`.

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

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
    Serialize,
    Deserialize,
    pernixc_query::Key,
)]
#[value(Option<Global<pernixc_symbol::ID>>)]
#[extend(method(get_implements))]
pub struct Key(pub Global<pernixc_symbol::ID>);
