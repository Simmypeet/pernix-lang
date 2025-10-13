//! Contains the query definition for determining if a function is marked as
//! `unsafe`.

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

/// A query key for determining whether a function symbol is marked as `unsafe`.
///
/// This query returns `true` if the function is declared with the `unsafe`
/// keyword, and `false` otherwise.
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
#[extend(method(is_unsafe_function), no_cyclic)]
pub struct Key(pub Global<pernixc_symbol::ID>);
