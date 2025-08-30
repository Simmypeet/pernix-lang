//! Contains the query definition for the list of elided lifetimes used in the
//! function signature.

use std::sync::Arc;

use pernixc_arena::Arena;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;
use pernixc_term::lifetime::ElidedLifetime;

/// A query for retrieving the list of elided lifetimes used in the function
/// signature.
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
#[value(Arc<Arena<ElidedLifetime>>)]
#[extend(method(get_elided_lifetimes))]
pub struct Key(pub Global<pernixc_symbol::ID>);
