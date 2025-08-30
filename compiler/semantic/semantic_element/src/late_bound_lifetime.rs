//! Contains the query definition for the list of lifetime parameters that
//! are considered late bound.

use std::sync::Arc;

use pernixc_arena::ID;
use pernixc_hash::HashSet;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;
use pernixc_term::generic_parameters::LifetimeParameter;

/// A query for retrieving the list of lifetime parameters that are considered
/// late bound.
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
#[value(Arc<HashSet<ID<LifetimeParameter>>>)]
#[extend(method(get_late_bound_lifetimes))]
pub struct Key(pub Global<pernixc_symbol::ID>);
