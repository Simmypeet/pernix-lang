//! Defines a query for retrieving the effects that a function may perform.

use std::sync::Arc;

use pernixc_serialize::Serialize;
use pernixc_stable_hash::StableHash;
use pernixc_symbol::ID;
use pernixc_target::Global;
use pernixc_term::effect::Effect;

/// A query for retrieving a set of effects that the function may perform.
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
    pernixc_query::Key,
)]
#[value(Arc<Effect>)]
pub struct Key(pub Global<ID>);
