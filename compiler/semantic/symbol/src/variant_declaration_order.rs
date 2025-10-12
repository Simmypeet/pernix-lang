//! A query for retrieving the declaration order of a variant in the enum.

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

use crate::ID;

/// A query for retrieving the declaration order of a variant in the enum.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Key,
)]
#[value(usize)]
#[extend(method(get_variant_declaration_order), no_cyclic)]
pub struct Key(pub Global<ID>);
