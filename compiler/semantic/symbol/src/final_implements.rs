//! Contains the qyery definition for retrieving whether the trait
//! implementation is final or not

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

/// Representing the symbol that is being implemented by the `implements`
/// symbol.
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
    Serialize,
    Deserialize,
    pernixc_query::Key,
    StableHash,
)]
#[value(bool)]
#[extend(method(is_trait_implements_final), no_cyclic)]
pub struct Key(pub Global<crate::ID>);
