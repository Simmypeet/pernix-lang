//! Contains the definition of the [`Key`] type.

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
#[value(Global<crate::ID>)]
#[extend(method(get_implements), no_cyclic)]
pub struct Key(pub Global<crate::ID>);
