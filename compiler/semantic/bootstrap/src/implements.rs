//! Contains the definition of the [`Key`] type.

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

use crate::symbol;

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
#[value(Global<symbol::ID>)]
#[extend(method(get_implements), unwrap("should have no cyclic dependencies"))]
pub struct Key(pub Global<symbol::ID>);
