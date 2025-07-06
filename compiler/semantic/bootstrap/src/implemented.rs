//! Contains the definition of the [`Key`] type.

use std::sync::Arc;

use pernixc_hash::HashSet;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

use crate::symbol;

/// Used for retrieving a list of `implements` symbols that implement the
/// given symbol ID.
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
#[value(Arc<HashSet<Global<symbol::ID>>>)]
#[extend(method(get_implemented), unwrap("should have no cyclic dependencies"))]
pub struct Key(pub Global<symbol::ID>);
