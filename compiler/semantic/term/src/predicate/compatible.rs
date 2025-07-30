use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

/// A predicate representing compatible equality between two values.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    derive_new::new,
)]
#[allow(missing_docs)]
pub struct Compatible<T, U = T> {
    pub lhs: T,
    pub rhs: U,
}
