//! A crate for calculating fingerprints of query results

use pernixc_stable_hash::{StableHash, StableHasher as _};

/// Calculates a 128-bit fingerprint for a value that implements the
/// `StableHash` trait.
pub fn fingerprint<T: ?Sized + StableHash>(value: &T) -> u128 {
    let mut hasher = pernixc_stable_hash::Sip128Hasher::new();
    value.stable_hash(&mut hasher);
    hasher.finish()
}
