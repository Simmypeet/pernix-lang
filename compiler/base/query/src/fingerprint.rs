//! A crate for calculating fingerprints of query results

use pernixc_stable_hash::{StableHash, StableHasher as _};

/// Calculates a 128-bit fingerprint for a value that implements the
/// `StableHash` trait.
pub fn fingerprint<T: ?Sized + StableHash>(
    fingerprint: u64,
    value: &T,
) -> u128 {
    let mut hasher = pernixc_stable_hash::Sip128Hasher::new();
    fingerprint.stable_hash(&mut hasher);
    value.stable_hash(&mut hasher);
    hasher.finish()
}
