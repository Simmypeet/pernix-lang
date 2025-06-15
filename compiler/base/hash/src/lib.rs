//! This file contains re-exports for the hash map/set types that use the a more
//! optimized hash function from the `fnv` crate. Since the default hash map/set
//! uses the `RandomState` and `SipHasher13` providing a more cryptographically
//! secure has function, but with a performance cost.

/// A type alias for a hash map that uses the `fnv` hash function.
pub type HashMap<T, V> = std::collections::HashMap<T, V, fnv::FnvBuildHasher>;

/// A type alias for a hash set that uses the `fnv` hash function.
pub type HashSet<T> = std::collections::HashSet<T, fnv::FnvBuildHasher>;

/// A type alias for a dash map that uses the `fnv` hash function.
pub type DasMap<T, V> = dashmap::DashMap<T, V, fnv::FnvBuildHasher>;

/// A type alias for a dash set that uses the `fnv` hash function.
pub type DasSet<T> = dashmap::DashSet<T, fnv::FnvBuildHasher>;
