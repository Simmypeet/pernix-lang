//! This file contains re-exports for the hash map/set types that use the a more
//! optimized hash function from the `fnv` crate. Since the default hash map/set
//! uses the `RandomState` and `SipHasher13` providing a more cryptographically
//! secure has function, but with a performance cost.

use std::hash::BuildHasherDefault;

use qbice::Identifiable;

/// A wrapper around the `fxhash::FxHasher` to implement the [`Identifiable`]
/// trait.
#[derive(Debug, Clone, Default, Identifiable)]
pub struct FxHasher(fxhash::FxHasher);

impl std::hash::Hasher for FxHasher {
    fn finish(&self) -> u64 { self.0.finish() }

    fn write(&mut self, bytes: &[u8]) { self.0.write(bytes) }

    fn write_u8(&mut self, i: u8) { self.0.write_u8(i) }

    fn write_u16(&mut self, i: u16) { self.0.write_u16(i) }

    fn write_u32(&mut self, i: u32) { self.0.write_u32(i) }

    fn write_u64(&mut self, i: u64) { self.0.write_u64(i) }

    fn write_usize(&mut self, i: usize) { self.0.write_usize(i) }

    fn write_i8(&mut self, i: i8) { self.0.write_i8(i) }

    fn write_i16(&mut self, i: i16) { self.0.write_i16(i) }

    fn write_i32(&mut self, i: i32) { self.0.write_i32(i) }

    fn write_i64(&mut self, i: i64) { self.0.write_i64(i) }

    fn write_isize(&mut self, i: isize) { self.0.write_isize(i) }

    fn write_i128(&mut self, i: i128) { self.0.write_i128(i) }

    fn write_u128(&mut self, i: u128) { self.0.write_u128(i) }
}

/// A type alias for a hash map that uses the `fx` hash function.
pub type HashMap<T, V> =
    std::collections::HashMap<T, V, BuildHasherDefault<FxHasher>>;

/// A type alias for a hash set that uses the `fx` hash function.
pub type HashSet<T> =
    std::collections::HashSet<T, BuildHasherDefault<FxHasher>>;

/// A type alias for a dash map that uses the `fx` hash function.
pub type DashMap<T, V> = dashmap::DashMap<T, V, BuildHasherDefault<FxHasher>>;

/// A type alias for a dash set that uses the `fx` hash function.
pub type DashSet<T> = dashmap::DashSet<T, BuildHasherDefault<FxHasher>>;

/// A type alias for a read-only view of a dash map that uses the `fx` hash
/// function.
pub type ReadOnlyView<K, V> =
    dashmap::ReadOnlyView<K, V, BuildHasherDefault<FxHasher>>;
