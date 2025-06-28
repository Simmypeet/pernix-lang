//! # Stable Hash Library
//!
//! This crate provides a stable hashing system that produces consistent hash
//! values across different program runs and platforms. Unlike standard hash
//! functions that may vary between runs for security reasons, stable hashes are
//! designed to be reproducible and deterministic.
//!
//! The main components of this library are:
//!
//! - [`StableHash`] trait: Defines how types can be hashed in a stable manner
//! - [`StableHasher`] trait: Defines the interface for stable hash functions
//! - [`StableSipHasher`] struct: A concrete implementation using the `SipHash`
//!   algorithm
//! - [`Value`] trait: Represents hash output values that can be combined
//!
//! ## Example
//!
//! ```rust
//! use pernixc_stable_hash::{StableHash, StableHasher, StableSipHasher};
//!
//! let mut hasher = StableSipHasher::new();
//! "hello world".stable_hash(&mut hasher);
//! let hash = hasher.finish();
//! ```

use std::{
    hash::{BuildHasher, Hash},
    mem::Discriminant,
};

extern crate self as pernixc_stable_hash;
pub use pernixc_stable_hash_derive::StableHash;

#[doc(hidden)]
pub mod __internal {}

/// A trait for values that can be used as hash outputs.
///
/// This trait represents numeric values that can be used as the result of
/// stable hashing. Types implementing this trait must support default
/// construction, stable hashing of themselves, and wrapping addition for
/// combining hash values.
pub trait Value: Default + StableHash {
    /// Performs wrapping addition of two values.
    ///
    /// This is used to combine hash values in a way that doesn't panic on
    /// overflow, which is essential for stable hash computation.
    ///
    /// # Arguments
    ///
    /// * `other` - The value to add to this value
    ///
    /// # Returns
    ///
    /// The result of wrapping addition
    #[must_use]
    fn wrapping_add(self, other: Self) -> Self;
}

/// A trait for stable hash functions.
///
/// This trait defines the interface for hash functions that produce consistent,
/// stable hash values. Unlike standard hash functions that may vary between
/// runs for security reasons, stable hashers are designed to be reproducible.
///
/// The hasher works by accepting data through various `write_*` methods and
/// producing a final hash value through the `finish` method.
///
/// ## Example
///
/// ```rust
/// use pernixc_stable_hash::{StableHash, StableHasher, StableSipHasher};
///
/// let mut hasher = StableSipHasher::new();
/// hasher.write_u32(42);
/// hasher.write_str("hello");
/// let hash = hasher.finish();
/// ```
pub trait StableHasher {
    /// The type of hash value produced by this hasher.
    ///
    /// This associated type must implement [`Value`], which allows hash values
    /// to be combined using wrapping addition for stable hash computation.
    type Hash: Value;

    /// Consumes the hasher and returns the final hash value.
    ///
    /// This method should be called after all data has been written to the
    /// hasher to obtain the computed hash value.
    ///
    /// # Returns
    ///
    /// The computed hash value of type `Self::Hash`
    fn finish(&self) -> Self::Hash;

    /// Writes a slice of bytes to the hasher.
    ///
    /// This is the fundamental method that all other `write_*` methods delegate
    /// to. The bytes are processed in the order they appear in the slice.
    ///
    /// # Arguments
    ///
    /// * `bytes` - The byte slice to hash
    fn write(&mut self, bytes: &[u8]);

    /// Writes a single `u8` value to the hasher.
    ///
    /// # Arguments
    ///
    /// * `i` - The `u8` value to hash
    fn write_u8(&mut self, i: u8) { self.write(&[i]); }

    /// Writes a single `i8` value to the hasher.
    ///
    /// The value is converted to little-endian bytes before hashing.
    ///
    /// # Arguments
    ///
    /// * `i` - The `i8` value to hash
    fn write_i8(&mut self, i: i8) { self.write(&i.to_le_bytes()); }

    /// Writes a single `u16` value to the hasher.
    ///
    /// The value is converted to little-endian bytes before hashing.
    ///
    /// # Arguments
    ///
    /// * `i` - The `u16` value to hash
    fn write_u16(&mut self, i: u16) { self.write(&i.to_le_bytes()); }

    /// Writes a single `i16` value to the hasher.
    ///
    /// The value is converted to little-endian bytes before hashing.
    ///
    /// # Arguments
    ///
    /// * `i` - The `i16` value to hash
    fn write_i16(&mut self, i: i16) { self.write(&i.to_le_bytes()); }

    /// Writes a single `u32` value to the hasher.
    ///
    /// The value is converted to little-endian bytes before hashing.
    ///
    /// # Arguments
    ///
    /// * `i` - The `u32` value to hash
    fn write_u32(&mut self, i: u32) { self.write(&i.to_le_bytes()); }

    /// Writes a single `i32` value to the hasher.
    ///
    /// The value is converted to little-endian bytes before hashing.
    ///
    /// # Arguments
    ///
    /// * `i` - The `i32` value to hash
    fn write_i32(&mut self, i: i32) { self.write(&i.to_le_bytes()); }

    /// Writes a single `u64` value to the hasher.
    ///
    /// The value is converted to little-endian bytes before hashing.
    ///
    /// # Arguments
    ///
    /// * `i` - The `u64` value to hash
    fn write_u64(&mut self, i: u64) { self.write(&i.to_le_bytes()); }

    /// Writes a single `i64` value to the hasher.
    ///
    /// The value is converted to little-endian bytes before hashing.
    ///
    /// # Arguments
    ///
    /// * `i` - The `i64` value to hash
    fn write_i64(&mut self, i: i64) { self.write(&i.to_le_bytes()); }

    /// Writes a single `u128` value to the hasher.
    ///
    /// The value is converted to little-endian bytes before hashing.
    ///
    /// # Arguments
    ///
    /// * `i` - The `u128` value to hash
    fn write_u128(&mut self, i: u128) { self.write(&i.to_le_bytes()); }

    /// Writes a single `i128` value to the hasher.
    ///
    /// The value is converted to little-endian bytes before hashing.
    ///
    /// # Arguments
    ///
    /// * `i` - The `i128` value to hash
    fn write_i128(&mut self, i: i128) { self.write(&i.to_le_bytes()); }

    /// Writes a single `usize` value to the hasher.
    ///
    /// The value is converted to little-endian bytes before hashing.
    ///
    /// # Arguments
    ///
    /// * `i` - The `usize` value to hash
    fn write_usize(&mut self, i: usize) { self.write(&i.to_le_bytes()); }

    /// Writes a single `isize` value to the hasher.
    ///
    /// The value is converted to little-endian bytes before hashing.
    ///
    /// # Arguments
    ///
    /// * `i` - The `isize` value to hash
    fn write_isize(&mut self, i: isize) { self.write(&i.to_le_bytes()); }

    /// Writes a single `f32` value to the hasher.
    ///
    /// NaN values are normalized to a canonical NaN before hashing to ensure
    /// consistent hash values regardless of the specific NaN representation.
    /// The value is converted to little-endian bytes before hashing.
    ///
    /// # Arguments
    ///
    /// * `f` - The `f32` value to hash
    fn write_f32(&mut self, f: f32) {
        let normalized = if f.is_nan() { f32::NAN } else { f };
        self.write(&normalized.to_le_bytes());
    }

    /// Writes a single `f64` value to the hasher.
    ///
    /// NaN values are normalized to a canonical NaN before hashing to ensure
    /// consistent hash values regardless of the specific NaN representation.
    /// The value is converted to little-endian bytes before hashing.
    ///
    /// # Arguments
    ///
    /// * `f` - The `f64` value to hash
    fn write_f64(&mut self, f: f64) {
        let normalized = if f.is_nan() { f64::NAN } else { f };
        self.write(&normalized.to_le_bytes());
    }

    /// Writes a length prefix for variable-length data.
    ///
    /// This is typically used before writing collections or strings to include
    /// their length in the hash, ensuring that different collections with the
    /// same elements in the same order but different lengths produce different
    /// hashes.
    ///
    /// # Arguments
    ///
    /// * `len` - The length value to write as a prefix
    fn write_length_prefix(&mut self, len: usize) { self.write_usize(len); }

    /// Writes a string to the hasher.
    ///
    /// The string is hashed with a length prefix followed by its UTF-8 bytes.
    /// This ensures that strings with the same content but different lengths
    /// (due to embedded nulls or similar) produce different hash values.
    ///
    /// # Arguments
    ///
    /// * `s` - The string slice to hash
    fn write_str(&mut self, s: &str) {
        self.write_length_prefix(s.len());
        self.write(s.as_bytes());
    }

    /// Creates a sub-hasher and computes a hash for a nested structure.
    ///
    /// This method is used to compute hashes for nested data structures while
    /// maintaining the stability of the overall hash. The provided closure is
    /// called with a new hasher instance, and the resulting hash is returned.
    ///
    /// This is particularly useful for hashing collections where the order of
    /// elements shouldn't affect the final hash (like `HashMap` or `HashSet`).
    ///
    /// # Arguments
    ///
    /// * `f` - A closure that receives a mutable reference to a sub-hasher
    ///
    /// # Returns
    ///
    /// The hash value computed by the sub-hasher
    fn sub_hash(
        &mut self,
        f: &mut dyn FnMut(&mut dyn StableHasher<Hash = Self::Hash>),
    ) -> Self::Hash;
}

/// A trait for types that can be hashed in a stable manner.
///
/// Types implementing this trait can produce consistent hash values across
/// different program runs and platforms. This is essential for reproducible
/// builds, serialization, and testing scenarios where hash consistency matters.
///
/// The trait provides a single method that takes a mutable reference to a
/// [`StableHasher`] and feeds the type's data into the hasher.
///
/// ## Example
///
/// ```rust
/// use pernixc_stable_hash::{StableHash, StableHasher, StableSipHasher};
///
/// #[derive(Debug)]
/// struct Point {
///     x: i32,
///     y: i32,
/// }
///
/// impl StableHash for Point {
///     fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
///         self.x.stable_hash(state);
///         self.y.stable_hash(state);
///     }
/// }
///
/// let point = Point { x: 10, y: 20 };
/// let mut hasher = StableSipHasher::new();
/// point.stable_hash(&mut hasher);
/// let hash = hasher.finish();
/// ```
pub trait StableHash {
    /// Feeds this type's data into the provided hasher.
    ///
    /// Implementations should hash all fields that contribute to the type's
    /// identity in a consistent order. The hash should be stable across
    /// different program runs and platforms.
    ///
    /// # Arguments
    ///
    /// * `state` - The hasher to feed data into
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H);
}

static_assertions::assert_obj_safe!(StableHasher<Hash = u128>);
static_assertions::assert_obj_safe!(StableHasher<Hash = u64>);

impl Value for u8 {
    fn wrapping_add(self, other: Self) -> Self { self.wrapping_add(other) }
}

impl Value for u16 {
    fn wrapping_add(self, other: Self) -> Self { self.wrapping_add(other) }
}

impl Value for u32 {
    fn wrapping_add(self, other: Self) -> Self { self.wrapping_add(other) }
}

impl Value for u64 {
    fn wrapping_add(self, other: Self) -> Self { self.wrapping_add(other) }
}

impl Value for u128 {
    fn wrapping_add(self, other: Self) -> Self { self.wrapping_add(other) }
}

impl Value for usize {
    fn wrapping_add(self, other: Self) -> Self { self.wrapping_add(other) }
}

/// A stable hash function implementation based on the `SipHash` algorithm.
///
/// `StableSipHasher` provides a concrete implementation of the [`StableHasher`]
/// trait using a modified version of the `SipHash` algorithm. Unlike the
/// standard library's hasher, this implementation uses fixed keys to ensure
/// consistent hash values across different program runs.
///
/// The hasher produces 128-bit hash values and is designed to be
/// cryptographically secure while maintaining excellent performance
/// characteristics.
///
/// ## Features
///
/// - **Deterministic**: Always produces the same hash for the same input
/// - **Fast**: Optimized for performance with minimal overhead
/// - **Secure**: Based on the cryptographically secure `SipHash` algorithm
/// - **Cross-platform**: Consistent results across different architectures
///
/// ## Example
///
/// ```rust
/// use pernixc_stable_hash::{StableHash, StableHasher, StableSipHasher};
///
/// let mut hasher = StableSipHasher::new();
/// "hello world".stable_hash(&mut hasher);
/// let hash = hasher.finish();
/// println!("Hash: {:x}", hash);
/// ```
#[derive(Clone, Copy, Debug)]
pub struct StableSipHasher {
    v0: u64,
    v1: u64,
    v2: u64,
    v3: u64,
    tail: u64,
    ntail: usize,
    processed: usize,
}

impl StableSipHasher {
    /// Creates a new `StableSipHasher` with default keys.
    ///
    /// This constructor uses fixed, predetermined keys to ensure that hash
    /// values are consistent across different program runs. The default keys
    /// are chosen to provide good hash distribution while maintaining
    /// stability.
    ///
    /// # Returns
    ///
    /// A new `StableSipHasher` instance ready for use
    ///
    /// # Example
    ///
    /// ```rust
    /// use pernixc_stable_hash::{StableHasher, StableSipHasher};
    ///
    /// let mut hasher = StableSipHasher::new();
    /// hasher.write_u32(42);
    /// let hash = hasher.finish();
    /// ```
    #[must_use]
    pub const fn new() -> Self {
        Self::new_with_keys(0x736f_6d65_7073_6575, 0x646f_7261_6e64_6f6d)
    }

    /// Creates a new `StableSipHasher` with custom keys.
    ///
    /// This constructor allows you to specify custom keys for the hasher.
    /// Different keys will produce different hash values for the same input,
    /// but the same keys will always produce consistent results.
    ///
    /// # Arguments
    ///
    /// * `key0` - The first 64-bit key for the hash function
    /// * `key1` - The second 64-bit key for the hash function
    ///
    /// # Returns
    ///
    /// A new `StableSipHasher` instance initialized with the provided keys
    ///
    /// # Example
    ///
    /// ```rust
    /// use pernixc_stable_hash::{StableHasher, StableSipHasher};
    ///
    /// let mut hasher = StableSipHasher::new_with_keys(
    ///     0x1234_5678_9abc_def0,
    ///     0xfedc_ba98_7654_3210,
    /// );
    /// hasher.write_str("test");
    /// let hash = hasher.finish();
    /// ```
    #[must_use]
    pub const fn new_with_keys(key0: u64, key1: u64) -> Self {
        let mut hasher = Self {
            v0: key0 ^ 0x736f_6d65_7073_6575,
            v1: key1 ^ 0x646f_7261_6e64_6f6d,
            v2: key0 ^ 0x6c79_6765_6e65_7261,
            v3: key1 ^ 0x7465_6462_7974_6573,
            tail: 0,
            ntail: 0,
            processed: 0,
        };

        hasher.v1 ^= 0xee;

        hasher
    }

    const fn new_sub_hasher() -> Self {
        Self::new_with_keys(0x0123_4567_89ab_cdef, 0xfedc_ba98_7654_3210)
    }

    #[inline]
    const fn sipround(&mut self) {
        self.v0 = self.v0.wrapping_add(self.v1);
        self.v1 = self.v1.rotate_left(13);
        self.v1 ^= self.v0;
        self.v0 = self.v0.rotate_left(32);

        self.v2 = self.v2.wrapping_add(self.v3);
        self.v3 = self.v3.rotate_left(16);
        self.v3 ^= self.v2;

        self.v0 = self.v0.wrapping_add(self.v3);
        self.v3 = self.v3.rotate_left(21);
        self.v3 ^= self.v0;

        self.v2 = self.v2.wrapping_add(self.v1);
        self.v1 = self.v1.rotate_left(17);
        self.v1 ^= self.v2;
        self.v2 = self.v2.rotate_left(32);
    }

    #[inline]
    const fn process_block(&mut self, block: u64) {
        self.v3 ^= block;

        self.sipround();
        self.sipround();

        self.v0 ^= block;
    }

    fn finish_128(&self) -> u128 {
        let mut hasher = *self;

        let mut tail = hasher.tail;
        tail |= (hasher.processed.wrapping_add(hasher.ntail) as u64) << 56;

        hasher.process_block(tail);

        hasher.v2 ^= 0xff;

        hasher.sipround();
        hasher.sipround();
        hasher.sipround();
        hasher.sipround();

        let first_half = hasher.v0 ^ hasher.v1 ^ hasher.v2 ^ hasher.v3;

        hasher.v1 ^= 0xdd;

        hasher.sipround();
        hasher.sipround();
        hasher.sipround();
        hasher.sipround();

        let second_half = hasher.v0 ^ hasher.v1 ^ hasher.v2 ^ hasher.v3;

        (u128::from(second_half) << 64) | u128::from(first_half)
    }
}

impl Default for StableSipHasher {
    fn default() -> Self { Self::new() }
}

impl StableHasher for StableSipHasher {
    type Hash = u128;

    fn finish(&self) -> u128 { self.finish_128() }

    fn write(&mut self, bytes: &[u8]) {
        let mut bytes = bytes;

        if self.ntail > 0 {
            let needed = 8 - self.ntail;
            let available = bytes.len().min(needed);

            for &byte in &bytes[..available] {
                self.tail |= u64::from(byte) << (8 * self.ntail);
                self.ntail += 1;
            }

            if self.ntail == 8 {
                self.process_block(self.tail);
                self.processed = self.processed.wrapping_add(8);
                self.tail = 0;
                self.ntail = 0;
            }

            bytes = &bytes[available..];
        }

        while bytes.len() >= 8 {
            let block = u64::from_le_bytes([
                bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5],
                bytes[6], bytes[7],
            ]);
            self.process_block(block);
            self.processed = self.processed.wrapping_add(8);
            bytes = &bytes[8..];
        }

        for (i, &byte) in bytes.iter().enumerate() {
            self.tail |= u64::from(byte) << (8 * i);
        }
        self.ntail = bytes.len();
    }

    fn sub_hash(
        &mut self,
        f: &mut dyn FnMut(&mut dyn StableHasher<Hash = Self::Hash>),
    ) -> Self::Hash {
        let mut sub_hasher = Self::new_sub_hasher();
        f(&mut sub_hasher);
        sub_hasher.finish()
    }
}

impl StableHash for u8 {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        state.write_u8(*self);
    }
}

impl StableHash for i8 {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        state.write_i8(*self);
    }
}

impl StableHash for u16 {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        state.write_u16(*self);
    }
}

impl StableHash for i16 {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        state.write_i16(*self);
    }
}

impl StableHash for u32 {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        state.write_u32(*self);
    }
}

impl StableHash for i32 {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        state.write_i32(*self);
    }
}

impl StableHash for u64 {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        state.write_u64(*self);
    }
}

impl StableHash for i64 {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        state.write_i64(*self);
    }
}

impl StableHash for u128 {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        state.write_u128(*self);
    }
}

impl StableHash for i128 {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        state.write_i128(*self);
    }
}

impl StableHash for usize {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        state.write_usize(*self);
    }
}

impl StableHash for isize {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        state.write_isize(*self);
    }
}

impl StableHash for f32 {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        state.write_f32(*self);
    }
}

impl StableHash for f64 {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        state.write_f64(*self);
    }
}

impl StableHash for str {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        state.write_str(self);
    }
}

impl StableHash for String {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.as_str().stable_hash(state);
    }
}

impl<T: StableHash> StableHash for Vec<T> {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        state.write_length_prefix(self.len());
        for item in self {
            item.stable_hash(state);
        }
    }
}

impl<K: StableHash, V: StableHash, B> StableHash
    for std::collections::HashMap<K, V, B>
{
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.len().stable_hash(state);
        let mut combined = H::Hash::default();

        for (key, value) in self {
            combined = combined.wrapping_add(state.sub_hash(&mut |sub| {
                key.stable_hash(sub);
                value.stable_hash(sub);
            }));
        }

        combined.stable_hash(state);
    }
}

impl<T: StableHash, B> StableHash for std::collections::HashSet<T, B> {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.len().stable_hash(state);
        let mut combined = H::Hash::default();

        for value in self {
            combined = combined.wrapping_add(state.sub_hash(&mut |sub| {
                value.stable_hash(sub);
            }));
        }

        combined.stable_hash(state);
    }
}

impl<K: StableHash, V: StableHash> StableHash
    for std::collections::BTreeMap<K, V>
{
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.len().stable_hash(state);
        for (key, value) in self {
            key.stable_hash(state);
            value.stable_hash(state);
        }
    }
}

impl<T: StableHash> StableHash for std::collections::BTreeSet<T> {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.len().stable_hash(state);
        for value in self {
            value.stable_hash(state);
        }
    }
}

impl<T: StableHash + ?Sized> StableHash for &T {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        (**self).stable_hash(state);
    }
}

impl<T: StableHash + ?Sized> StableHash for &mut T {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        (**self).stable_hash(state);
    }
}

// Commonly used standard library types
impl<T: StableHash> StableHash for Option<T> {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        let discriminant = std::mem::discriminant(self);
        discriminant.stable_hash(state);

        if let Some(value) = self {
            value.stable_hash(state);
        }
    }
}

impl<T: StableHash, E: StableHash> StableHash for Result<T, E> {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        let discriminant = std::mem::discriminant(self);
        discriminant.stable_hash(state);

        match self {
            Ok(value) => {
                value.stable_hash(state);
            }
            Err(error) => {
                error.stable_hash(state);
            }
        }
    }
}

impl StableHash for bool {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        state.write_u8(u8::from(*self));
    }
}

impl StableHash for char {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        state.write_u32(*self as u32);
    }
}

// Tuple implementations using macro
macro_rules! impl_stable_hash_tuple {
    () => {
        impl StableHash for () {
            fn stable_hash<H: StableHasher + ?Sized>(&self, _state: &mut H) {
                // Unit type has no data to hash
            }
        }
    };
    ($($name:ident)+) => {
        impl<$($name: StableHash),+> StableHash for ($($name,)+) {
            #[allow(non_snake_case)]
            fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
                let ($($name,)+) = self;
                $($name.stable_hash(state);)+
            }
        }
    };
}

// Implement tuples from () to (T, U, V, W, X, Y, Z, A, B, C, D, E)
impl_stable_hash_tuple!();
impl_stable_hash_tuple!(T);
impl_stable_hash_tuple!(T U);
impl_stable_hash_tuple!(T U V);
impl_stable_hash_tuple!(T U V W);
impl_stable_hash_tuple!(T U V W X);
impl_stable_hash_tuple!(T U V W X Y);
impl_stable_hash_tuple!(T U V W X Y Z);
impl_stable_hash_tuple!(T U V W X Y Z A);
impl_stable_hash_tuple!(T U V W X Y Z A B);
impl_stable_hash_tuple!(T U V W X Y Z A B C);
impl_stable_hash_tuple!(T U V W X Y Z A B C D);
impl_stable_hash_tuple!(T U V W X Y Z A B C D E);

// Array implementation using const generics - coerce to slice and hash
impl<T: StableHash, const N: usize> StableHash for [T; N] {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        // Coerce the array to a slice and hash it
        self.as_slice().stable_hash(state);
    }
}

// Slice implementation
impl<T: StableHash> StableHash for [T] {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        state.write_length_prefix(self.len());
        for item in self {
            item.stable_hash(state);
        }
    }
}

// Box implementation
impl<T: StableHash + ?Sized> StableHash for Box<T> {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        (**self).stable_hash(state);
    }
}

// Rc and Arc implementations
impl<T: StableHash + ?Sized> StableHash for std::rc::Rc<T> {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        (**self).stable_hash(state);
    }
}

impl<T: StableHash + ?Sized> StableHash for std::sync::Arc<T> {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        (**self).stable_hash(state);
    }
}

// Cow implementation
impl<T: StableHash + Clone> StableHash for std::borrow::Cow<'_, T> {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        (**self).stable_hash(state);
    }
}

// Range implementations
impl<T: StableHash> StableHash for std::ops::Range<T> {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.start.stable_hash(state);
        self.end.stable_hash(state);
    }
}

impl<T: StableHash> StableHash for std::ops::RangeInclusive<T> {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.start().stable_hash(state);
        self.end().stable_hash(state);
    }
}

impl<T: StableHash> StableHash for std::ops::RangeFrom<T> {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.start.stable_hash(state);
    }
}

impl<T: StableHash> StableHash for std::ops::RangeTo<T> {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.end.stable_hash(state);
    }
}

impl<T: StableHash> StableHash for std::ops::RangeToInclusive<T> {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.end.stable_hash(state);
    }
}

impl StableHash for std::ops::RangeFull {
    fn stable_hash<H: StableHasher + ?Sized>(&self, _state: &mut H) {
        // RangeFull (..) has no data to hash
    }
}

// VecDeque implementation
impl<T: StableHash> StableHash for std::collections::VecDeque<T> {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        state.write_length_prefix(self.len());
        for item in self {
            item.stable_hash(state);
        }
    }
}

// LinkedList implementation
impl<T: StableHash> StableHash for std::collections::LinkedList<T> {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        state.write_length_prefix(self.len());
        for item in self {
            item.stable_hash(state);
        }
    }
}

impl<T: StableHash> StableHash for std::collections::BinaryHeap<T> {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.len().stable_hash(state);
        let mut combined = H::Hash::default();

        for value in self {
            combined = combined.wrapping_add(state.sub_hash(&mut |sub| {
                value.stable_hash(sub);
            }));
        }

        combined.stable_hash(state);
    }
}

// NonZero types
impl StableHash for std::num::NonZeroU8 {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.get().stable_hash(state);
    }
}

impl StableHash for std::num::NonZeroU16 {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.get().stable_hash(state);
    }
}

impl StableHash for std::num::NonZeroU32 {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.get().stable_hash(state);
    }
}

impl StableHash for std::num::NonZeroU64 {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.get().stable_hash(state);
    }
}

impl StableHash for std::num::NonZeroU128 {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.get().stable_hash(state);
    }
}

impl StableHash for std::num::NonZeroUsize {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.get().stable_hash(state);
    }
}

impl StableHash for std::num::NonZeroI8 {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.get().stable_hash(state);
    }
}

impl StableHash for std::num::NonZeroI16 {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.get().stable_hash(state);
    }
}

impl StableHash for std::num::NonZeroI32 {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.get().stable_hash(state);
    }
}

impl StableHash for std::num::NonZeroI64 {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.get().stable_hash(state);
    }
}

impl StableHash for std::num::NonZeroI128 {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.get().stable_hash(state);
    }
}

impl StableHash for std::num::NonZeroIsize {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.get().stable_hash(state);
    }
}

// Duration implementation
impl StableHash for std::time::Duration {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.as_secs().stable_hash(state);
        self.subsec_nanos().stable_hash(state);
    }
}

// PathBuf and Path implementations
impl StableHash for std::path::PathBuf {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.as_path().stable_hash(state);
    }
}

impl StableHash for std::path::Path {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.as_os_str().stable_hash(state);
    }
}

// OsString and OsStr implementations
impl StableHash for std::ffi::OsString {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.as_os_str().stable_hash(state);
    }
}

impl StableHash for std::ffi::OsStr {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.as_encoded_bytes().stable_hash(state);
    }
}

// CString and CStr implementations
impl StableHash for std::ffi::CString {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.as_c_str().stable_hash(state);
    }
}

impl StableHash for std::ffi::CStr {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        let bytes = self.to_bytes();
        state.write_length_prefix(bytes.len());
        state.write(bytes);
    }
}

macro_rules! impl_stable_hash_atomic {
    ($type:ty) => {
        impl StableHash for $type {
            fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
                self.load(std::sync::atomic::Ordering::Relaxed)
                    .stable_hash(state);
            }
        }
    };
}

impl_stable_hash_atomic!(std::sync::atomic::AtomicBool);
impl_stable_hash_atomic!(std::sync::atomic::AtomicU8);
impl_stable_hash_atomic!(std::sync::atomic::AtomicU16);
impl_stable_hash_atomic!(std::sync::atomic::AtomicU32);
impl_stable_hash_atomic!(std::sync::atomic::AtomicU64);
impl_stable_hash_atomic!(std::sync::atomic::AtomicUsize);
impl_stable_hash_atomic!(std::sync::atomic::AtomicI8);
impl_stable_hash_atomic!(std::sync::atomic::AtomicI16);
impl_stable_hash_atomic!(std::sync::atomic::AtomicI32);
impl_stable_hash_atomic!(std::sync::atomic::AtomicI64);
impl_stable_hash_atomic!(std::sync::atomic::AtomicIsize);

impl<K: StableHash + Eq + Hash, V: StableHash, B: BuildHasher + Clone>
    StableHash for dashmap::DashMap<K, V, B>
{
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.len().stable_hash(state);
        let mut combined = H::Hash::default();

        for entry in self {
            combined = combined.wrapping_add(state.sub_hash(&mut |sub| {
                entry.key().stable_hash(sub);
                entry.value().stable_hash(sub);
            }));
        }

        combined.stable_hash(state);
    }
}

impl<T: StableHash + Eq + Hash, B: BuildHasher + Clone> StableHash
    for dashmap::DashSet<T, B>
{
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        self.len().stable_hash(state);
        let mut combined = H::Hash::default();

        for value in self.iter() {
            combined = combined.wrapping_add(state.sub_hash(&mut |sub| {
                value.stable_hash(sub);
            }));
        }

        combined.stable_hash(state);
    }
}

impl<const SIZE: usize, const PAD1: usize, const PAD2: usize, HEAP> StableHash
    for flexstr::FlexStr<SIZE, PAD1, PAD2, HEAP>
where
    HEAP: std::ops::Deref<Target = str>,
{
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        state.write_str(self.as_str());
    }
}

impl<T> StableHash for Discriminant<T> {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        let bytes = unsafe {
            std::slice::from_raw_parts(
                std::ptr::from_ref::<Self>(self).cast::<u8>(),
                std::mem::size_of::<Self>(),
            )
        };
        state.write(bytes);
    }
}

impl<T> StableHash for std::marker::PhantomData<T> {
    fn stable_hash<H: StableHasher + ?Sized>(&self, _state: &mut H) {}
}

#[cfg(test)]
mod test;
