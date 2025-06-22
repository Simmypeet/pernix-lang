//! # Stable Hashing
//!
//! This crate provides traits and utilities for stable hashing, ensuring
//! consistent hash values across different runs and platforms. The stable
//! hashing system is designed to be deterministic and reproducible, making it
//! suitable for use in scenarios where hash consistency is critical, such as:
//!
//! - Compiler intermediate representations
//! - Serialization and deserialization
//! - Testing and debugging
//! - Incremental compilation systems
//!
//! ## Core Traits
//!
//! - [`Value`]: Represents hash values that can be combined
//! - [`StableHasher`]: The core hashing interface with deterministic behavior
//! - [`StableHash`]: Types that can be hashed in a stable manner
//!
//! ## Example
//!
//! ```rust
//! use stable_hash::{StableHash, StableHasher};
//!
//! // Implement StableHash for your types
//! struct MyStruct {
//!     value: u32,
//! }
//!
//! impl StableHash for MyStruct {
//!     fn stable_hash<H: StableHasher>(&self, state: &mut H) -> H::Hash {
//!         self.value.stable_hash(state)
//!     }
//! }
//! ```

/// A trait for hash values that can be combined through wrapping addition.
///
/// This trait is used to represent the result type of stable hashing
/// operations. Hash values must support wrapping addition to enable composition
/// of hashes from multiple components in a deterministic and order-independent
/// manner.
///
/// The key requirement is that the `wrapping_add` operation must be both
/// commutative and associative, ensuring that hash combinations produce
/// consistent results regardless of the order in which hash values are
/// combined. This property is crucial for stable hashing where deterministic
/// behavior across different execution contexts is required.
///
/// # Mathematical Properties
///
/// Implementations must satisfy:
/// - **Commutativity**: `a + b = b + a`
/// - **Associativity**: `(a + b) + c = a + (b + c)`
///
/// These properties ensure that collections of hash values can be combined
/// in any order and still produce the same final result.
///
/// # Examples
///
/// Common implementations include:
/// - `u64` for 64-bit hash values
/// - `u128` for 128-bit hash values
///
/// ```rust
/// use stable_hash::Value;
///
/// fn combine_hashes<V: Value>(hash1: V, hash2: V) -> V {
///     hash1.wrapping_add(hash2)
/// }
///
/// // Hash values can be combined in any order
/// fn combine_multiple<V: Value>(hashes: Vec<V>) -> V
/// where
///     V: Default + Copy,
/// {
///     hashes.into_iter().fold(V::default(), |acc, h| acc.wrapping_add(h))
/// }
/// ```
pub trait Value {
    /// Performs wrapping addition with another hash value.
    ///
    /// This operation combines two hash values using wrapping arithmetic,
    /// which ensures that overflow is handled consistently across platforms.
    /// The wrapping addition operation has crucial mathematical properties
    /// that make it suitable for stable hashing:
    ///
    /// - **Commutative**: `a.wrapping_add(b) == b.wrapping_add(a)`
    /// - **Associative**: `(a.wrapping_add(b)).wrapping_add(c) ==
    ///   a.wrapping_add(b.wrapping_add(c))`
    ///
    /// These properties ensure that hash values can be combined in any order
    /// and still produce the same result, which is essential for stable hashing
    /// where the order of operations must not affect the final hash value.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use stable_hash::Value;
    ///
    /// let hash1: u64 = 0x1234567890abcdef;
    /// let hash2: u64 = 0xfedcba0987654321;
    /// let hash3: u64 = 0x1111222233334444;
    ///
    /// // Commutative property
    /// assert_eq!(hash1.wrapping_add(hash2), hash2.wrapping_add(hash1));
    ///
    /// // Associative property
    /// let combined1 = hash1.wrapping_add(hash2).wrapping_add(hash3);
    /// let combined2 = hash1.wrapping_add(hash2.wrapping_add(hash3));
    /// assert_eq!(combined1, combined2);
    /// ```
    ///
    /// # Parameters
    ///
    /// - `other`: The hash value to add to this one
    ///
    /// # Returns
    ///
    /// The combined hash value using wrapping addition
    #[must_use]
    fn wrapping_add(self, other: Self) -> Self;
}

/// A trait for stable hashers that provide deterministic hashing behavior.
///
/// `StableHasher` is the core trait for implementing stable hashing. Unlike
/// standard hashers that may produce different results across runs, stable
/// hashers guarantee consistent output for the same input, making them suitable
/// for use in scenarios where reproducibility is required.
///
/// The hasher supports hierarchical hashing through the `sub_hash` method,
/// allowing for structured hashing of complex data types.
///
/// # Type Parameters
///
/// - `Hash`: The type of hash values produced by this hasher, must implement
///   [`Value`]
///
/// # Examples
///
/// ```rust
/// use stable_hash::{StableHasher, Value};
///
/// struct MyHasher {
///     state: u64,
/// }
///
/// impl StableHasher for MyHasher {
///     type Hash = u64;
///
///     fn finish(&self) -> u128 { self.state as u128 }
///
///     fn write(&mut self, bytes: &[u8]) {
///         // Implementation details...
///     }
///
///     fn sub_hash(
///         &mut self,
///         f: &mut dyn FnMut(&mut dyn StableHasher<Hash = Self::Hash>),
///     ) -> Self::Hash {
///         // Implementation details...
///         # 0
///     }
/// }
/// ```
pub trait StableHasher {
    /// The type of hash values produced by this hasher.
    ///
    /// This associated type must implement [`Value`] to support hash
    /// combination operations. Common choices are `u64` for performance or
    /// `u128` for reduced collision probability.
    type Hash: Value;

    /// Returns the final hash value as a 128-bit integer.
    ///
    /// This method should be called after all data has been written to the
    /// hasher to obtain the final hash result. The result is always
    /// returned as `u128` regardless of the internal hash type for
    /// consistency.
    ///
    /// # Returns
    ///
    /// The final hash value as a 128-bit unsigned integer
    fn finish(&self) -> u128;

    /// Writes a slice of bytes into the hasher.
    ///
    /// This is the fundamental method for feeding data into the hasher. All
    /// other data types should ultimately be converted to bytes and fed
    /// through this method to ensure consistent hashing behavior.
    ///
    /// # Parameters
    ///
    /// - `bytes`: The byte slice to hash
    fn write(&mut self, bytes: &[u8]);

    /// Writes a single `u8` value into the hasher.
    ///
    /// The default implementation converts the value to bytes using
    /// little-endian encoding for consistent cross-platform behavior.
    ///
    /// # Parameters
    ///
    /// - `i`: The `u8` value to hash
    fn write_u8(&mut self, i: u8) { self.write(&[i]); }

    /// Writes a single `i8` value into the hasher.
    ///
    /// The default implementation converts the value to bytes using
    /// little-endian encoding for consistent cross-platform behavior.
    ///
    /// # Parameters
    ///
    /// - `i`: The `i8` value to hash
    fn write_i8(&mut self, i: i8) { self.write(&i.to_le_bytes()); }

    /// Writes a single `u16` value into the hasher.
    ///
    /// The default implementation converts the value to bytes using
    /// little-endian encoding for consistent cross-platform behavior.
    ///
    /// # Parameters
    ///
    /// - `i`: The `u16` value to hash
    fn write_u16(&mut self, i: u16) { self.write(&i.to_le_bytes()); }

    /// Writes a single `i16` value into the hasher.
    ///
    /// The default implementation converts the value to bytes using
    /// little-endian encoding for consistent cross-platform behavior.
    ///
    /// # Parameters
    ///
    /// - `i`: The `i16` value to hash
    fn write_i16(&mut self, i: i16) { self.write(&i.to_le_bytes()); }

    /// Writes a single `u32` value into the hasher.
    ///
    /// The default implementation converts the value to bytes using
    /// little-endian encoding for consistent cross-platform behavior.
    ///
    /// # Parameters
    ///
    /// - `i`: The `u32` value to hash
    fn write_u32(&mut self, i: u32) { self.write(&i.to_le_bytes()); }

    /// Writes a single `i32` value into the hasher.
    ///
    /// The default implementation converts the value to bytes using
    /// little-endian encoding for consistent cross-platform behavior.
    ///
    /// # Parameters
    ///
    /// - `i`: The `i32` value to hash
    fn write_i32(&mut self, i: i32) { self.write(&i.to_le_bytes()); }

    /// Writes a single `u64` value into the hasher.
    ///
    /// The default implementation converts the value to bytes using
    /// little-endian encoding for consistent cross-platform behavior.
    ///
    /// # Parameters
    ///
    /// - `i`: The `u64` value to hash
    fn write_u64(&mut self, i: u64) { self.write(&i.to_le_bytes()); }

    /// Writes a single `i64` value into the hasher.
    ///
    /// The default implementation converts the value to bytes using
    /// little-endian encoding for consistent cross-platform behavior.
    ///
    /// # Parameters
    ///
    /// - `i`: The `i64` value to hash
    fn write_i64(&mut self, i: i64) { self.write(&i.to_le_bytes()); }

    /// Writes a single `u128` value into the hasher.
    ///
    /// The default implementation converts the value to bytes using
    /// little-endian encoding for consistent cross-platform behavior.
    ///
    /// # Parameters
    ///
    /// - `i`: The `u128` value to hash
    fn write_u128(&mut self, i: u128) { self.write(&i.to_le_bytes()); }

    /// Writes a single `i128` value into the hasher.
    ///
    /// The default implementation converts the value to bytes using
    /// little-endian encoding for consistent cross-platform behavior.
    ///
    /// # Parameters
    ///
    /// - `i`: The `i128` value to hash
    fn write_i128(&mut self, i: i128) { self.write(&i.to_le_bytes()); }

    /// Writes a single `usize` value into the hasher.
    ///
    /// The default implementation converts the value to bytes using
    /// little-endian encoding for consistent cross-platform behavior.
    ///
    /// # Parameters
    ///
    /// - `i`: The `usize` value to hash
    fn write_usize(&mut self, i: usize) { self.write(&i.to_le_bytes()); }

    /// Writes a single `isize` value into the hasher.
    ///
    /// The default implementation converts the value to bytes using
    /// little-endian encoding for consistent cross-platform behavior.
    ///
    /// # Parameters
    ///
    /// - `i`: The `isize` value to hash
    fn write_isize(&mut self, i: isize) { self.write(&i.to_le_bytes()); }

    /// Writes a single `f32` value into the hasher.
    ///
    /// The default implementation converts the value to bytes using
    /// little-endian encoding for consistent cross-platform behavior.
    /// Special handling is applied for NaN values to ensure deterministic
    /// behavior.
    ///
    /// # Parameters
    ///
    /// - `f`: The `f32` value to hash
    fn write_f32(&mut self, f: f32) {
        // Normalize NaN to ensure deterministic hashing
        let normalized = if f.is_nan() { f32::NAN } else { f };
        self.write(&normalized.to_le_bytes());
    }

    /// Writes a single `f64` value into the hasher.
    ///
    /// The default implementation converts the value to bytes using
    /// little-endian encoding for consistent cross-platform behavior.
    /// Special handling is applied for NaN values to ensure deterministic
    /// behavior.
    ///
    /// # Parameters
    ///
    /// - `f`: The `f64` value to hash
    fn write_f64(&mut self, f: f64) {
        // Normalize NaN to ensure deterministic hashing
        let normalized = if f.is_nan() { f64::NAN } else { f };
        self.write(&normalized.to_le_bytes());
    }

    /// Writes a length prefix for variable-length data.
    ///
    /// This method is used to hash a length value before hashing the actual
    /// data, ensuring that different-sized data with the same prefix don't
    /// collide. The default implementation writes the length as a `usize`
    /// using little-endian encoding.
    ///
    /// # Parameters
    ///
    /// - `len`: The length to write as a prefix
    ///
    /// # Examples
    ///
    /// ```rust
    /// use stable_hash::StableHasher;
    ///
    /// fn hash_slice<H: StableHasher>(hasher: &mut H, data: &[u8]) -> H::Hash {
    ///     hasher.sub_hash(&mut |sub| {
    ///         sub.write_length_prefix(data.len());
    ///         sub.write(data);
    ///     })
    /// }
    /// ```
    fn write_length_prefix(&mut self, len: usize) { self.write_usize(len); }

    /// Writes a string slice into the hasher.
    ///
    /// This method hashes a string by first writing its length as a prefix
    /// (to prevent collisions between strings that are prefixes of others)
    /// and then writing the UTF-8 bytes of the string. This ensures
    /// deterministic behavior and prevents hash collisions between strings
    /// like "ab" + "c" and "a" + "bc".
    ///
    /// # Design Choice: Length Prefix vs Suffix Delimiter
    ///
    /// Unlike Rust's standard `Hasher::write_str` which uses a `0xFF` suffix
    /// for performance reasons, this implementation uses a length prefix for
    /// several stability-focused reasons:
    ///
    /// 1. **Cross-platform consistency**: Length prefixes behave identically
    ///    across different hasher implementations and architectures
    /// 2. **Deterministic semantics**: The length-first approach makes data
    ///    boundaries explicit and predictable
    /// 3. **Stable hashing priority**: Consistency is more important than
    ///    micro-optimizations in stable hashing contexts
    ///
    /// # Parameters
    ///
    /// - `s`: The string slice to hash
    ///
    /// # Examples
    ///
    /// ```rust
    /// use stable_hash::StableHasher;
    ///
    /// fn hash_string<H: StableHasher>(hasher: &mut H, text: &str) -> H::Hash {
    ///     hasher.sub_hash(&mut |sub| {
    ///         sub.write_str(text);
    ///     })
    /// }
    /// ```
    ///
    /// # Implementation Details
    ///
    /// The default implementation is equivalent to:
    /// ```rust,ignore
    /// hasher.write_length_prefix(s.len());
    /// hasher.write(s.as_bytes());
    /// ```
    ///
    /// This approach guarantees that different strings always produce
    /// different hash input sequences, preventing prefix-based collisions.
    fn write_str(&mut self, s: &str) {
        self.write_length_prefix(s.len());
        self.write(s.as_bytes());
    }

    /// Computes a sub-hash using a nested hasher context.
    ///
    /// This method enables hierarchical hashing by creating a nested hashing
    /// context. The provided closure receives a mutable reference to a
    /// sub-hasher, allowing for structured hashing of complex data types
    /// while maintaining deterministic ordering.
    ///
    /// # Deterministic Requirements
    ///
    /// **CRITICAL**: This method MUST be deterministic across all invocations.
    /// For the same input data and hasher state, `sub_hash` must always:
    ///
    /// 1. **Produce identical results**: Multiple calls with the same inputs
    ///    must yield exactly the same hash value
    /// 2. **Maintain consistent sub-hasher behavior**: The sub-hasher provided
    ///    to the closure must behave identically across invocations
    /// 3. **Preserve state isolation**: Each sub-hash operation must not
    ///    interfere with other sub-hash operations or the parent hasher's state
    /// 4. **Ensure reproducible execution**: The same sequence of operations
    ///    within the closure must produce the same final hash
    ///
    /// Violating these requirements will break the stability guarantees of the
    /// entire hashing system, leading to inconsistent results across program
    /// runs, which is unacceptable for incremental compilation, serialization,
    /// and testing scenarios.
    ///
    /// # Implementation Guidelines
    ///
    /// When implementing this method:
    /// - Use deterministic initialization for the sub-hasher
    /// - Ensure the sub-hasher's internal state is fully isolated
    /// - Do not rely on non-deterministic sources (e.g., memory addresses,
    ///   timestamps)
    /// - Maintain consistent behavior regardless of system state or execution
    ///   context
    ///
    /// # Parameters
    ///
    /// - `f`: A closure that receives a mutable reference to a sub-hasher. This
    ///   closure must also be deterministic in its operations.
    ///
    /// # Returns
    ///
    /// The hash value computed by the sub-hasher, guaranteed to be identical
    /// for identical inputs across all invocations.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use stable_hash::StableHasher;
    ///
    /// fn hash_pair<H: StableHasher>(hasher: &mut H, a: u32, b: u32) -> H::Hash {
    ///     // This sub_hash call must produce the same result every time
    ///     // for the same values of a and b
    ///     hasher.sub_hash(&mut |sub_hasher| {
    ///         sub_hasher.write(&a.to_le_bytes());
    ///         sub_hasher.write(&b.to_le_bytes());
    ///     })
    /// }
    ///
    /// // Example of deterministic nested hashing
    /// fn hash_struct<H: StableHasher>(
    ///     hasher: &mut H,
    ///     name: &str,
    ///     id: u64,
    ///     active: bool,
    /// ) -> H::Hash {
    ///     hasher.sub_hash(&mut |sub| {
    ///         // Always hash in the same order for deterministic results
    ///         sub.write(name.as_bytes());
    ///         sub.write(&id.to_le_bytes());
    ///         sub.write(&[active as u8]);
    ///     })
    /// }
    /// ```
    ///
    /// # Counterexample (DO NOT DO)
    ///
    /// ```rust,ignore
    /// use stable_hash::StableHasher;
    /// use std::collections::HashMap;
    ///
    /// // BAD: Non-deterministic due to HashMap iteration order
    /// fn bad_hash_map<H: StableHasher>(hasher: &mut H, map: &HashMap<String, u32>) -> H::Hash {
    ///     hasher.sub_hash(&mut |sub| {
    ///         for (key, value) in map.iter() { // Order is not guaranteed!
    ///             sub.write(key.as_bytes());
    ///             sub.write(&value.to_le_bytes());
    ///         }
    ///     })
    /// }
    /// ```
    fn sub_hash(
        &mut self,
        f: &mut dyn FnMut(&mut dyn StableHasher<Hash = Self::Hash>),
    ) -> Self::Hash;
}

/// A trait for types that can be stably hashed.
///
/// Types implementing this trait can produce deterministic hash values that
/// remain consistent across different program runs and platforms. This is
/// essential for use cases like incremental compilation, serialization,
/// and testing.
///
/// # Implementation Guidelines
///
/// When implementing `StableHash` for your types:
///
/// 1. Hash all fields that contribute to the type's identity
/// 2. Use a consistent order for hashing fields
/// 3. Consider using `sub_hash` for complex nested structures
/// 4. Ensure the implementation is deterministic
///
/// # Examples
///
/// ```rust
/// use stable_hash::{StableHash, StableHasher};
///
/// struct Point {
///     x: f64,
///     y: f64,
/// }
///
/// impl StableHash for Point {
///     fn stable_hash<H: StableHasher>(&self, state: &mut H) -> H::Hash {
///         state.sub_hash(&mut |sub_state| {
///             sub_state.write(&self.x.to_le_bytes());
///             sub_state.write(&self.y.to_le_bytes());
///         })
///     }
/// }
/// ```
pub trait StableHash {
    /// Computes a stable hash of this value.
    ///
    /// This method feeds the value's data into the provided hasher in a
    /// deterministic manner, producing a stable hash that will be consistent
    /// across different program runs.
    ///
    /// # Type Parameters
    ///
    /// - `H`: The type of hasher to use, must implement [`StableHasher`]
    ///
    /// # Parameters
    ///
    /// - `state`: A mutable reference to the hasher
    ///
    /// # Returns
    ///
    /// The hash value computed by the hasher
    fn stable_hash<H: StableHasher>(&self, state: &mut H) -> H::Hash;
}

static_assertions::assert_obj_safe!(StableHasher<Hash = u128>);
static_assertions::assert_obj_safe!(StableHasher<Hash = u64>);

/// Implementations of `Value` for unsigned integer types.
///
/// These implementations enable using standard unsigned integer types as hash
/// values in the stable hashing system. All implementations use the built-in
/// `wrapping_add` method to ensure consistent overflow behavior.
impl Value for u8 {
    #[must_use]
    fn wrapping_add(self, other: Self) -> Self { self.wrapping_add(other) }
}

impl Value for u16 {
    #[must_use]
    fn wrapping_add(self, other: Self) -> Self { self.wrapping_add(other) }
}

impl Value for u32 {
    #[must_use]
    fn wrapping_add(self, other: Self) -> Self { self.wrapping_add(other) }
}

impl Value for u64 {
    #[must_use]
    fn wrapping_add(self, other: Self) -> Self { self.wrapping_add(other) }
}

impl Value for u128 {
    #[must_use]
    fn wrapping_add(self, other: Self) -> Self { self.wrapping_add(other) }
}

impl Value for usize {
    #[must_use]
    fn wrapping_add(self, other: Self) -> Self { self.wrapping_add(other) }
}

/// A stable hasher implementation using SipHash-2-4 algorithm.
///
/// This implementation provides cryptographically secure hashing with excellent
/// performance characteristics, making it ideal for critical fingerprinting and
/// equality checking scenarios. The hasher produces 128-bit hashes for maximum
/// collision resistance.
///
/// # Features
///
/// - **Cryptographically secure**: Resistant to hash-flooding attacks
/// - **Deterministic**: Always produces the same hash for the same input
/// - **128-bit output**: Provides excellent collision resistance
/// - **Cross-platform consistent**: Identical behavior across all platforms
///
/// # Examples
///
/// ```rust
/// use stable_hash::{StableHash, StableHasher, StableSipHasher};
///
/// let mut hasher = StableSipHasher::new();
/// hasher.write_str("hello world");
/// let hash = hasher.finish();
///
/// // Or using the StableHash trait
/// let hash = "hello world".stable_hash(&mut StableSipHasher::new());
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
    /// Creates a new `StableSipHasher` with deterministic seeds.
    ///
    /// This constructor uses fixed, deterministic seeds to ensure that
    /// hash values are consistent across different program runs and
    /// platforms. This is essential for stable hashing requirements.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use stable_hash::StableSipHasher;
    ///
    /// let hasher = StableSipHasher::new();
    /// ```
    #[must_use]
    pub const fn new() -> Self {
        Self::new_with_keys(0x736f_6d65_7073_6575, 0x646f_7261_6e64_6f6d)
    }

    /// Creates a new `StableSipHasher` with custom keys.
    ///
    /// This allows for custom initialization while maintaining deterministic
    /// behavior. The keys should be fixed constants to ensure stability.
    ///
    /// # Parameters
    ///
    /// - `key0`: First 64-bit key
    /// - `key1`: Second 64-bit key
    ///
    /// # Examples
    ///
    /// ```rust
    /// use stable_hash::StableSipHasher;
    ///
    /// let hasher =
    ///     StableSipHasher::new_with_keys(0x1234567890abcdef, 0xfedcba0987654321);
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

        // XOR in the 128-bit output flag
        hasher.v1 ^= 0xee;

        hasher
    }

    /// Creates a fresh hasher for sub-hashing operations.
    ///
    /// This method creates a new hasher instance with deterministic
    /// initialization suitable for use in `sub_hash` operations.
    /// The hasher is isolated from the parent hasher state.
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

        // SipHash-2-4: 2 compression rounds
        self.sipround();
        self.sipround();

        self.v0 ^= block;
    }

    fn finish_128(&self) -> u128 {
        let mut hasher = *self;

        // Process remaining tail bytes
        let mut tail = hasher.tail;
        tail |= (hasher.processed.wrapping_add(hasher.ntail) as u64) << 56;

        hasher.process_block(tail);

        // Finalization: XOR in 0xff and run 4 rounds
        hasher.v2 ^= 0xff;

        // SipHash-2-4: 4 finalization rounds
        hasher.sipround();
        hasher.sipround();
        hasher.sipround();
        hasher.sipround();

        let first_half = hasher.v0 ^ hasher.v1 ^ hasher.v2 ^ hasher.v3;

        // Second half for 128-bit output
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

        // Process any remaining tail bytes first
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

        // Process full 8-byte blocks
        while bytes.len() >= 8 {
            let block = u64::from_le_bytes([
                bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5],
                bytes[6], bytes[7],
            ]);
            self.process_block(block);
            self.processed = self.processed.wrapping_add(8);
            bytes = &bytes[8..];
        }

        // Store remaining bytes in tail
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
