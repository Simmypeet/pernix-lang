//! Contains multiple important traits used by the query system

use std::hash::Hash;

use pernixc_arena::ID;
use pernixc_target::Global;
use serde::{Deserialize, Serialize};

/// A tag struct used for signifying that a key is an input key.
///
/// An input key is a key that has the value explicitly set by the user and
/// can be thought of the starting point of the whole query system.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Input;

/// A stable alternative to [`std::any::TypeId`] that is used to uniquely
/// identify types in a way that is consistent across different runs of the
/// compiler.
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
)]
pub struct StableTypeID(pub u64, pub u64);

impl StableTypeID {
    /// Creates a [`StableTypeID`] from a unique type name.
    #[must_use]
    pub const fn from_unique_type_name(name: &'static str) -> Self {
        // Implementation of a const-compatible 128-bit hash function
        // Using a modified SipHash-like algorithm optimized for collision
        // resistance

        const K0: u64 = 0x736f_6d65_7073_6575;
        const K1: u64 = 0x646f_7261_6e64_6f6d;
        const K2: u64 = 0x6c79_6765_6e65_7261;
        const K3: u64 = 0x7465_6462_7974_6573;

        let bytes = name.as_bytes();
        let len = bytes.len();

        // Initialize state with keys
        let mut v0 = K0;
        let mut v1 = K1;
        let mut v2 = K2;
        let mut v3 = K3;

        // Mix in the length
        v0 ^= len as u64;
        v1 ^= (len as u64).wrapping_mul(0x9e37_79b9_7f4a_7c15);

        // Process 8-byte chunks
        let mut i = 0;
        while i + 8 <= len {
            let chunk = Self::read_u64_le(bytes, i);
            v0 ^= chunk;
            Self::sipround(&mut v0, &mut v1, &mut v2, &mut v3);
            Self::sipround(&mut v0, &mut v1, &mut v2, &mut v3);
            v3 ^= chunk;
            i += 8;
        }

        // Handle remaining bytes
        let mut tail = 0u64;
        let mut shift = 0;
        while i < len {
            tail |= (bytes[i] as u64) << shift;
            shift += 8;
            i += 1;
        }

        // Finalize with tail
        v0 ^= tail;
        Self::sipround(&mut v0, &mut v1, &mut v2, &mut v3);
        Self::sipround(&mut v0, &mut v1, &mut v2, &mut v3);
        v3 ^= tail;

        // Additional rounds for security
        let mut round = 0;
        while round < 4 {
            Self::sipround(&mut v0, &mut v1, &mut v2, &mut v3);
            round += 1;
        }

        // Final mixing to ensure good avalanche properties
        v0 ^= v2;
        v1 ^= v3;
        Self::sipround(&mut v0, &mut v1, &mut v2, &mut v3);
        Self::sipround(&mut v0, &mut v1, &mut v2, &mut v3);

        let hash1 = v0 ^ v1;
        let hash2 = v2 ^ v3;

        Self(hash1, hash2)
    }

    /// Combines two [`StableTypeID`]s into a new one, ensuring that the
    /// combination is collision-resistant and has good avalanche properties.
    #[must_use]
    pub const fn combine(self, other: Self) -> Self {
        // Use a collision-resistant approach to combine two 128-bit hashes
        // We treat each hash as a pair of 64-bit values and use a mixing
        // function similar to our SipHash implementation to ensure good
        // avalanche properties

        // Initialize with distinct constants to avoid symmetry issues
        let mut v0 = self.0 ^ 0x736f_6d65_7073_6575;
        let mut v1 = self.1 ^ 0x646f_7261_6e64_6f6d;
        let mut v2 = other.0 ^ 0x6c79_6765_6e65_7261;
        let mut v3 = other.1 ^ 0x7465_6462_7974_6573;

        // Mix the values to prevent simple XOR attacks and ensure avalanche
        Self::sipround(&mut v0, &mut v1, &mut v2, &mut v3);
        Self::sipround(&mut v0, &mut v1, &mut v2, &mut v3);

        // Add asymmetry to prevent combine(a, b) == combine(b, a)
        v0 ^= 0x1f83_d9ab_fb41_bd6b; // Different constant for ordering sensitivity
        v1 ^= 0x5be0_cd19_137e_2179;

        Self::sipround(&mut v0, &mut v1, &mut v2, &mut v3);
        Self::sipround(&mut v0, &mut v1, &mut v2, &mut v3);

        // Final mixing with cross-dependencies to maximize avalanche
        v0 ^= v2;
        v1 ^= v3;
        v2 ^= v0.wrapping_mul(0x9e37_79b9_7f4a_7c15);
        v3 ^= v1.wrapping_mul(0xc2b2_ae35_86d4_0f00);

        Self::sipround(&mut v0, &mut v1, &mut v2, &mut v3);

        let hash1 = v0 ^ v1;
        let hash2 = v2 ^ v3;

        Self(hash1, hash2)
    }

    const fn read_u64_le(bytes: &[u8], start: usize) -> u64 {
        // Read 8 bytes as little-endian u64 from specific start position
        (bytes[start] as u64)
            | ((bytes[start + 1] as u64) << 8)
            | ((bytes[start + 2] as u64) << 16)
            | ((bytes[start + 3] as u64) << 24)
            | ((bytes[start + 4] as u64) << 32)
            | ((bytes[start + 5] as u64) << 40)
            | ((bytes[start + 6] as u64) << 48)
            | ((bytes[start + 7] as u64) << 56)
    }

    const fn sipround(v0: &mut u64, v1: &mut u64, v2: &mut u64, v3: &mut u64) {
        *v0 = v0.wrapping_add(*v1);
        *v1 = v1.rotate_left(13);
        *v1 ^= *v0;
        *v0 = v0.rotate_left(32);

        *v2 = v2.wrapping_add(*v3);
        *v3 = v3.rotate_left(16);
        *v3 ^= *v2;

        *v0 = v0.wrapping_add(*v3);
        *v3 = v3.rotate_left(21);
        *v3 ^= *v0;

        *v2 = v2.wrapping_add(*v1);
        *v1 = v1.rotate_left(17);
        *v1 ^= *v2;
        *v2 = v2.rotate_left(32);
    }
}

/// A trait representing a key that can be used to store and retrieve values
/// inside a [`map::Map`].
///
/// To implement this trait, use the [`Key`] derive macro. This will
/// automatically implement the [`Key`] trait for your type. The derive macro
/// will also generate a unique type name for the key.
///
/// ``` ignore
/// #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Key)]
/// #[value(MyValue)]
/// pub struct MyKey;
/// ```
pub trait Key:
    'static
    + Send
    + Sync
    + Eq
    + Clone
    + std::hash::Hash
    + Serialize
    + for<'de> Deserialize<'de>
{
    /// The corresponding value type for this key
    type Value: 'static
        + Send
        + Sync
        + Clone
        + Default
        + Eq
        + Serialize
        + for<'de> Deserialize<'de>;

    /// The stable type ID for this key, used for serialization and
    /// deserialization purposes.
    const STABLE_TYPE_ID: StableTypeID;

    /// Merges a new value with an existing value for this key.
    ///
    /// This method is called when attempting to set a value for a key that
    /// already has a value in the database. The default implementation
    /// performs an equality check and returns an error if the values
    /// differ, ensuring data consistency.
    ///
    /// # Arguments
    ///
    /// * `old` - A mutable reference to the existing value that will be updated
    /// * `new` - The new value to merge with the existing one
    ///
    /// # Returns
    ///
    /// * `Ok(())` if the merge was successful (values are equal in the default
    ///   implementation)
    /// * `Err(String)` if the merge failed due to incompatible values, with an
    ///   error message
    ///
    /// # Default Behavior
    ///
    /// The default implementation compares the old and new values for equality:
    /// - If they are equal, the merge succeeds without modifying the old value
    /// - If they differ, an error is returned indicating incompatible values
    ///
    /// # Custom Implementations
    ///
    /// Types can override this method to implement custom merge logic, such as:
    /// - Combining numeric values (e.g., summing, taking maximum)
    /// - Merging collections (e.g., union of sets, concatenation of lists)
    /// - Applying domain-specific merge strategies
    ///
    /// # Examples
    ///
    /// ```ignore
    /// // Default behavior - values must be equal
    /// let mut old_value = 42;
    /// assert!(MyKey::merge_value(&mut old_value, 42).is_ok());
    /// assert!(MyKey::merge_value(&mut old_value, 24).is_err());
    ///
    /// // Custom implementation for additive merging
    /// impl Key for AdditiveKey {
    ///     type Value = i32;
    ///
    ///     fn merge_value(old: &mut Self::Value, new: Self::Value) -> Result<(), String> {
    ///         *old += new;
    ///         Ok(())
    ///     }
    /// }
    /// ```
    fn merge_value(
        old: &mut Self::Value,
        new: Self::Value,
    ) -> Result<(), String> {
        if old == &new {
            Ok(())
        } else {
            Err(format!(
                "Encountered an incompatible value for key {}",
                std::any::type_name::<Self>()
            ))
        }
    }
}

/// A type alias for a [`smallbox::SmallBox`] with a [`Global<ID<()>>`] as its
/// size for the local storage.
///
/// This smallbox is used mainly for performance reasons to avoid heap
/// allocation (premature optimization?). Since most of the queries are just
/// global IDs, the [`Global<ID<()>>`] should be enough to store the data
/// without allocating a heap object.
pub type SmallBox<T> = smallbox::SmallBox<T, Global<ID<()>>>;

/// A trait allowing store multiple types of as a key in a hashmap. This is
/// automatically implemented for all types that implement the [`Key`] trait.
#[doc(hidden)]
pub trait Dynamic {
    #[doc(hidden)]
    fn any(&self) -> &dyn std::any::Any;
    #[doc(hidden)]
    fn eq(&self, other: &dyn Dynamic) -> bool;
    #[doc(hidden)]
    fn hash(&self, state: &mut dyn std::hash::Hasher);
    #[doc(hidden)]
    fn smallbox_clone(&self) -> SmallBox<dyn Dynamic>;
    #[doc(hidden)]
    fn stable_type_id(&self) -> StableTypeID;
    #[doc(hidden)]
    fn type_name(&self) -> &'static str;
}

/// A new type wrapper around [`SmallBox<dyn Dynamic>`] that allows it to be
/// serializable and deserializable.
#[derive(
    Debug, PartialEq, Eq, Hash, derive_more::Deref, derive_more::DerefMut,
)]
pub struct DynamicBox(pub SmallBox<dyn Dynamic>);

impl Clone for DynamicBox {
    fn clone(&self) -> Self {
        Self(self.0.smallbox_clone())
    }
}

impl<K: Key> Dynamic for K {
    fn any(&self) -> &dyn std::any::Any {
        self as &dyn std::any::Any
    }

    fn eq(&self, other: &dyn Dynamic) -> bool {
        other.any().downcast_ref::<Self>().is_some_and(|other| self.eq(other))
    }

    fn hash(&self, mut state: &mut dyn std::hash::Hasher) {
        let id = std::any::TypeId::of::<Self>();

        id.hash(&mut state);
        Hash::hash(self, &mut state);
    }

    fn smallbox_clone(&self) -> SmallBox<dyn Dynamic> {
        smallbox::smallbox!(self.clone())
    }

    fn stable_type_id(&self) -> StableTypeID {
        Self::STABLE_TYPE_ID
    }

    fn type_name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }
}

impl std::fmt::Debug for dyn Dynamic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Dynamic").field(&self.any()).finish_non_exhaustive()
    }
}

impl PartialEq for dyn Dynamic {
    fn eq(&self, other: &Self) -> bool {
        Dynamic::eq(self, other)
    }
}

impl Eq for dyn Dynamic {}

impl Hash for dyn Dynamic {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Dynamic::hash(self, state);
    }
}
