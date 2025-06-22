//! Contains the definition of the [`StableTypeID`] type and [`Identifiable`]
//! trait.

pub use pernixc_identifiable_derive::Identifiable;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

/// A stable alternative to [`std::any::TypeId`] that is used to uniquely
/// identify types in a way that is consistent across different runs of the
/// compiler.
///
/// Unlike [`std::any::TypeId`], which can vary between different executions
/// of the same program, `StableTypeID` provides deterministic type
/// identification that remains consistent across:
/// - Different compiler runs
/// - Different machines
/// - Different deployments
///
/// This makes it suitable for incremental compilation systems, caching
/// mechanisms, and serialization scenarios where type identity must be
/// preserved.
///
/// # Construction
///
/// `StableTypeID` can be created in several ways:
///
/// 1. **From a unique type name** (recommended):
///    [`from_unique_type_name`](Self::from_unique_type_name)
/// 2. **From raw parts** (unsafe): [`from_raw_parts`](Self::from_raw_parts)
/// 3. **By combining existing IDs**: [`combine`](Self::combine)
/// 4. **Through the derive macro** (most convenient): `#[derive(Identifiable)]`
///
/// # Hash Quality
///
/// The internal hash function is based on `SipHash` and provides:
/// - **Collision resistance**: Extremely low probability of hash collisions
/// - **Avalanche effect**: Small input changes result in dramatically different
///   outputs
/// - **Deterministic output**: Same input always produces the same hash
/// - **Fast computation**: Optimized for compile-time evaluation
///
/// # Examples
///
/// ## Basic Usage
///
/// ```rust
/// use pernixc_stable_type_id::StableTypeID;
///
/// // Create from a unique type name
/// let id = StableTypeID::from_unique_type_name("myapp@1.0.0::models::User");
///
/// // IDs are consistent
/// let same_id =
///     StableTypeID::from_unique_type_name("myapp@1.0.0::models::User");
/// assert_eq!(id, same_id);
/// ```
///
/// ## Combining IDs
///
/// ```rust
/// use pernixc_stable_type_id::StableTypeID;
///
/// let base_id = StableTypeID::from_unique_type_name("Container");
/// let param_id = StableTypeID::from_unique_type_name("String");
/// let combined = base_id.combine(param_id);
///
/// // Order matters for combination
/// let reversed = param_id.combine(base_id);
/// assert_ne!(combined, reversed);
/// ```
///
/// ## Using with the Derive Macro
///
/// ```rust
/// use pernixc_stable_type_id::Identifiable;
///
/// #[derive(Identifiable)]
/// struct MyType {
///     field: i32,
/// }
///
/// let id = MyType::STABLE_TYPE_ID;
/// ```
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
)]
#[allow(clippy::unsafe_derive_deserialize)]
pub struct StableTypeID(u64, u64);

impl StableTypeID {
    /// Constructs a new [`StableTypeID`] from two 64-bit integers.
    ///
    /// # Safety
    ///
    /// You should consider using the derive [`Identifiable`] trait instead,
    /// which provides a safer and more idiomatic way to create stable type IDs.
    ///
    /// This function is unsafe because it allows creating a `StableTypeID`
    /// from arbitrary values, which could lead to collisions if not used
    /// carefully. Only use this if you have a specific need to construct
    /// IDs from known hash values and can guarantee uniqueness.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use pernixc_stable_type_id::StableTypeID;
    ///
    /// // Only use if you have pre-computed hash values
    /// let id = unsafe {
    ///     StableTypeID::from_raw_parts(
    ///         0x1234_5678_9abc_def0,
    ///         0xfed_cba98_7654_3210,
    ///     )
    /// };
    /// ```
    #[must_use]
    pub const unsafe fn from_raw_parts(high: u64, low: u64) -> Self {
        // This function is unsafe because it allows creating a StableTypeID
        // from raw parts, which could lead to collisions if not used carefully.
        Self(high, low)
    }

    /// Creates a [`StableTypeID`] from a unique type name.
    ///
    /// This is the primary method for generating stable type IDs. The input
    /// should be a unique string that identifies a specific type, typically
    /// including package information, module path, and type name.
    ///
    /// # Algorithm
    ///
    /// Uses a const-compatible 128-bit hash function based on `SipHash` that
    /// provides excellent collision resistance and avalanche properties.
    /// The algorithm is optimized for compile-time evaluation.
    ///
    /// # Recommended Format
    ///
    /// For maximum uniqueness, use the format:
    /// `"package@version::module::path::TypeName"`
    ///
    /// # Examples
    ///
    /// ```rust
    /// use pernixc_stable_type_id::StableTypeID;
    ///
    /// // Recommended format with full qualification
    /// let user_id =
    ///     StableTypeID::from_unique_type_name("myapp@1.0.0::models::user::User");
    ///
    /// // For generic types, include concrete type information
    /// let vec_string_id = StableTypeID::from_unique_type_name(
    ///     "std@1.0.0::vec::Vec<alloc::string::String>",
    /// );
    ///
    /// // Same input always produces same output
    /// let same_id =
    ///     StableTypeID::from_unique_type_name("myapp@1.0.0::models::user::User");
    /// assert_eq!(user_id, same_id);
    /// ```
    ///
    /// # Collision Resistance
    ///
    /// The probability of two different type names producing the same ID is
    /// astronomically low (approximately 2^-128), making accidental collisions
    /// practically impossible in real-world usage.
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
    ///
    /// This method is primarily used for constructing stable IDs for generic
    /// types by combining a base type ID with the IDs of its type parameters.
    ///
    /// # Properties
    ///
    /// - **Non-commutative**: `a.combine(b) != b.combine(a)` (order matters)
    /// - **Collision-resistant**: Different input pairs produce different
    ///   outputs
    /// - **Avalanche effect**: Small changes in input cause large changes in
    ///   output
    /// - **Deterministic**: Same inputs always produce the same output
    ///
    /// # Algorithm
    ///
    /// Uses a modified `SipHash`-like mixing function that:
    /// 1. Initializes state with the input IDs and distinct constants
    /// 2. Applies multiple rounds of mixing to ensure good avalanche properties
    /// 3. Adds asymmetry to distinguish order (prevents `combine(a,b) ==
    ///    combine(b,a)`)
    /// 4. Performs final cross-mixing to maximize collision resistance
    ///
    /// # Examples
    ///
    /// ```rust
    /// use pernixc_stable_type_id::StableTypeID;
    ///
    /// let vec_id = StableTypeID::from_unique_type_name("std::vec::Vec");
    /// let string_id = StableTypeID::from_unique_type_name("std::string::String");
    /// let i32_id = StableTypeID::from_unique_type_name("i32");
    ///
    /// // Create IDs for Vec<String> and Vec<i32>
    /// let vec_string = vec_id.combine(string_id);
    /// let vec_i32 = vec_id.combine(i32_id);
    ///
    /// // Different combinations produce different results
    /// assert_ne!(vec_string, vec_i32);
    ///
    /// // Order matters
    /// let string_vec = string_id.combine(vec_id);
    /// assert_ne!(vec_string, string_vec);
    /// ```
    ///
    /// # Usage in Generic Types
    ///
    /// For a generic type `Container<T, U>`, the stable ID would be computed
    /// as:
    ///
    /// ```rust
    /// # use pernixc_stable_type_id::StableTypeID;
    /// # let container_base = StableTypeID::from_unique_type_name("Container");
    /// # let t_id = StableTypeID::from_unique_type_name("T");
    /// # let u_id = StableTypeID::from_unique_type_name("U");
    /// let combined_id = container_base.combine(t_id).combine(u_id);
    /// ```
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

    /// Returns the high 64 bits of the stable type ID.
    #[must_use]
    pub const fn high(&self) -> u64 {
        // Returns the high 64 bits of the stable type ID
        self.0
    }

    /// Returns the low 64 bits of the stable type ID.
    #[must_use]
    pub const fn low(&self) -> u64 {
        // Returns the low 64 bits of the stable type ID
        self.1
    }

    /// Converts the stable type ID into a 128-bit integer representation.
    #[must_use]
    pub const fn as_u128(&self) -> u128 {
        // Returns the stable type ID as a 128-bit integer
        ((self.0 as u128) << 64) | (self.1 as u128)
    }
}

/// A trait for types that can provide a stable, unique identifier across
/// different compiler runs.
///
/// This trait is designed to be implemented for types that need a consistent
/// identity that doesn't change between different executions of the compiler,
/// unlike [`std::any::TypeId`] which can vary between runs.
///
/// # Stability Guarantees
///
/// The [`STABLE_TYPE_ID`](Self::STABLE_TYPE_ID) must remain consistent across:
/// - Different compiler runs
/// - Different machines
/// - Different versions of the same code (as long as the type structure doesn't
///   change)
///
/// # Implementation
///
/// While you can implement this trait manually, it's strongly recommended to
/// use the `#[derive(Identifiable)]` macro instead, which automatically
/// generates collision-resistant stable IDs based on:
/// - Package name and version
/// - Module path
/// - Type name
/// - Generic type parameters (if any)
///
/// # Examples
///
/// ## Manual Implementation
///
/// ```rust
/// use pernixc_stable_type_id::{Identifiable, StableTypeID};
///
/// struct MyType;
///
/// impl Identifiable for MyType {
///     const STABLE_TYPE_ID: StableTypeID =
///         StableTypeID::from_unique_type_name(
///             "mypackage@1.0.0::mymodule::MyType",
///         );
/// }
/// ```
///
/// ## Using the Derive Macro (Recommended)
///
/// ```rust
/// use pernixc_stable_type_id::Identifiable;
///
/// #[derive(Identifiable)]
/// struct MyType {
///     field: i32,
/// }
///
/// #[derive(Identifiable)]
/// struct GenericType<T: Identifiable> {
///     value: T,
/// }
/// ```
///
/// # Generic Types
///
/// For generic types, the stable ID includes information about the concrete
/// type parameters. This means `Vec<i32>` and `Vec<String>` will have
/// different stable IDs.
///
/// # Thread Safety
///
/// This trait is inherently thread-safe as it only provides a constant value.
/// The [`StableTypeID`] itself is [`Send`] and [`Sync`].
#[diagnostic::on_unimplemented(
    message = "The type `{Self}` does not implement `Identifiable`",
    note = "You can derive `Identifiable` using the `#[derive(Identifiable)]` \
            macro",
    label = "`Identifiable` is required for stable type identification"
)]
pub trait Identifiable {
    /// Returns the stable type ID of the type.
    ///
    /// This constant provides a unique identifier for the type that remains
    /// consistent across different compiler runs and environments.
    ///
    /// # Collision Resistance
    ///
    /// The ID is generated using a cryptographically-inspired hash function
    /// that provides strong collision resistance. The probability of two
    /// different types having the same ID is astronomically low.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use pernixc_stable_type_id::Identifiable;
    ///
    /// #[derive(Identifiable)]
    /// struct MyStruct;
    ///
    /// // The ID is the same across different uses
    /// let id1 = MyStruct::STABLE_TYPE_ID;
    /// let id2 = <MyStruct as Identifiable>::STABLE_TYPE_ID;
    /// assert_eq!(id1, id2);
    /// ```
    const STABLE_TYPE_ID: StableTypeID;
}
