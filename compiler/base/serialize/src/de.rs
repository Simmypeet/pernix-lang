//! Custom deserialization framework for the Pernix compiler.
//!
//! This module provides a custom deserialization framework similar to standard
//! serde, but simplified without a visitor pattern and with an extension
//! mechanism for easier customization and state passing. The framework is
//! designed to handle complex deserialization scenarios where additional
//! context or state needs to be maintained during deserialization.
//!
//! ## Key Traits
//!
//! - [`Deserializer`] - The main trait for types that can deserialize Rust data
//!   structures
//! - [`Deserialize`] - Trait for types that can be deserialized using a
//!   deserializer
//! - [`SeqAccess`], [`TupleAccess`], [`TupleStructAccess`], [`StructAccess`],
//!   [`MapAccess`] - Compound data structure deserializers
//! - [`TupleVariantAccess`], [`StructVariantAccess`] - Enum variant
//!   deserializers
//!
//! ## Extension Mechanism
//!
//! The [`Deserializer::Extension`] associated type allows for specialized
//! deserialization behavior. Extensions can maintain state across
//! deserialization operations and provide custom handling for specific types
//! like shared pointers.

// Extension mechanism for custom deserialization behavior.
// pub mod extension;

/// An identifier for fields or enum variants.
///
/// This enum allows identification by either a numeric index or a string name,
/// providing flexibility for different serialization formats.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Identifier {
    /// Identification by numeric index (0-based).
    Index(u32),
    /// Identification by string name.
    Name(&'static str),
}

impl Identifier {
    /// Creates an identifier from an index.
    #[inline]
    #[must_use]
    pub const fn from_index(index: u32) -> Self { Self::Index(index) }

    /// Creates an identifier from a name.
    #[inline]
    #[must_use]
    pub const fn from_name(name: &'static str) -> Self { Self::Name(name) }

    /// Returns the index if this identifier is an index.
    #[inline]
    #[must_use]
    pub const fn as_index(&self) -> Option<u32> {
        match self {
            Self::Index(index) => Some(*index),
            Self::Name(_) => None,
        }
    }

    /// Returns the name if this identifier is a name.
    #[inline]
    #[must_use]
    pub const fn as_name(&self) -> Option<&'static str> {
        match self {
            Self::Index(_) => None,
            Self::Name(name) => Some(*name),
        }
    }
}

impl From<u32> for Identifier {
    fn from(index: u32) -> Self { Self::Index(index) }
}

impl From<&'static str> for Identifier {
    fn from(name: &'static str) -> Self { Self::Name(name) }
}

/// A trait for deserializing sequences (arrays, vectors, etc.).
///
/// This trait is used to deserialize ordered collections of elements where
/// all elements are of the same type or can be deserialized with the same
/// deserializer.
pub trait SeqAccess {
    /// The parent deserializer type that created this sequence access.
    type Parent: Deserializer;

    /// Deserialize the next element in the sequence.
    ///
    /// Returns `Ok(Some(element))` if there is a next element,
    /// `Ok(None)` if the sequence is finished, or an error if
    /// deserialization fails.
    fn next_element<T: Deserialize<Self::Parent>>(
        &mut self,
    ) -> Result<Option<T>, <Self::Parent as Deserializer>::Error>;

    /// Get the size hint for the remaining elements.
    ///
    /// Returns `(lower_bound, upper_bound)` where `upper_bound` is `None`
    /// if the exact size is unknown.
    fn size_hint(&self) -> (usize, Option<usize>);
}

/// A trait for deserializing tuples.
///
/// This trait is used to deserialize fixed-size ordered collections where
/// elements may be of different types but the number of elements is known at
/// compile time.
pub trait TupleAccess {
    /// The parent deserializer type that created this tuple access.
    type Parent: Deserializer;

    /// Deserialize the next element in the tuple.
    ///
    /// Returns `Ok(element)` if successful, or an error if deserialization
    /// fails.
    fn next_element<T: Deserialize<Self::Parent>>(
        &mut self,
    ) -> Result<T, <Self::Parent as Deserializer>::Error>;
}

/// A trait for deserializing tuple structs.
///
/// Tuple structs are structs with unnamed fields accessed by position,
/// similar to tuples but with a named type.
pub trait TupleStructAccess {
    /// The parent deserializer type that created this tuple struct access.
    type Parent: Deserializer;

    /// Deserialize the next field in the tuple struct.
    ///
    /// Returns `Ok(field)` if successful, or an error if deserialization fails.
    fn next_field<T: Deserialize<Self::Parent>>(
        &mut self,
    ) -> Result<T, <Self::Parent as Deserializer>::Error>;
}

/// A trait for accessing a specific field during struct deserialization.
///
/// This trait represents access to a single field that can be deserialized
/// on demand.
pub trait FieldAccess {
    /// The parent deserializer type that created this field access.
    type Parent: Deserializer;

    /// Deserialize the field value.
    ///
    /// Returns the deserialized field value, or an error if deserialization
    /// fails.
    fn deserialize<T: Deserialize<Self::Parent>>(
        self,
    ) -> Result<T, <Self::Parent as Deserializer>::Error>;
}

/// A trait for deserializing structs with named fields.
///
/// This trait handles the deserialization of structures where each field
/// has a name and can be accessed by name.
pub trait StructAccess {
    /// The parent deserializer type that created this struct access.
    type Parent: Deserializer;

    /// The type used for accessing individual fields.
    type FieldAccess<'s>: FieldAccess<Parent = Self::Parent>;

    /// Process the next field in the struct.
    ///
    /// This method allows examining the next field's identifier and accessing
    /// its value through the provided closure. The closure receives
    /// `Some((field_identifier, field_access))` if there is a next field,
    /// or `None` if all fields have been consumed.
    ///
    /// # Arguments
    ///
    /// * `next` - A closure that receives the field information and access
    ///
    /// Returns the result of the closure, or an error if deserialization fails.
    fn next_field<'s, R>(
        &'s mut self,
        next: impl FnOnce(
            Option<(Identifier, Self::FieldAccess<'s>)>,
        )
            -> Result<R, <Self::Parent as Deserializer>::Error>,
    ) -> Result<R, <Self::Parent as Deserializer>::Error>;
}

/// A trait for accessing a specific value during map deserialization.
///
/// This trait represents access to a single map value that can be deserialized
/// on demand after the key has been examined.
pub trait ValueAccess {
    /// The parent deserializer type that created this value access.
    type Parent: Deserializer;

    /// Deserialize the value.
    ///
    /// Returns the deserialized value, or an error if deserialization fails.
    fn deserialize<V: Deserialize<Self::Parent>>(
        self,
    ) -> Result<V, <Self::Parent as Deserializer>::Error>;
}

/// A trait for deserializing maps (dictionaries, hash tables, etc.).
///
/// Maps are collections of key-value pairs where keys and values
/// can be of different types.
pub trait MapAccess {
    /// The parent deserializer type that created this map access.
    type Parent: Deserializer;

    /// The type used for accessing individual values after key examination.
    type ValueAccess<'s>: ValueAccess<Parent = Self::Parent>;

    /// Process the next entry in the map.
    ///
    /// This method allows examining the next entry's key and accessing
    /// its value through the provided closure. The closure receives
    /// `Some((key, value_access))` if there is a next entry,
    /// or `None` if all entries have been consumed.
    ///
    /// # Arguments
    ///
    /// * `next` - A closure that receives the key and value access
    ///
    /// # Returns
    ///
    /// Returns the result of the closure, or an error if deserialization fails.
    fn next_entry<'s, K: Deserialize<Self::Parent>, R>(
        &'s mut self,
        next: impl FnOnce(
            Option<(K, Self::ValueAccess<'s>)>,
        )
            -> Result<R, <Self::Parent as Deserializer>::Error>,
    ) -> Result<R, <Self::Parent as Deserializer>::Error>;

    /// Get the size hint for the remaining entries.
    ///
    /// Returns `(lower_bound, upper_bound)` where `upper_bound` is `None`
    /// if the exact size is unknown.
    fn size_hint(&self) -> (usize, Option<usize>);
}

/// A trait for deserializing tuple variants of enums.
///
/// Tuple variants are enum variants that contain unnamed fields,
/// similar to tuple structs but within an enum context.
pub trait TupleVariantAccess {
    /// The parent deserializer type that created this tuple variant access.
    type Parent: Deserializer;

    /// Deserialize the next field in the tuple variant.
    ///
    /// Returns `Ok(field)` if successful, or an error if deserialization fails.
    fn next_field<T: Deserialize<Self::Parent>>(
        &mut self,
    ) -> Result<T, <Self::Parent as Deserializer>::Error>;
}

/// A trait for deserializing struct variants of enums.
///
/// Struct variants are enum variants that contain named fields,
/// similar to regular structs but within an enum context.
pub trait StructVariantAccess {
    /// The parent deserializer type that created this struct variant access.
    type Parent: Deserializer;

    /// The type used for accessing individual fields.
    type FieldAccess: FieldAccess<Parent = Self::Parent>;

    /// Process the next field in the struct variant.
    ///
    /// This method allows examining the next field's identifier and accessing
    /// its value through the provided closure. The closure receives
    /// `Some((field_identifier, field_access))` if there is a next field,
    /// or `None` if all fields have been consumed.
    ///
    /// # Arguments
    ///
    /// * `next` - A closure that receives the field information and access
    ///
    /// # Returns
    ///
    /// Returns the result of the closure, or an error if deserialization fails.
    fn next_field<R>(
        &mut self,
        next: impl FnOnce(
            Option<(Identifier, Self::FieldAccess)>,
        )
            -> Result<R, <Self::Parent as Deserializer>::Error>,
    ) -> Result<R, <Self::Parent as Deserializer>::Error>;
}

/// A trait for deserializing enums.
///
/// This trait provides access to the enum variant identifier and allows
/// deserializing the variant content through specific variant access methods.
pub trait EnumAccess {
    /// The parent deserializer type that created this enum access.
    type Parent: Deserializer;

    /// The type used for deserializing tuple variants.
    type TupleVariantAccess: TupleVariantAccess<Parent = Self::Parent>;

    /// The type used for deserializing struct variants.
    type StructVariantAccess: StructVariantAccess<Parent = Self::Parent>;

    /// Deserialize a unit variant (enum variant with no fields).
    ///
    /// This should be called when the variant is determined to be a unit
    /// variant.
    fn unit_variant(self) -> Result<(), <Self::Parent as Deserializer>::Error>;

    /// Deserialize a tuple variant (enum variant with unnamed fields).
    ///
    /// # Arguments
    ///
    /// * `len` - The expected number of fields in the tuple variant
    /// * `f` - A closure that processes the tuple variant access
    ///
    /// # Returns
    ///
    /// Returns the result of the closure, or an error if deserialization fails.
    fn tuple_variant<R>(
        self,
        len: usize,
        f: impl FnOnce(
            Self::TupleVariantAccess,
        ) -> Result<R, <Self::Parent as Deserializer>::Error>,
    ) -> Result<R, <Self::Parent as Deserializer>::Error>;

    /// Deserialize a struct variant (enum variant with named fields).
    ///
    /// # Arguments
    ///
    /// * `fields` - The expected field identifiers
    /// * `f` - A closure that processes the struct variant access
    ///
    /// # Returns
    ///
    /// Returns the result of the closure, or an error if deserialization fails.
    fn struct_variant<R>(
        self,
        fields: &'static [Identifier],
        f: impl FnOnce(
            Self::StructVariantAccess,
        ) -> Result<R, <Self::Parent as Deserializer>::Error>,
    ) -> Result<R, <Self::Parent as Deserializer>::Error>;
}

/// The main deserializer trait that defines the interface for deserializing
/// Rust data structures.
///
/// This trait provides methods for deserializing primitive types, collections,
/// and complex data structures. It includes an extension mechanism for
/// customization and state passing.
pub trait Deserializer {
    /// The error type returned by deserialization operations.
    type Error;

    /// An extension object that can be used as an additional context for
    /// specialized deserialization of certain types.
    ///
    /// The [`Deserialize`] type can add more trait bounds to this type to
    /// access additional extended functionality.
    type Extension;

    /// The type used for deserializing sequences.
    type SeqAccess<'s>: SeqAccess<Parent = Self>;

    /// The type used for deserializing tuples.
    type TupleAccess<'s>: TupleAccess<Parent = Self>;

    /// The type used for deserializing tuple structs.
    type TupleStructAccess<'s>: TupleStructAccess<Parent = Self>;

    /// The type used for deserializing structs.
    type StructAccess<'s>: StructAccess<Parent = Self>;

    /// The type used for deserializing maps.
    type MapAccess<'s>: MapAccess<Parent = Self>;

    /// The type used for deserializing tuple variants.
    type TupleVariantAccess<'s>: TupleVariantAccess<Parent = Self>;

    /// The type used for deserializing struct variants.
    type StructVariantAccess<'s>: StructVariantAccess<Parent = Self>;

    /// The type used for deserializing enums.
    type EnumAccess<'s>: EnumAccess<Parent = Self>;

    /// Deserialize an i8 value.
    fn expect_i8(&mut self) -> Result<i8, Self::Error>;

    /// Deserialize an i16 value.
    fn expect_i16(&mut self) -> Result<i16, Self::Error>;

    /// Deserialize an i32 value.
    fn expect_i32(&mut self) -> Result<i32, Self::Error>;

    /// Deserialize an i64 value.
    fn expect_i64(&mut self) -> Result<i64, Self::Error>;

    /// Deserialize a u8 value.
    fn expect_u8(&mut self) -> Result<u8, Self::Error>;

    /// Deserialize a u16 value.
    fn expect_u16(&mut self) -> Result<u16, Self::Error>;

    /// Deserialize a u32 value.
    fn expect_u32(&mut self) -> Result<u32, Self::Error>;

    /// Deserialize a u64 value.
    fn expect_u64(&mut self) -> Result<u64, Self::Error>;

    /// Deserialize an isize value.
    fn expect_isize(&mut self) -> Result<isize, Self::Error>;

    /// Deserialize a usize value.
    fn expect_usize(&mut self) -> Result<usize, Self::Error>;

    /// Deserialize an f32 value.
    fn expect_f32(&mut self) -> Result<f32, Self::Error>;

    /// Deserialize an f64 value.
    fn expect_f64(&mut self) -> Result<f64, Self::Error>;

    /// Deserialize a boolean value.
    fn expect_bool(&mut self) -> Result<bool, Self::Error>;

    /// Deserialize a character value.
    fn expect_char(&mut self) -> Result<char, Self::Error>;

    /// Deserialize a string value.
    fn expect_string(&mut self) -> Result<String, Self::Error>;

    /// Deserialize a byte vector.
    fn expect_bytes(&mut self) -> Result<Vec<u8>, Self::Error>;

    /// Deserialize a unit value (empty tuple).
    fn expect_unit(&mut self) -> Result<(), Self::Error>;

    /// Deserialize an optional value.
    ///
    /// Returns `Ok(Some(value))` if the option contains a value,
    /// `Ok(None)` if the option is None, or an error if deserialization fails.
    fn expect_option<T: Deserialize<Self>>(
        &mut self,
    ) -> Result<Option<T>, Self::Error>;

    /// Get a mutable reference to the extension object for customized
    /// deserialization.
    fn extension(&mut self) -> &mut Self::Extension;

    /// Deserialize a sequence (array, vector, etc.).
    ///
    /// # Arguments
    ///
    /// * `f` - A closure that processes the sequence access
    ///
    /// # Returns
    ///
    /// Returns the result of the closure, or an error if deserialization fails.
    fn expect_seq<'s, R>(
        &'s mut self,
        f: impl FnOnce(Self::SeqAccess<'s>) -> Result<R, Self::Error>,
    ) -> Result<R, Self::Error>;

    /// Deserialize a tuple.
    ///
    /// # Arguments
    ///
    /// * `len` - The expected number of elements in the tuple
    /// * `f` - A closure that processes the tuple access
    ///
    /// # Returns
    ///
    /// Returns the result of the closure, or an error if deserialization fails.
    fn expect_tuple<'s, R>(
        &mut self,
        len: usize,
        f: impl FnOnce(Self::TupleAccess<'s>) -> Result<R, Self::Error>,
    ) -> Result<R, Self::Error>;

    /// Deserialize a tuple struct.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the tuple struct type
    /// * `len` - The expected number of fields in the tuple struct
    /// * `f` - A closure that processes the tuple struct access
    ///
    /// # Returns
    ///
    /// Returns the result of the closure, or an error if deserialization fails.
    fn expect_tuple_struct<'s, R>(
        &mut self,
        name: &'static str,
        len: usize,
        f: impl FnOnce(Self::TupleStructAccess<'s>) -> Result<R, Self::Error>,
    ) -> Result<R, Self::Error>;

    /// Deserialize a unit struct (struct with no fields).
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the unit struct type
    fn expect_unit_struct(
        &mut self,
        name: &'static str,
    ) -> Result<(), Self::Error>;

    /// Deserialize a struct.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the struct type
    /// * `fields` - The expected field identifiers
    /// * `f` - A closure that processes the struct access
    ///
    /// # Returns
    ///
    /// Returns the result of the closure, or an error if deserialization fails.
    fn expect_struct<'s, R>(
        &mut self,
        name: &'static str,
        fields: &'static [Identifier],
        f: impl FnOnce(Self::StructAccess<'s>) -> Result<R, Self::Error>,
    ) -> Result<R, Self::Error>;

    /// Deserialize a map (dictionary, hash table, etc.).
    ///
    /// # Arguments
    ///
    /// * `f` - A closure that processes the map access
    ///
    /// # Returns
    ///
    /// Returns the result of the closure, or an error if deserialization fails.
    fn expect_map<'s, R>(
        &mut self,
        f: impl FnOnce(Self::MapAccess<'s>) -> Result<R, Self::Error>,
    ) -> Result<R, Self::Error>;

    /// Deserialize an enum.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the enum type
    /// * `variants` - The expected variant identifiers
    /// * `f` - A closure that processes the enum access
    ///
    /// # Returns
    ///
    /// Returns the result of the closure, or an error if deserialization fails.
    fn expect_enum<'s, R>(
        &'s mut self,
        name: &'static str,
        variants: &'static [Identifier],
        f: impl FnOnce(Identifier, Self::EnumAccess<'s>) -> Result<R, Self::Error>,
    ) -> Result<R, Self::Error>;
}

/// A trait for types that can be deserialized using a [`Deserializer`].
///
/// This trait should be implemented for any type that needs to be deserialized.
/// The implementation defines how the type's data should be read from the
/// deserializer.
pub trait Deserialize<D: ?Sized + Deserializer>: Sized {
    /// Deserialize this value using the provided deserializer.
    ///
    /// # Arguments
    ///
    /// * `deserializer` - The deserializer to read this value from
    ///
    /// # Errors
    ///
    /// Returns an error if the value cannot be deserialized.
    fn deserialize(deserializer: &mut D) -> Result<Self, D::Error>;
}

// =============================================================================
// Primitive Type Implementations
// =============================================================================

macro_rules! impl_deserialize_integer {
    ($($ty:ty => $method:ident),*) => {
        $(
            impl<D> Deserialize<D> for $ty
            where
                D: Deserializer,
            {
                fn deserialize(deserializer: &mut D) -> Result<Self, D::Error> {
                    deserializer.$method()
                }
            }
        )*
    };
}

impl_deserialize_integer! {
    i8 => expect_i8,
    i16 => expect_i16,
    i32 => expect_i32,
    i64 => expect_i64,
    u8 => expect_u8,
    u16 => expect_u16,
    u32 => expect_u32,
    u64 => expect_u64,
    isize => expect_isize,
    usize => expect_usize
}

impl<D> Deserialize<D> for f32
where
    D: Deserializer,
{
    fn deserialize(deserializer: &mut D) -> Result<Self, D::Error> {
        deserializer.expect_f32()
    }
}

impl<D> Deserialize<D> for f64
where
    D: Deserializer,
{
    fn deserialize(deserializer: &mut D) -> Result<Self, D::Error> {
        deserializer.expect_f64()
    }
}

impl<D> Deserialize<D> for bool
where
    D: Deserializer,
{
    fn deserialize(deserializer: &mut D) -> Result<Self, D::Error> {
        deserializer.expect_bool()
    }
}

impl<D> Deserialize<D> for char
where
    D: Deserializer,
{
    fn deserialize(deserializer: &mut D) -> Result<Self, D::Error> {
        deserializer.expect_char()
    }
}

impl<D> Deserialize<D> for String
where
    D: Deserializer,
{
    fn deserialize(deserializer: &mut D) -> Result<Self, D::Error> {
        deserializer.expect_string()
    }
}

// =============================================================================
// Collection Implementations
// =============================================================================

use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet, LinkedList, VecDeque},
    hash::BuildHasher,
};

impl<T, D> Deserialize<D> for Vec<T>
where
    T: Deserialize<D>,
    D: Deserializer,
{
    fn deserialize(deserializer: &mut D) -> Result<Self, D::Error> {
        deserializer.expect_seq(|mut seq| {
            let (lower, upper) = seq.size_hint();
            let mut vec = Self::with_capacity(upper.unwrap_or(lower));

            while let Some(element) = seq.next_element()? {
                vec.push(element);
            }

            Ok(vec)
        })
    }
}

impl<T, const N: usize, D> Deserialize<D> for [T; N]
where
    T: Deserialize<D>,
    D: Deserializer,
{
    fn deserialize(deserializer: &mut D) -> Result<Self, D::Error> {
        deserializer.expect_tuple(N, |mut tuple| {
            let mut result = Vec::with_capacity(N);
            for _ in 0..N {
                result.push(tuple.next_element()?);
            }
            // Convert Vec to array - this uses unsafe but is safe due to length
            // check
            result.try_into().map_err(|_| panic!("Array length mismatch"))
        })
    }
}

impl<K, V, BH, D> Deserialize<D> for HashMap<K, V, BH>
where
    K: Deserialize<D> + Eq + std::hash::Hash,
    V: Deserialize<D>,
    BH: BuildHasher + Default,
    D: Deserializer,
{
    fn deserialize(deserializer: &mut D) -> Result<Self, D::Error> {
        deserializer.expect_map(|mut map_access| {
            let (lower, upper) = map_access.size_hint();
            let mut map = Self::with_capacity_and_hasher(
                upper.unwrap_or(lower),
                BH::default(),
            );

            loop {
                let done = map_access.next_entry(|entry| {
                    if let Some((key, value_access)) = entry {
                        let value = value_access.deserialize()?;
                        map.insert(key, value);
                        Ok(false) // Continue
                    } else {
                        Ok(true) // Done
                    }
                })?;

                if done {
                    break;
                }
            }

            Ok(map)
        })
    }
}

impl<K, V, D> Deserialize<D> for BTreeMap<K, V>
where
    K: Deserialize<D> + Ord,
    V: Deserialize<D>,
    D: Deserializer,
{
    fn deserialize(deserializer: &mut D) -> Result<Self, D::Error> {
        deserializer.expect_map(|mut map_access| {
            let mut map = Self::new();

            loop {
                let done = map_access.next_entry(|entry| {
                    if let Some((key, value_access)) = entry {
                        let value = value_access.deserialize()?;
                        map.insert(key, value);
                        Ok(false) // Continue
                    } else {
                        Ok(true) // Done
                    }
                })?;

                if done {
                    break;
                }
            }

            Ok(map)
        })
    }
}

impl<T, BH, D> Deserialize<D> for HashSet<T, BH>
where
    T: Deserialize<D> + Eq + std::hash::Hash,
    BH: BuildHasher + Default,
    D: Deserializer,
{
    fn deserialize(deserializer: &mut D) -> Result<Self, D::Error> {
        deserializer.expect_seq(|mut seq| {
            let (lower, upper) = seq.size_hint();
            let mut hash_set = Self::with_capacity_and_hasher(
                upper.unwrap_or(lower),
                BH::default(),
            );

            while let Some(element) = seq.next_element()? {
                hash_set.insert(element);
            }

            Ok(hash_set)
        })
    }
}

impl<T, D> Deserialize<D> for BTreeSet<T>
where
    T: Deserialize<D> + Ord,
    D: Deserializer,
{
    fn deserialize(deserializer: &mut D) -> Result<Self, D::Error> {
        deserializer.expect_seq(|mut seq| {
            let mut btree_set = Self::new();

            while let Some(element) = seq.next_element()? {
                btree_set.insert(element);
            }

            Ok(btree_set)
        })
    }
}

impl<T, D> Deserialize<D> for VecDeque<T>
where
    T: Deserialize<D>,
    D: Deserializer,
{
    fn deserialize(deserializer: &mut D) -> Result<Self, D::Error> {
        deserializer.expect_seq(|mut seq| {
            let (lower, upper) = seq.size_hint();
            let mut deque = Self::with_capacity(upper.unwrap_or(lower));

            while let Some(element) = seq.next_element()? {
                deque.push_back(element);
            }

            Ok(deque)
        })
    }
}

impl<T, D> Deserialize<D> for LinkedList<T>
where
    T: Deserialize<D>,
    D: Deserializer,
{
    fn deserialize(deserializer: &mut D) -> Result<Self, D::Error> {
        deserializer.expect_seq(|mut seq| {
            let mut list = Self::new();

            while let Some(element) = seq.next_element()? {
                list.push_back(element);
            }

            Ok(list)
        })
    }
}

// =============================================================================
// Option and Result Implementations
// =============================================================================

impl<T, D> Deserialize<D> for Option<T>
where
    T: Deserialize<D>,
    D: Deserializer,
{
    fn deserialize(deserializer: &mut D) -> Result<Self, D::Error> {
        deserializer.expect_option()
    }
}

impl<T, E, D> Deserialize<D> for Result<T, E>
where
    T: Deserialize<D>,
    E: Deserialize<D>,
    D: Deserializer,
{
    fn deserialize(deserializer: &mut D) -> Result<Self, D::Error> {
        const VARIANTS: &[Identifier] =
            &[Identifier::Name("Ok"), Identifier::Name("Err")];

        deserializer.expect_enum(
            "Result",
            VARIANTS,
            |identifier, enum_access| {
                match identifier {
                    Identifier::Name("Ok") | Identifier::Index(0) => {
                        enum_access.tuple_variant(1, |mut tuple| {
                            Ok(Ok(tuple.next_field::<T>()?))
                        })
                    }
                    Identifier::Name("Err") | Identifier::Index(1) => {
                        enum_access.tuple_variant(1, |mut tuple| {
                            Ok(Err(tuple.next_field::<E>()?))
                        })
                    }
                    variant_id => {
                        // Unknown variant - this indicates corrupted or invalid
                        // data
                        panic!("Unknown Result variant: {variant_id:?}")
                    }
                }
            },
        )
    }
}

// =============================================================================
// Tuple Implementations
// =============================================================================

impl<D> Deserialize<D> for ()
where
    D: Deserializer,
{
    fn deserialize(deserializer: &mut D) -> Result<Self, D::Error> {
        deserializer.expect_unit()
    }
}

macro_rules! impl_deserialize_tuple {
    ($($len:expr => ($($idx:tt $T:ident),+)),*) => {
        $(
            impl<$($T,)* D> Deserialize<D> for ($($T,)*)
            where
                $($T: Deserialize<D>,)*
                D: Deserializer,
            {
                fn deserialize(deserializer: &mut D) -> Result<Self, D::Error> {
                    deserializer.expect_tuple($len, |mut tuple| {
                        Ok((
                            $(
                                tuple.next_element::<$T>()?,
                            )+
                        ))
                    })
                }
            }
        )*
    };
}

impl_deserialize_tuple! {
    1 => (0 T0),
    2 => (0 T0, 1 T1),
    3 => (0 T0, 1 T1, 2 T2),
    4 => (0 T0, 1 T1, 2 T2, 3 T3),
    5 => (0 T0, 1 T1, 2 T2, 3 T3, 4 T4),
    6 => (0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5),
    7 => (0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6),
    8 => (0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7),
    9 => (0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8),
    10 => (0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8, 9 T9),
    11 => (0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8, 9 T9, 10 T10),
    12 => (0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8, 9 T9, 10 T10, 11 T11),
    13 => (0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8, 9 T9, 10 T10, 11 T11, 12 T12),
    14 => (0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8, 9 T9, 10 T10, 11 T11, 12 T12, 13 T13),
    15 => (0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8, 9 T9, 10 T10, 11 T11, 12 T12, 13 T13, 14 T14),
    16 => (0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8, 9 T9, 10 T10, 11 T11, 12 T12, 13 T13, 14 T14, 15 T15)
}

// =============================================================================
// Reference and Smart Pointer Implementations
// =============================================================================

use std::{borrow::Cow, boxed::Box};

impl<T, D> Deserialize<D> for Box<T>
where
    T: Deserialize<D>,
    D: Deserializer,
{
    fn deserialize(deserializer: &mut D) -> Result<Self, D::Error> {
        let value = T::deserialize(deserializer)?;
        Ok(Self::new(value))
    }
}

impl<T, D> Deserialize<D> for Cow<'static, T>
where
    T: Deserialize<D> + ToOwned + Clone,
    T::Owned: Deserialize<D>,
    D: Deserializer,
{
    fn deserialize(deserializer: &mut D) -> Result<Self, D::Error> {
        let owned = T::Owned::deserialize(deserializer)?;
        Ok(Cow::Owned(owned))
    }
}

// =============================================================================
// Additional Standard Library Types
// =============================================================================

use std::marker::PhantomData;

impl<T, D> Deserialize<D> for PhantomData<T>
where
    D: Deserializer,
{
    fn deserialize(deserializer: &mut D) -> Result<Self, D::Error> {
        deserializer.expect_tuple_struct("PhantomData", 1, |_| Ok(Self))
    }
}
