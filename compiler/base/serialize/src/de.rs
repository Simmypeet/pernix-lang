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
//! - [`Error`] - Trait for deserialization error types
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

use std::fmt::Display;

/// Trait used by `Deserialize` implementations to generically construct
/// errors belonging to the `Deserializer` against which they are
/// currently running.
///
/// This trait follows the same pattern as serde's Error trait, allowing
/// deserialization implementations to create custom errors without depending
/// on specific error types.
///
/// # Example
///
/// ```no_run
/// # use pernixc_serialize::de::{self, Deserialize, Deserializer};
/// #
/// struct MyType {
///     data: String,
/// }
///
/// impl<D, E> Deserialize<D, E> for MyType
/// where
///     D: Deserializer<E>,
///     D::Error: de::Error,
/// {
///     fn deserialize(
///         deserializer: &mut D,
///         extension: &mut E,
///     ) -> Result<Self, D::Error> {
///         let data = String::deserialize(deserializer, extension)?;
///         if data.is_empty() {
///             return Err(de::Error::custom("data cannot be empty"));
///         }
///         Ok(MyType { data })
///     }
/// }
/// ```
pub trait Error: Sized + std::error::Error {
    /// Create a custom error with a message.
    ///
    /// The message should not be capitalized and should not end with a
    /// period.
    ///
    /// # Arguments
    ///
    /// * `msg` - A message describing the error
    fn custom<T>(msg: T) -> Self
    where
        T: Display;

    /// Create an error for when a field appears multiple times in a struct.
    ///
    /// # Arguments
    ///
    /// * `field` - The identifier of the duplicated field
    fn duplicated_field(field: Identifier) -> Self {
        Self::custom(format!("duplicated field `{}`", field))
    }

    /// Create an error for when a required field is missing from a struct.
    ///
    /// # Arguments
    ///
    /// * `field` - The identifier of the missing field
    fn missing_field(field: Identifier) -> Self {
        Self::custom(format!("missing field `{}`", field))
    }

    /// Create an error for when an unknown field is encountered in a struct.
    ///
    /// # Arguments
    ///
    /// * `field` - The identifier of the unknown field
    fn unknown_field(field: Identifier) -> Self {
        Self::custom(format!("unknown field `{}`", field))
    }

    /// Create an error for when an unknown enum variant is encountered.
    ///
    /// # Arguments
    ///
    /// * `variant` - The identifier of the unknown variant
    fn unknown_enum_variant(variant: Identifier) -> Self {
        Self::custom(format!("unknown enum variant `{}`", variant))
    }
}

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
pub trait SeqAccess<E> {
    /// The parent deserializer type that created this sequence access.
    type Parent: Deserializer<E>;

    /// Deserialize the next element in the sequence.
    ///
    /// Returns `Ok(Some(element))` if there is a next element,
    /// `Ok(None)` if the sequence is finished, or an error if
    /// deserialization fails.
    fn next_element<T: Deserialize<Self::Parent, E>>(
        &mut self,
        extension: &mut E,
    ) -> Result<Option<T>, <Self::Parent as Deserializer<E>>::Error>;

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
pub trait TupleAccess<E> {
    /// The parent deserializer type that created this tuple access.
    type Parent: Deserializer<E>;

    /// Deserialize the next element in the tuple.
    ///
    /// Returns `Ok(element)` if successful, or an error if deserialization
    /// fails.
    fn next_element<T: Deserialize<Self::Parent, E>>(
        &mut self,
        extension: &mut E,
    ) -> Result<T, <Self::Parent as Deserializer<E>>::Error>;
}

/// A trait for deserializing tuple structs.
///
/// Tuple structs are structs with unnamed fields accessed by position,
/// similar to tuples but with a named type.
pub trait TupleStructAccess<E> {
    /// The parent deserializer type that created this tuple struct access.
    type Parent: Deserializer<E>;

    /// Deserialize the next field in the tuple struct.
    ///
    /// Returns `Ok(field)` if successful, or an error if deserialization fails.
    fn next_field<T: Deserialize<Self::Parent, E>>(
        &mut self,
        extension: &mut E,
    ) -> Result<T, <Self::Parent as Deserializer<E>>::Error>;
}

/// A trait for accessing a specific field during struct deserialization.
///
/// This trait represents access to a single field that can be deserialized
/// on demand.
pub trait FieldAccess<E> {
    /// The parent deserializer type that created this field access.
    type Parent: Deserializer<E>;

    /// Deserialize the field value.
    ///
    /// Returns the deserialized field value, or an error if deserialization
    /// fails.
    fn deserialize<T: Deserialize<Self::Parent, E>>(
        self,
        extension: &mut E,
    ) -> Result<T, <Self::Parent as Deserializer<E>>::Error>;
}

/// A trait for deserializing structs with named fields.
///
/// This trait handles the deserialization of structures where each field
/// has a name and can be accessed by name.
pub trait StructAccess<E> {
    /// The parent deserializer type that created this struct access.
    type Parent: Deserializer<E>;

    /// The type used for accessing individual fields.
    type FieldAccess<'s>: FieldAccess<E, Parent = Self::Parent>;

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
    fn next_field<'s, 'e, R>(
        &'s mut self,
        extension: &'e mut E,
        next: impl FnOnce(
            Option<(Identifier, Self::FieldAccess<'s>, &'e mut E)>,
        )
            -> Result<R, <Self::Parent as Deserializer<E>>::Error>,
    ) -> Result<R, <Self::Parent as Deserializer<E>>::Error>;
}

/// A trait for accessing a specific value during map deserialization.
///
/// This trait represents access to a single map value that can be deserialized
/// on demand after the key has been examined.
pub trait ValueAccess<E> {
    /// The parent deserializer type that created this value access.
    type Parent: Deserializer<E>;

    /// Deserialize the value.
    ///
    /// Returns the deserialized value, or an error if deserialization fails.
    fn deserialize<V: Deserialize<Self::Parent, E>>(
        self,
        extension: &mut E,
    ) -> Result<V, <Self::Parent as Deserializer<E>>::Error>;
}

/// A trait for deserializing maps (dictionaries, hash tables, etc.).
///
/// Maps are collections of key-value pairs where keys and values
/// can be of different types.
pub trait MapAccess<E> {
    /// The parent deserializer type that created this map access.
    type Parent: Deserializer<E>;

    /// The type used for accessing individual values after key examination.
    type ValueAccess<'s>: ValueAccess<E, Parent = Self::Parent>;

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
    fn next_entry<'s, 'e, K: Deserialize<Self::Parent, E>, R>(
        &'s mut self,
        extension: &'e mut E,
        next: impl FnOnce(
            Option<(K, Self::ValueAccess<'s>, &'e mut E)>,
        )
            -> Result<R, <Self::Parent as Deserializer<E>>::Error>,
    ) -> Result<R, <Self::Parent as Deserializer<E>>::Error>;

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
pub trait TupleVariantAccess<E> {
    /// The parent deserializer type that created this tuple variant access.
    type Parent: Deserializer<E>;

    /// Deserialize the next field in the tuple variant.
    ///
    /// Returns `Ok(field)` if successful, or an error if deserialization fails.
    fn next_field<T: Deserialize<Self::Parent, E>>(
        &mut self,
        extension: &mut E,
    ) -> Result<T, <Self::Parent as Deserializer<E>>::Error>;
}

/// A trait for deserializing struct variants of enums.
///
/// Struct variants are enum variants that contain named fields,
/// similar to regular structs but within an enum context.
pub trait StructVariantAccess<E> {
    /// The parent deserializer type that created this struct variant access.
    type Parent: Deserializer<E>;

    /// The type used for accessing individual fields.
    type FieldAccess<'s>: FieldAccess<E, Parent = Self::Parent>;

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
    fn next_field<'s, 'e, R>(
        &'s mut self,
        extension: &'e mut E,
        next: impl FnOnce(
            Option<(Identifier, Self::FieldAccess<'s>, &'e mut E)>,
        )
            -> Result<R, <Self::Parent as Deserializer<E>>::Error>,
    ) -> Result<R, <Self::Parent as Deserializer<E>>::Error>;
}

/// A trait for deserializing enums.
///
/// This trait provides access to the enum variant identifier and allows
/// deserializing the variant content through specific variant access methods.
pub trait EnumAccess<E> {
    /// The parent deserializer type that created this enum access.
    type Parent: Deserializer<E>;

    /// The type used for deserializing tuple variants.
    type TupleVariantAccess: TupleVariantAccess<E, Parent = Self::Parent>;

    /// The type used for deserializing struct variants.
    type StructVariantAccess: StructVariantAccess<E, Parent = Self::Parent>;

    /// Deserialize a unit variant (enum variant with no fields).
    ///
    /// This should be called when the variant is determined to be a unit
    /// variant.
    fn unit_variant(
        self,
    ) -> Result<(), <Self::Parent as Deserializer<E>>::Error>;

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
    fn tuple_variant<'e, R>(
        self,
        len: usize,
        extension: &'e mut E,
        f: impl FnOnce(
            Self::TupleVariantAccess,
            &'e mut E,
        )
            -> Result<R, <Self::Parent as Deserializer<E>>::Error>,
    ) -> Result<R, <Self::Parent as Deserializer<E>>::Error>;

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
    fn struct_variant<'e, R>(
        self,
        fields: &'static [&'static str],
        extension: &'e mut E,
        f: impl FnOnce(
            Self::StructVariantAccess,
            &'e mut E,
        )
            -> Result<R, <Self::Parent as Deserializer<E>>::Error>,
    ) -> Result<R, <Self::Parent as Deserializer<E>>::Error>;
}

/// The main deserializer trait that defines the interface for deserializing
/// Rust data structures.
///
/// This trait provides methods for deserializing primitive types, collections,
/// and complex data structures. It includes an extension mechanism for
/// customization and state passing.
pub trait Deserializer<E> {
    /// The error type returned by deserialization operations.
    type Error: Error;

    /// The type used for deserializing sequences.
    type SeqAccess<'s>: SeqAccess<E, Parent = Self>;

    /// The type used for deserializing tuples.
    type TupleAccess<'s>: TupleAccess<E, Parent = Self>;

    /// The type used for deserializing tuple structs.
    type TupleStructAccess<'s>: TupleStructAccess<E, Parent = Self>;

    /// The type used for deserializing structs.
    type StructAccess<'s>: StructAccess<E, Parent = Self>;

    /// The type used for deserializing maps.
    type MapAccess<'s>: MapAccess<E, Parent = Self>;

    /// The type used for deserializing tuple variants.
    type TupleVariantAccess<'s>: TupleVariantAccess<E, Parent = Self>;

    /// The type used for deserializing struct variants.
    type StructVariantAccess<'s>: StructVariantAccess<E, Parent = Self>;

    /// The type used for deserializing enums.
    type EnumAccess<'s>: EnumAccess<E, Parent = Self>;

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

    /// Deserialze a string slice.
    fn expect_str(&mut self) -> Result<&str, Self::Error>;

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
    fn expect_option<T: Deserialize<Self, E>>(
        &mut self,
        extension: &mut E,
    ) -> Result<Option<T>, Self::Error>;

    /// Deserialize a sequence (array, vector, etc.).
    ///
    /// # Arguments
    ///
    /// * `f` - A closure that processes the sequence access
    ///
    /// # Returns
    ///
    /// Returns the result of the closure, or an error if deserialization fails.
    fn expect_seq<'s, 'e, R>(
        &'s mut self,
        extension: &'e mut E,
        f: impl FnOnce(Self::SeqAccess<'s>, &'e mut E) -> Result<R, Self::Error>,
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
    fn expect_tuple<'s, 'e, R>(
        &'s mut self,
        len: usize,
        extension: &'e mut E,
        f: impl FnOnce(Self::TupleAccess<'s>, &'e mut E) -> Result<R, Self::Error>,
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
    fn expect_tuple_struct<'s, 'e, R>(
        &'s mut self,
        name: &'static str,
        len: usize,
        extension: &'e mut E,
        f: impl FnOnce(
            Self::TupleStructAccess<'s>,
            &'e mut E,
        ) -> Result<R, Self::Error>,
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
    fn expect_struct<'s, 'e, R>(
        &'s mut self,
        name: &'static str,
        fields: &'static [&'static str],
        extension: &'e mut E,
        f: impl FnOnce(Self::StructAccess<'s>, &'e mut E) -> Result<R, Self::Error>,
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
    fn expect_map<'s, 'e, R>(
        &'s mut self,
        extension: &'e mut E,
        f: impl FnOnce(Self::MapAccess<'s>, &'e mut E) -> Result<R, Self::Error>,
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
    fn expect_enum<'s, 'e, R>(
        &'s mut self,
        name: &'static str,
        variants: &'static [&'static str],
        extension: &'e mut E,
        f: impl FnOnce(
            Identifier,
            Self::EnumAccess<'s>,
            &'e mut E,
        ) -> Result<R, Self::Error>,
    ) -> Result<R, Self::Error>;
}

/// A trait for types that can be deserialized using a [`Deserializer`].
///
/// This trait should be implemented for any type that needs to be deserialized.
/// The implementation defines how the type's data should be read from the
/// deserializer.
pub trait Deserialize<D: Deserializer<E> + ?Sized, E>: Sized {
    /// Deserialize this value using the provided deserializer.
    ///
    /// # Arguments
    ///
    /// * `deserializer` - The deserializer to read this value from
    ///
    /// # Errors
    ///
    /// Returns an error if the value cannot be deserialized.
    fn deserialize(
        deserializer: &mut D,
        extension: &mut E,
    ) -> Result<Self, D::Error>;
}

// =============================================================================
// Primitive Type Implementations
// =============================================================================

macro_rules! impl_deserialize_integer {
    ($($ty:ty => $method:ident),*) => {
        $(
            impl<D, E> Deserialize<D, E> for $ty
            where
                D: Deserializer<E>,
            {
                fn deserialize(deserializer: &mut D, _extension: &mut E) -> Result<Self, D::Error> {
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

impl<D, E> Deserialize<D, E> for f32
where
    D: Deserializer<E>,
{
    fn deserialize(
        deserializer: &mut D,
        _extension: &mut E,
    ) -> Result<Self, D::Error> {
        deserializer.expect_f32()
    }
}

impl<D, E> Deserialize<D, E> for f64
where
    D: Deserializer<E>,
{
    fn deserialize(
        deserializer: &mut D,
        _extension: &mut E,
    ) -> Result<Self, D::Error> {
        deserializer.expect_f64()
    }
}

impl<D, E> Deserialize<D, E> for bool
where
    D: Deserializer<E>,
{
    fn deserialize(
        deserializer: &mut D,
        _extension: &mut E,
    ) -> Result<Self, D::Error> {
        deserializer.expect_bool()
    }
}

impl<D, E> Deserialize<D, E> for char
where
    D: Deserializer<E>,
{
    fn deserialize(
        deserializer: &mut D,
        _extension: &mut E,
    ) -> Result<Self, D::Error> {
        deserializer.expect_char()
    }
}

impl<D, E> Deserialize<D, E> for String
where
    D: Deserializer<E>,
{
    fn deserialize(
        deserializer: &mut D,
        _extension: &mut E,
    ) -> Result<Self, D::Error> {
        deserializer.expect_string()
    }
}

// =============================================================================
// Collection Implementations
// =============================================================================

use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet, LinkedList, VecDeque},
    hash::{BuildHasher, Hash},
    ops::Range,
};

impl<T, D, E> Deserialize<D, E> for Vec<T>
where
    T: Deserialize<D, E>,
    D: Deserializer<E>,
{
    fn deserialize(
        deserializer: &mut D,
        extension: &mut E,
    ) -> Result<Self, D::Error> {
        deserializer.expect_seq(extension, |mut seq, e| {
            let (lower, upper) = seq.size_hint();
            let mut vec = Self::with_capacity(upper.unwrap_or(lower));

            while let Some(element) = seq.next_element(e)? {
                vec.push(element);
            }

            Ok(vec)
        })
    }
}

impl<T, const N: usize, D, E> Deserialize<D, E> for [T; N]
where
    T: Deserialize<D, E>,
    D: Deserializer<E>,
{
    fn deserialize(
        deserializer: &mut D,
        extension: &mut E,
    ) -> Result<Self, D::Error> {
        use std::mem::{ManuallyDrop, MaybeUninit};

        deserializer.expect_seq(extension, |mut seq, e| {
            // Create an uninitialized array
            let mut array: [MaybeUninit<T>; N] =
                unsafe { MaybeUninit::uninit().assume_init() };

            // Deserialize each element with proper error handling
            for i in 0..N {
                match seq.next_element(e) {
                    Ok(Some(element)) => {
                        array[i] = MaybeUninit::new(element);
                    }
                    Ok(None) => {
                        // Not enough elements, clean up and return error
                        // Drop any successfully initialized elements
                        for element in &mut array[..i] {
                            unsafe {
                                element.assume_init_read();
                            }
                        }
                        return Err(D::Error::custom(
                            "array deserialization: not enough elements",
                        ));
                    }
                    Err(e) => {
                        // Error during deserialization, clean up and propagate
                        // Drop any successfully initialized elements
                        for element in &mut array[..i] {
                            unsafe {
                                element.assume_init_read();
                            }
                        }
                        return Err(e);
                    }
                }
            }

            // All elements successfully initialized, convert to final array
            // This is safe because we've initialized all N elements
            let result = unsafe {
                (&array as *const [MaybeUninit<T>; N] as *const [T; N]).read()
            };

            // Prevent double-drop of the MaybeUninit array
            let _ = ManuallyDrop::new(array);

            Ok(result)
        })
    }
}

impl<K, V, BH, D, E> Deserialize<D, E> for HashMap<K, V, BH>
where
    K: Deserialize<D, E> + Eq + std::hash::Hash,
    V: Deserialize<D, E>,
    BH: BuildHasher + Default,
    D: Deserializer<E>,
{
    fn deserialize(
        deserializer: &mut D,
        extension: &mut E,
    ) -> Result<Self, D::Error> {
        deserializer.expect_map(extension, |mut map_access, e| {
            let (lower, upper) = map_access.size_hint();
            let mut map = Self::with_capacity_and_hasher(
                upper.unwrap_or(lower),
                BH::default(),
            );

            loop {
                let done = map_access.next_entry(e, |entry| {
                    if let Some((key, value_access, e)) = entry {
                        let value = value_access.deserialize(e)?;
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

impl<K, V, D, E> Deserialize<D, E> for BTreeMap<K, V>
where
    K: Deserialize<D, E> + Ord,
    V: Deserialize<D, E>,
    D: Deserializer<E>,
{
    fn deserialize(
        deserializer: &mut D,
        extension: &mut E,
    ) -> Result<Self, D::Error> {
        deserializer.expect_map(extension, |mut map_access, e| {
            let mut map = Self::new();

            loop {
                let done = map_access.next_entry(e, |entry| {
                    if let Some((key, value_access, e)) = entry {
                        let value = value_access.deserialize(e)?;
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

impl<T, BH, D, E> Deserialize<D, E> for HashSet<T, BH>
where
    T: Deserialize<D, E> + Eq + std::hash::Hash,
    BH: BuildHasher + Default,
    D: Deserializer<E>,
{
    fn deserialize(
        deserializer: &mut D,
        extension: &mut E,
    ) -> Result<Self, D::Error> {
        deserializer.expect_seq(extension, |mut seq, e| {
            let (lower, upper) = seq.size_hint();
            let mut hash_set = Self::with_capacity_and_hasher(
                upper.unwrap_or(lower),
                BH::default(),
            );

            while let Some(element) = seq.next_element(e)? {
                hash_set.insert(element);
            }

            Ok(hash_set)
        })
    }
}

impl<T, D, E> Deserialize<D, E> for BTreeSet<T>
where
    T: Deserialize<D, E> + Ord,
    D: Deserializer<E>,
{
    fn deserialize(
        deserializer: &mut D,
        extension: &mut E,
    ) -> Result<Self, D::Error> {
        deserializer.expect_seq(extension, |mut seq, e| {
            let mut btree_set = Self::new();

            while let Some(element) = seq.next_element(e)? {
                btree_set.insert(element);
            }

            Ok(btree_set)
        })
    }
}

impl<T, D, E> Deserialize<D, E> for VecDeque<T>
where
    T: Deserialize<D, E>,
    D: Deserializer<E>,
{
    fn deserialize(
        deserializer: &mut D,
        extension: &mut E,
    ) -> Result<Self, D::Error> {
        deserializer.expect_seq(extension, |mut seq, e| {
            let (lower, upper) = seq.size_hint();
            let mut deque = Self::with_capacity(upper.unwrap_or(lower));

            while let Some(element) = seq.next_element(e)? {
                deque.push_back(element);
            }

            Ok(deque)
        })
    }
}

impl<T, D, E> Deserialize<D, E> for LinkedList<T>
where
    T: Deserialize<D, E>,
    D: Deserializer<E>,
{
    fn deserialize(
        deserializer: &mut D,
        extension: &mut E,
    ) -> Result<Self, D::Error> {
        deserializer.expect_seq(extension, |mut seq, e| {
            let mut list = Self::new();

            while let Some(element) = seq.next_element(e)? {
                list.push_back(element);
            }

            Ok(list)
        })
    }
}

// =============================================================================
// Option and Result Implementations
// =============================================================================

impl<T, D, E> Deserialize<D, E> for Option<T>
where
    T: Deserialize<D, E>,
    D: Deserializer<E>,
{
    fn deserialize(
        deserializer: &mut D,
        extension: &mut E,
    ) -> Result<Self, D::Error> {
        deserializer.expect_option(extension)
    }
}

impl<T, E, D, Ext> Deserialize<D, Ext> for Result<T, E>
where
    T: Deserialize<D, Ext>,
    E: Deserialize<D, Ext>,
    D: Deserializer<Ext>,
{
    fn deserialize(
        deserializer: &mut D,
        extension: &mut Ext,
    ) -> Result<Self, D::Error> {
        const VARIANTS: &[&str] = &["Ok", "Err"];

        deserializer.expect_enum(
            "Result",
            VARIANTS,
            extension,
            |identifier, enum_access, extension| {
                match identifier {
                    Identifier::Name("Ok") | Identifier::Index(0) => {
                        enum_access.tuple_variant(
                            1,
                            extension,
                            |mut tuple, e| {
                                Ok(Ok(TupleVariantAccess::next_field(
                                    &mut tuple, e,
                                )?))
                            },
                        )
                    }
                    Identifier::Name("Err") | Identifier::Index(1) => {
                        enum_access.tuple_variant(
                            1,
                            extension,
                            |mut tuple, e| {
                                Ok(Err(TupleVariantAccess::next_field(
                                    &mut tuple, e,
                                )?))
                            },
                        )
                    }
                    variant_id => {
                        // Unknown variant - this indicates corrupted or invalid
                        // data
                        Err(D::Error::custom(format!(
                            "unknown Result variant: {variant_id:?}"
                        )))
                    }
                }
            },
        )
    }
}

// =============================================================================
// Tuple Implementations
// =============================================================================

impl<D, E> Deserialize<D, E> for ()
where
    D: Deserializer<E>,
{
    fn deserialize(
        deserializer: &mut D,
        _extension: &mut E,
    ) -> Result<Self, D::Error> {
        deserializer.expect_unit()
    }
}

macro_rules! impl_deserialize_tuple {
    ($($len:expr => ($($idx:tt $T:ident),+)),*) => {
        $(
            impl<$($T,)* D, E> Deserialize<D, E> for ($($T,)*)
            where
                $($T: Deserialize<D, E>,)*
                D: Deserializer<E>,
            {
                fn deserialize(deserializer: &mut D, extension: &mut E) -> Result<Self, D::Error> {
                    deserializer.expect_tuple($len, extension, |mut tuple, extension| {
                        Ok((
                            $(
                                tuple.next_element::<$T>(extension)?,
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

impl<T, D, E> Deserialize<D, E> for Box<T>
where
    T: Deserialize<D, E>,
    D: Deserializer<E>,
{
    fn deserialize(
        deserializer: &mut D,
        extension: &mut E,
    ) -> Result<Self, D::Error> {
        let value = T::deserialize(deserializer, extension)?;
        Ok(Self::new(value))
    }
}

impl<T, D, E> Deserialize<D, E> for Cow<'static, T>
where
    T: Deserialize<D, E> + ToOwned + Clone,
    T::Owned: Deserialize<D, E>,
    D: Deserializer<E>,
{
    fn deserialize(
        deserializer: &mut D,
        extension: &mut E,
    ) -> Result<Self, D::Error> {
        let owned = T::Owned::deserialize(deserializer, extension)?;
        Ok(Cow::Owned(owned))
    }
}

// =============================================================================
// Additional Standard Library Types
// =============================================================================

use std::marker::PhantomData;

impl<T, D, E> Deserialize<D, E> for PhantomData<T>
where
    D: Deserializer<E>,
{
    fn deserialize(
        deserializer: &mut D,
        extension: &mut E,
    ) -> Result<Self, D::Error> {
        deserializer
            .expect_tuple_struct("PhantomData", 1, extension, |_, _| Ok(Self))
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Index(index) => write!(f, "{}", index),
            Self::Name(name) => write!(f, "{}", name),
        }
    }
}

use std::path::PathBuf;

use dashmap::DashMap;
use flexstr::FlexStr;

impl<D, E> Deserialize<D, E> for PathBuf
where
    D: Deserializer<E>,
{
    fn deserialize(
        deserializer: &mut D,
        extension: &mut E,
    ) -> Result<Self, D::Error> {
        Ok(PathBuf::from(String::deserialize(deserializer, extension)?))
    }
}

impl<D: Deserializer<E>, T: Deserialize<D, E>, E> Deserialize<D, E>
    for Range<T>
{
    fn deserialize(
        deserializer: &mut D,
        extension: &mut E,
    ) -> Result<Self, <D as Deserializer<E>>::Error> {
        deserializer.expect_struct(
            "Range",
            &["start", "end"],
            extension,
            |mut x, extension| {
                let mut start = None;
                let mut end = None;

                loop {
                    let should_continue =
                        StructAccess::next_field(&mut x, extension, |field| {
                            let Some((ident, access, e)) = field else {
                                return Ok(false);
                            };

                            match ident {
                                Identifier::Name("start")
                                | Identifier::Index(0) => {
                                    if start.is_some() {
                                        return Err(
                                            D::Error::duplicated_field(
                                                Identifier::Name("start"),
                                            ),
                                        );
                                    }
                                    start = Some(access.deserialize(e)?);
                                }
                                Identifier::Name("end")
                                | Identifier::Index(1) => {
                                    if end.is_some() {
                                        return Err(
                                            D::Error::duplicated_field(
                                                Identifier::Name("end"),
                                            ),
                                        );
                                    }
                                    end = Some(access.deserialize(e)?);
                                }
                                _ => {
                                    return Err(D::Error::unknown_field(ident));
                                }
                            }

                            Ok(true)
                        })?;

                    if !should_continue {
                        break;
                    }
                }

                Ok(Range {
                    start: start.ok_or_else(|| {
                        D::Error::missing_field(Identifier::Name("start"))
                    })?,
                    end: end.ok_or_else(|| {
                        D::Error::missing_field(Identifier::Name("end"))
                    })?,
                })
            },
        )
    }
}

impl<
        D: Deserializer<E>,
        K: Deserialize<D, E> + Eq + Hash,
        V: Deserialize<D, E>,
        BH: BuildHasher + Clone + Default,
        E,
    > Deserialize<D, E> for DashMap<K, V, BH>
{
    fn deserialize(
        deserializer: &mut D,
        extension: &mut E,
    ) -> Result<Self, <D as Deserializer<E>>::Error> {
        deserializer.expect_map(extension, |mut map_access, extension| {
            let (lower, upper) = map_access.size_hint();
            let map = Self::with_capacity_and_hasher(
                upper.unwrap_or(lower),
                BH::default(),
            );

            loop {
                let done = map_access.next_entry(extension, |entry| {
                    if let Some((key, value_access, extension)) = entry {
                        let value = value_access.deserialize(extension)?;
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

impl<
        D: Deserializer<E>,
        const SIZE: usize,
        const PAD1: usize,
        const PAD2: usize,
        HEAP,
        E,
    > Deserialize<D, E> for FlexStr<SIZE, PAD1, PAD2, HEAP>
where
    HEAP: for<'a> From<&'a str>,
{
    fn deserialize(
        deserializer: &mut D,
        _extension: &mut E,
    ) -> Result<Self, <D as Deserializer<E>>::Error> {
        Ok(deserializer.expect_str()?.into())
    }
}

impl<D: Deserializer<E>, E, T: Deserialize<D, E>> Deserialize<D, E>
    for std::sync::RwLock<T>
{
    fn deserialize(
        deserializer: &mut D,
        extension: &mut E,
    ) -> Result<Self, <D as Deserializer<E>>::Error> {
        let value = T::deserialize(deserializer, extension)?;
        Ok(std::sync::RwLock::new(value))
    }
}

impl<D: Deserializer<E>, E, T: Deserialize<D, E>> Deserialize<D, E>
    for std::sync::Mutex<T>
{
    fn deserialize(
        deserializer: &mut D,
        extension: &mut E,
    ) -> Result<Self, <D as Deserializer<E>>::Error> {
        let value = T::deserialize(deserializer, extension)?;
        Ok(std::sync::Mutex::new(value))
    }
}

impl<D: Deserializer<E>, E, T: Deserialize<D, E>> Deserialize<D, E>
    for parking_lot::RwLock<T>
{
    fn deserialize(
        deserializer: &mut D,
        extension: &mut E,
    ) -> Result<Self, <D as Deserializer<E>>::Error> {
        let value = T::deserialize(deserializer, extension)?;
        Ok(parking_lot::RwLock::new(value))
    }
}

impl<D: Deserializer<E>, E, T: Deserialize<D, E>> Deserialize<D, E>
    for parking_lot::Mutex<T>
{
    fn deserialize(
        deserializer: &mut D,
        extension: &mut E,
    ) -> Result<Self, <D as Deserializer<E>>::Error> {
        let value = T::deserialize(deserializer, extension)?;
        Ok(parking_lot::Mutex::new(value))
    }
}
