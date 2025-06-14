//! Custom serialization framework for the Pernix compiler.
//!
//! This module provides a custom serialization framework similar to standard
//! serde, but with an extension mechanism for easier customization and state
//! passing. The framework is designed to handle complex serialization scenarios
//! where additional context or state needs to be maintained during
//! serialization.
//!
//! ## Key Traits
//!
//! - [`Serializer`] - The main trait for types that can serialize Rust data
//!   structures
//! - [`Serialize`] - Trait for types that can be serialized using a serializer
//! - [`Error`] - Trait for serialization error types
//! - [`Seq`], [`Tuple`], [`TupleStruct`], [`Struct`], [`Map`] - Compound data
//!   structure serializers
//! - [`TupleVariant`], [`StructVariant`] - Enum variant serializers
//!
//! ## Extension Mechanism
//!
//! The [`Serializer::Extension`] associated type allows for specialized
//! serialization behavior. Extensions can maintain state across serialization
//! operations and provide custom handling for specific types like shared
//! pointers.

use std::fmt::Display;

/// Trait used by `Serialize` implementations to generically construct
/// errors belonging to the `Serializer` against which they are
/// currently running.
///
/// This trait follows the same pattern as serde's Error trait, allowing
/// serialization implementations to create custom errors without depending
/// on specific error types.
///
/// # Example
///
/// ```no_run
/// # use pernixc_serialize::ser::{self, Serialize, Serializer};
/// #
/// struct MyType {
///     data: String,
/// }
///
/// impl<S> Serialize<S> for MyType
/// where
///     S: Serializer,
///     S::Error: ser::Error,
/// {
///     fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
///         if self.data.is_empty() {
///             return Err(ser::Error::custom("data cannot be empty"));
///         }
///         self.data.serialize(serializer)
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
}

/// A trait for serializing sequences (arrays, vectors, etc.).
///
/// This trait is used to serialize ordered collections of elements where
/// all elements are of the same type or can be serialized with the same
/// serializer.
///
/// # Examples
///
/// Seq is used for data structures like:
/// - `[1, 2, 3]` (arrays)
/// - `vec![1, 2, 3]` (vectors)
/// - `&[1, 2, 3]` (slices)
pub trait Seq {
    /// The parent serializer type that created this sequence serializer.
    type Parent: Serializer;

    /// Serialize a single element in the sequence.
    ///
    /// # Arguments
    ///
    /// * `value` - The element to serialize
    ///
    /// # Errors
    ///
    /// Returns an error if the element cannot be serialized.
    fn serialize_element<T: Serialize<Self::Parent> + ?Sized>(
        &mut self,
        value: &T,
    ) -> Result<(), <Self::Parent as Serializer>::Error>;
}

/// A trait for serializing tuples.
///
/// This trait is used to serialize fixed-size ordered collections where
/// elements may be of different types but the number of elements is known at
/// compile time.
///
/// # Examples
///
/// Tuple is used for data structures like:
/// - `(a, b, c)` (tuples with named variables)
/// - `(1, "hello", true)` (tuples with mixed types)
/// - `()` (unit tuple)
pub trait Tuple {
    /// The parent serializer type that created this tuple serializer.
    type Parent: Serializer;

    /// Serialize a single element in the tuple.
    ///
    /// # Arguments
    ///
    /// * `value` - The element to serialize
    ///
    /// # Errors
    ///
    /// Returns an error if the element cannot be serialized.
    fn serialize_element<T: Serialize<Self::Parent> + ?Sized>(
        &mut self,
        value: &T,
    ) -> Result<(), <Self::Parent as Serializer>::Error>;
}

/// A trait for serializing tuple structs.
///
/// Tuple structs are structs with unnamed fields accessed by position,
/// similar to tuples but with a named type.
///
/// # Examples
///
/// [`TupleStruct`] is used for data structures like:
/// - `Point(x, y)` where `struct Point(i32, i32);`
/// - `Color(255, 128, 0)` where `struct Color(u8, u8, u8);`
/// - `Wrapper(value)` where `struct Wrapper(String);`
pub trait TupleStruct {
    /// The parent serializer type that created this tuple struct serializer.
    type Parent: Serializer;

    /// Serialize a single field in the tuple struct.
    ///
    /// # Arguments
    ///
    /// * `value` - The field value to serialize
    ///
    /// # Errors
    ///
    /// Returns an error if the field cannot be serialized.
    fn serialize_field<T: Serialize<Self::Parent> + ?Sized>(
        &mut self,
        value: &T,
    ) -> Result<(), <Self::Parent as Serializer>::Error>;
}

/// A trait for serializing structs with named fields.
///
/// This trait handles the serialization of structures where each field
/// has a name and can be optionally skipped during serialization.
///
/// # Examples
///
/// Struct is used for data structures like:
/// - `Person { name: "Alice", age: 30 }` where `struct Person { name: String,
///   age: u32 }`
/// - `Rectangle { width: 10, height: 20 }` where `struct Rectangle { width:
///   u32, height: u32 }`
/// - `Config { debug: true, max_connections: 100 }` where `struct Config {
///   debug: bool, max_connections: usize }`
pub trait Struct {
    /// The parent serializer type that created this struct serializer.
    type Parent: Serializer;

    /// Serialize a named field in the struct.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the field
    /// * `value` - The field value to serialize
    ///
    /// # Errors
    ///
    /// Returns an error if the field cannot be serialized.
    fn serialize_field<T: Serialize<Self::Parent> + ?Sized>(
        &mut self,
        name: &'static str,
        value: &T,
    ) -> Result<(), <Self::Parent as Serializer>::Error>;

    /// Skip serialization of a named field.
    ///
    /// This allows for conditional field serialization where certain fields
    /// may be omitted from the serialized output.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the field to skip
    ///
    /// # Errors
    ///
    /// Returns an error if the field skipping operation fails.
    fn skip_field(
        &mut self,
        name: &'static str,
    ) -> Result<(), <Self::Parent as Serializer>::Error>;
}

/// A trait for serializing maps (dictionaries, hash tables, etc.).
///
/// Maps are collections of key-value pairs where keys and values
/// can be of different types.
///
/// # Examples
///
/// Map is used for data structures like:
/// - `HashMap<String, i32>` where `{"Alice": 25, "Bob": 30}`
/// - `BTreeMap<i32, String>` where `{1: "one", 2: "two", 3: "three"}`
/// - `IndexMap<&str, bool>` where `{"debug": true, "verbose": false}`
pub trait Map {
    /// The parent serializer type that created this map serializer.
    type Parent: Serializer;

    /// Serialize a key-value pair in the map.
    ///
    /// # Arguments
    ///
    /// * `key` - The key to serialize
    /// * `value` - The value to serialize
    ///
    /// # Errors
    ///
    /// Returns an error if the key or value cannot be serialized.
    fn serialize_entry<
        K: Serialize<Self::Parent> + ?Sized,
        V: Serialize<Self::Parent> + ?Sized,
    >(
        &mut self,
        key: &K,
        value: &V,
    ) -> Result<(), <Self::Parent as Serializer>::Error>;
}

/// A trait for serializing tuple variants of enums.
///
/// Tuple variants are enum variants that contain unnamed fields,
/// similar to tuple structs but within an enum context.
///
/// # Examples
///
/// [`TupleVariant`] is used for enum variants like:
/// - `Message::Move(x, y)` where `enum Message { Move(i32, i32), ... }`
/// - `Result::Ok(value)` where `enum Result<T, E> { Ok(T), Err(E) }`
/// - `Option::Some(data)` where `enum Option<T> { Some(T), None }`
pub trait TupleVariant {
    /// The parent serializer type that created this tuple variant serializer.
    type Parent: Serializer;

    /// Serialize a field in the tuple variant.
    ///
    /// # Arguments
    ///
    /// * `value` - The field value to serialize
    ///
    /// # Errors
    ///
    /// Returns an error if the field cannot be serialized.
    fn serialize_field<T: Serialize<Self::Parent> + ?Sized>(
        &mut self,
        value: &T,
    ) -> Result<(), <Self::Parent as Serializer>::Error>;
}

/// A trait for serializing struct variants of enums.
///
/// Struct variants are enum variants that contain named fields,
/// similar to regular structs but within an enum context.
///
/// # Examples
///
/// [`StructVariant`] is used for enum variants like:
/// - `Message::ChangeColor { r: 255, g: 0, b: 0 }` where `enum Message {
///   ChangeColor { r: u8, g: u8, b: u8 }, ... }`
/// - `Shape::Rectangle { width: 10, height: 20 }` where `enum Shape { Rectangle
///   { width: u32, height: u32 }, ... }`
/// - `Event::KeyPress { key: 'a', modifiers: ["ctrl"] }` where `enum Event {
///   KeyPress { key: char, modifiers: Vec<String> }, ... }`
pub trait StructVariant {
    /// The parent serializer type that created this struct variant serializer.
    type Parent: Serializer;

    /// Serialize a named field in the struct variant.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the field
    /// * `value` - The field value to serialize
    ///
    /// # Errors
    ///
    /// Returns an error if the field cannot be serialized.
    fn serialize_field<T: Serialize<Self::Parent> + ?Sized>(
        &mut self,
        name: &'static str,
        value: &T,
    ) -> Result<(), <Self::Parent as Serializer>::Error>;

    /// Skip serialization of a named field in the struct variant.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the field to skip
    ///
    /// # Errors
    ///
    /// Returns an error if the field skipping operation fails.
    fn skip_field(
        &mut self,
        name: &'static str,
    ) -> Result<(), <Self::Parent as Serializer>::Error>;
}

/// The main serializer trait that defines the interface for serializing Rust
/// data structures.
///
/// This trait provides methods for serializing primitive types, collections,
/// and complex data structures. It includes an extension mechanism for
/// customization and state passing.
pub trait Serializer {
    /// The error type returned by serialization operations.
    type Error: Error;

    /// An extension object that can be used as an additional context for
    /// specialized serialization of certain types.
    ///
    /// The [`Serialize`] type can add more trait bounds to this type to access
    /// additional extended functionality.
    type Extension;

    /// The type used for serializing sequences.
    type Seq<'s>: Seq<Parent = Self>;

    /// The type used for serializing tuples.
    type Tuple<'s>: Tuple<Parent = Self>;

    /// The type used for serializing tuple structs.
    type TupleStruct<'s>: TupleStruct<Parent = Self>;

    /// The type used for serializing structs.
    type Struct<'s>: Struct<Parent = Self>;

    /// The type used for serializing maps.
    type Map<'s>: Map<Parent = Self>;

    /// The type used for serializing tuple variants.
    type TupleVariant<'s>: TupleVariant<Parent = Self>;

    /// The type used for serializing struct variants.
    type StructVariant<'s>: StructVariant<Parent = Self>;

    /// Serialize an i8 value.
    fn emit_i8(&mut self, value: i8) -> Result<(), Self::Error>;

    /// Serialize an i16 value.
    fn emit_i16(&mut self, value: i16) -> Result<(), Self::Error>;

    /// Serialize an i32 value.
    fn emit_i32(&mut self, value: i32) -> Result<(), Self::Error>;

    /// Serialize an i64 value.
    fn emit_i64(&mut self, value: i64) -> Result<(), Self::Error>;

    /// Serialize a u8 value.
    fn emit_u8(&mut self, value: u8) -> Result<(), Self::Error>;

    /// Serialize a u16 value.
    fn emit_u16(&mut self, value: u16) -> Result<(), Self::Error>;

    /// Serialize a u32 value.
    fn emit_u32(&mut self, value: u32) -> Result<(), Self::Error>;

    /// Serialize a u64 value.
    fn emit_u64(&mut self, value: u64) -> Result<(), Self::Error>;

    /// Serialize an isize value.
    fn emit_isize(&mut self, value: isize) -> Result<(), Self::Error>;

    /// Serialize a usize value.
    fn emit_usize(&mut self, value: usize) -> Result<(), Self::Error>;

    /// Serialize an f32 value.
    fn emit_f32(&mut self, value: f32) -> Result<(), Self::Error>;

    /// Serialize an f64 value.
    fn emit_f64(&mut self, value: f64) -> Result<(), Self::Error>;

    /// Serialize a boolean value.
    fn emit_bool(&mut self, value: bool) -> Result<(), Self::Error>;

    /// Serialize a character value.
    fn emit_char(&mut self, value: char) -> Result<(), Self::Error>;

    /// Serialize a string slice.
    fn emit_str(&mut self, value: &str) -> Result<(), Self::Error>;

    /// Serialize a byte slice.
    fn emit_bytes(&mut self, value: &[u8]) -> Result<(), Self::Error>;

    /// Serialize a unit value (empty tuple).
    fn emit_unit(&mut self) -> Result<(), Self::Error>;

    /// Serialize a None option value.
    fn emit_none(&mut self) -> Result<(), Self::Error>;

    /// Serialize a Some option value.
    ///
    /// # Arguments
    ///
    /// * `value` - The contained value to serialize
    fn emit_some<T: Serialize<Self> + ?Sized>(
        &mut self,
        value: &T,
    ) -> Result<(), Self::Error>;

    /// Get a mutable reference to the extension object for customized
    /// serialization.
    fn extension(&mut self) -> &mut Self::Extension;

    /// Serialize a sequence (array, vector, etc.).
    ///
    /// # Arguments
    ///
    /// * `len` - The number of elements in the sequence
    /// * `f` - A closure that serializes the sequence elements
    fn emit_seq<'s>(
        &'s mut self,
        len: usize,
        f: impl FnOnce(Self::Seq<'s>) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error>;

    /// Serialize a tuple.
    ///
    /// # Arguments
    ///
    /// * `len` - The number of elements in the tuple
    /// * `f` - A closure that serializes the tuple elements
    fn emit_tuple<'s>(
        &'s mut self,
        len: usize,
        f: impl FnOnce(Self::Tuple<'s>) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error>;

    /// Serialize a tuple struct.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the tuple struct type
    /// * `len` - The number of fields in the tuple struct
    /// * `f` - A closure that serializes the tuple struct fields
    fn emit_tuple_struct<'s>(
        &'s mut self,
        name: &'static str,
        len: usize,
        f: impl FnOnce(Self::TupleStruct<'s>) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error>;

    /// Serialize a unit struct (struct with no fields).
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the unit struct type
    fn emit_unit_struct(
        &mut self,
        name: &'static str,
    ) -> Result<(), Self::Error>;

    /// Serialize a unit variant (enum variant with no fields).
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the enum type
    /// * `index` - The index of the variant within the enum
    fn emit_unit_variant(
        &mut self,
        name: &'static str,
        index: u32,
    ) -> Result<(), Self::Error>;

    /// Serialize a map (dictionary, hash table, etc.).
    ///
    /// # Arguments
    ///
    /// * `len` - The number of key-value pairs in the map
    /// * `f` - A closure that serializes the map entries
    fn emit_map<'s>(
        &'s mut self,
        len: usize,
        f: impl FnOnce(Self::Map<'s>) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error>;

    /// Serialize a struct.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the struct type
    /// * `len` - The number of fields in the struct
    /// * `f` - A closure that serializes the struct fields
    fn emit_struct<'s>(
        &'s mut self,
        name: &'static str,
        len: usize,
        f: impl FnOnce(Self::Struct<'s>) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error>;

    /// Serialize a tuple variant (enum variant with unnamed fields).
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the enum type
    /// * `variant` - The name of the variant
    /// * `index` - The index of the variant within the enum
    /// * `len` - The number of fields in the tuple variant
    /// * `f` - A closure that serializes the variant fields
    fn emit_tuple_variant<'s>(
        &'s mut self,
        name: &'static str,
        variant: &'static str,
        index: u32,
        len: usize,
        f: impl FnOnce(Self::TupleVariant<'s>) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error>;

    /// Serialize a struct variant (enum variant with named fields).
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the enum type
    /// * `variant` - The name of the variant
    /// * `index` - The index of the variant within the enum
    /// * `len` - The number of fields in the struct variant
    /// * `f` - A closure that serializes the variant fields
    fn emit_struct_variant<'s>(
        &'s mut self,
        name: &'static str,
        variant: &'static str,
        index: u32,
        len: usize,
        f: impl FnOnce(Self::StructVariant<'s>) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error>;
}

/// A trait for types that can be serialized using a [`Serializer`].
///
/// This trait should be implemented for any type that needs to be serialized.
/// The implementation defines how the type's data should be written to the
/// serializer.
pub trait Serialize<S: ?Sized + Serializer> {
    /// Serialize this value using the provided serializer.
    ///
    /// # Arguments
    ///
    /// * `serializer` - The serializer to write this value to
    ///
    /// # Errors
    ///
    /// Returns an error if the value cannot be serialized.
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error>;
}

// =============================================================================
// Primitive Type Implementations
// =============================================================================

macro_rules! impl_serialize_integer {
    ($($ty:ty => $method:ident),*) => {
        $(
            impl<S> Serialize<S> for $ty
            where
                S: Serializer,
            {
                fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
                    serializer.$method(*self)
                }
            }
        )*
    };
}

impl_serialize_integer! {
    i8 => emit_i8,
    i16 => emit_i16,
    i32 => emit_i32,
    i64 => emit_i64,
    u8 => emit_u8,
    u16 => emit_u16,
    u32 => emit_u32,
    u64 => emit_u64,
    isize => emit_isize,
    usize => emit_usize
}

impl<S> Serialize<S> for f32
where
    S: Serializer,
{
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
        serializer.emit_f32(*self)
    }
}

impl<S> Serialize<S> for f64
where
    S: Serializer,
{
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
        serializer.emit_f64(*self)
    }
}

impl<S> Serialize<S> for bool
where
    S: Serializer,
{
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
        serializer.emit_bool(*self)
    }
}

impl<S> Serialize<S> for char
where
    S: Serializer,
{
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
        serializer.emit_char(*self)
    }
}

impl<S> Serialize<S> for str
where
    S: Serializer,
{
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
        serializer.emit_str(self)
    }
}

impl<S> Serialize<S> for String
where
    S: Serializer,
{
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
        serializer.emit_str(self)
    }
}

// =============================================================================
// Collection Implementations
// =============================================================================

use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet, LinkedList, VecDeque},
    hash::BuildHasher,
};

impl<T, S> Serialize<S> for Vec<T>
where
    T: Serialize<S>,
    S: Serializer,
{
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
        serializer.emit_seq(self.len(), |mut seq| {
            for item in self {
                seq.serialize_element(item)?;
            }
            Ok(())
        })
    }
}

impl<T, S> Serialize<S> for [T]
where
    T: Serialize<S>,
    S: Serializer,
{
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
        serializer.emit_seq(self.len(), |mut seq| {
            for item in self {
                seq.serialize_element(item)?;
            }
            Ok(())
        })
    }
}

impl<T, const N: usize, S> Serialize<S> for [T; N]
where
    T: Serialize<S>,
    S: Serializer,
{
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
        serializer.emit_seq(N, |mut seq| {
            for item in self {
                seq.serialize_element(item)?;
            }
            Ok(())
        })
    }
}

impl<K, V, BH, S> Serialize<S> for HashMap<K, V, BH>
where
    K: Serialize<S>,
    V: Serialize<S>,
    BH: BuildHasher,
    S: Serializer,
{
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
        serializer.emit_map(self.len(), |mut map| {
            for (key, value) in self {
                map.serialize_entry(key, value)?;
            }
            Ok(())
        })
    }
}

impl<K, V, S> Serialize<S> for BTreeMap<K, V>
where
    K: Serialize<S>,
    V: Serialize<S>,
    S: Serializer,
{
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
        serializer.emit_map(self.len(), |mut map| {
            for (key, value) in self {
                map.serialize_entry(key, value)?;
            }
            Ok(())
        })
    }
}

impl<T, BH, S> Serialize<S> for HashSet<T, BH>
where
    T: Serialize<S>,
    BH: BuildHasher,
    S: Serializer,
{
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
        serializer.emit_seq(self.len(), |mut seq| {
            for item in self {
                seq.serialize_element(item)?;
            }
            Ok(())
        })
    }
}

impl<T, S> Serialize<S> for BTreeSet<T>
where
    T: Serialize<S>,
    S: Serializer,
{
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
        serializer.emit_seq(self.len(), |mut seq| {
            for item in self {
                seq.serialize_element(item)?;
            }
            Ok(())
        })
    }
}

impl<T, S> Serialize<S> for VecDeque<T>
where
    T: Serialize<S>,
    S: Serializer,
{
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
        serializer.emit_seq(self.len(), |mut seq| {
            for item in self {
                seq.serialize_element(item)?;
            }
            Ok(())
        })
    }
}

impl<T, S> Serialize<S> for LinkedList<T>
where
    T: Serialize<S>,
    S: Serializer,
{
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
        serializer.emit_seq(self.len(), |mut seq| {
            for item in self {
                seq.serialize_element(item)?;
            }
            Ok(())
        })
    }
}

// =============================================================================
// Option and Result Implementations
// =============================================================================

impl<T, S> Serialize<S> for Option<T>
where
    T: Serialize<S>,
    S: Serializer,
{
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
        match self {
            Some(value) => serializer.emit_some(value),
            None => serializer.emit_none(),
        }
    }
}

impl<T, E, S> Serialize<S> for Result<T, E>
where
    T: Serialize<S>,
    E: Serialize<S>,
    S: Serializer,
{
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
        match self {
            Ok(value) => serializer.emit_tuple_variant(
                "Result",
                "Ok",
                0,
                1,
                |mut variant| {
                    TupleVariant::serialize_field(&mut variant, value)
                },
            ),
            Err(error) => serializer.emit_tuple_variant(
                "Result",
                "Err",
                1,
                1,
                |mut variant| {
                    TupleVariant::serialize_field(&mut variant, error)
                },
            ),
        }
    }
}

// =============================================================================
// Tuple Implementations
// =============================================================================

impl<S> Serialize<S> for ()
where
    S: Serializer,
{
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
        serializer.emit_unit()
    }
}

macro_rules! impl_serialize_tuple {
    ($($len:expr => ($($idx:tt $T:ident),+)),*) => {
        $(
            impl<$($T,)* S> Serialize<S> for ($($T,)*)
            where
                $($T: Serialize<S>,)*
                S: Serializer,
            {
                fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
                    serializer.emit_tuple($len, |mut tuple| {
                        $(
                            tuple.serialize_element(&self.$idx)?;
                        )*
                        Ok(())
                    })
                }
            }
        )*
    };
}

impl_serialize_tuple! {
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

impl<T, S> Serialize<S> for &T
where
    T: Serialize<S> + ?Sized,
    S: Serializer,
{
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
        (**self).serialize(serializer)
    }
}

impl<T, S> Serialize<S> for &mut T
where
    T: Serialize<S> + ?Sized,
    S: Serializer,
{
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
        (**self).serialize(serializer)
    }
}

use std::{borrow::Cow, boxed::Box};

impl<T, S> Serialize<S> for Box<T>
where
    T: Serialize<S> + ?Sized,
    S: Serializer,
{
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
        (**self).serialize(serializer)
    }
}

impl<T, S> Serialize<S> for Cow<'_, T>
where
    T: Serialize<S> + ToOwned + ?Sized,
    S: Serializer,
{
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
        (**self).serialize(serializer)
    }
}

// =============================================================================
// Additional Standard Library Types
// =============================================================================

use std::marker::PhantomData;

impl<T, S> Serialize<S> for PhantomData<T>
where
    S: Serializer,
{
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
        serializer.emit_tuple_struct("PhantomData", 1, |_| Ok(()))
    }
}
