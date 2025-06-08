//! Serialization traits and interfaces for the Pernix compiler.
//!
//! This module provides a custom serialization framework similar to serde, but
//! with additional extension capabilities for specialized serialization
//! contexts. The framework is designed to be flexible and extensible, allowing
//! serializers to carry additional state and context through the
//! [`Serializer::Extension`] associated type.
//!
//! # Overview
//!
//! The serialization framework consists of several key traits:
//!
//! - [`Serializer`] - The main trait that defines the serialization interface
//! - [`Serialize`] - Trait for types that can be serialized
//! - Compound serialization traits for different data structures:
//!   - [`Seq`] - For sequences like arrays and vectors
//!   - [`Tuple`] - For tuples
//!   - [`TupleStruct`] - For tuple structs
//!   - [`Struct`] - For structs with named fields
//!   - [`Map`] - For key-value collections
//!   - [`TupleVariant`] - For enum tuple variants
//!   - [`StructVariant`] - For enum struct variants
//!
//! # Extension Mechanism
//!
//! Unlike standard serde, this framework includes an extension mechanism via
//! the [`Serializer::Extension`] associated type. This allows serializers to
//! carry additional context or state that can be accessed during serialization
//! for specialized behavior.
//!
//! # Example Usage
//!
//! ```rust,ignore
//! use crate::ser::{Serializer, Serialize};
//!
//! // Define a custom serializer
//! struct MySerializer {
//!     // serializer state
//! }
//!
//! impl Serializer for MySerializer {
//!     type Error = MyError;
//!     type Extension = MyExtension;
//!     // ... other associated types and methods
//! }
//!
//! // Implement Serialize for custom types
//! impl<S: Serializer> Serialize<S> for MyStruct {
//!     fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
//!         // Use serializer methods to serialize self
//!         serializer.emit_struct("MyStruct", 2, |s| {
//!             s.serialize_field("field1", &self.field1)?;
//!             s.serialize_field("field2", &self.field2)
//!         })
//!     }
//! }
//! ```

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
    type Error;

    /// An extension object that can be used as an additional context for
    /// specialized serialization of certain types.
    ///
    /// The [`Serialize`] type can add more trait bounds to this type to access
    /// additional extended functionality.
    type Extension;

    /// The type used for serializing sequences.
    type Seq: Seq<Parent = Self>;

    /// The type used for serializing tuples.
    type Tuple: Tuple<Parent = Self>;

    /// The type used for serializing tuple structs.
    type TupleStruct: TupleStruct<Parent = Self>;

    /// The type used for serializing structs.
    type Struct: Struct<Parent = Self>;

    /// The type used for serializing maps.
    type Map: Map<Parent = Self>;

    /// The type used for serializing tuple variants.
    type TupleVariant: TupleVariant<Parent = Self>;

    /// The type used for serializing struct variants.
    type StructVariant: StructVariant<Parent = Self>;

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
    fn emit_seq(
        &mut self,
        len: usize,
        f: impl FnOnce(&mut Self::Seq) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error>;

    /// Serialize a tuple.
    ///
    /// # Arguments
    ///
    /// * `len` - The number of elements in the tuple
    /// * `f` - A closure that serializes the tuple elements
    fn emit_tuple(
        &mut self,
        len: usize,
        f: impl FnOnce(&mut Self::Tuple) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error>;

    /// Serialize a tuple struct.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the tuple struct type
    /// * `len` - The number of fields in the tuple struct
    /// * `f` - A closure that serializes the tuple struct fields
    fn emit_tuple_struct(
        &mut self,
        name: &'static str,
        len: usize,
        f: impl FnOnce(&mut Self::TupleStruct) -> Result<(), Self::Error>,
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
    fn emit_map(
        &mut self,
        len: usize,
        f: impl FnOnce(&mut Self::Map) -> Result<(), Self::Error>,
    );

    /// Serialize a struct.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the struct type
    /// * `len` - The number of fields in the struct
    /// * `f` - A closure that serializes the struct fields
    fn emit_struct(
        &mut self,
        name: &'static str,
        len: usize,
        f: impl FnOnce(&mut Self::Struct) -> Result<(), Self::Error>,
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
    fn emit_tuple_variant(
        &mut self,
        name: &'static str,
        variant: &'static str,
        index: u32,
        len: usize,
        f: impl FnOnce(&mut Self::TupleVariant) -> Result<(), Self::Error>,
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
    fn emit_struct_variant(
        &mut self,
        name: &'static str,
        variant: &'static str,
        index: u32,
        len: usize,
        f: impl FnOnce(&mut Self::StructVariant) -> Result<(), Self::Error>,
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
