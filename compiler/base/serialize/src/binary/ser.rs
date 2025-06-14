//! Binary serializer implementation.
//!
//! This module provides a compact binary serializer that writes data to any
//! type implementing `std::io::Write`. The format is designed to be simple,
//! compact, and efficient.
//!
//! # Examples
//!
//! ```rust
//! use pernixc_serializer::{binary::ser::BinarySerializer, ser::Serialize};
//!
//! let mut buffer = Vec::new();
//! let mut serializer = BinarySerializer::new(buffer);
//!
//! // Serialize a primitive value
//! 42u32.serialize(&mut serializer).unwrap();
//! let buffer = serializer.into_inner();
//! assert_eq!(buffer, 42u32.to_le_bytes());
//! ```
//!
//! # Binary Format
//!
//! The binary format uses the following conventions:
//!
//! - **Integers**: Written in little-endian format
//! - **Strings**: Length-prefixed with varint encoding
//! - **Sequences/Collections**: Length-prefixed with varint encoding
//! - **Maps**: Length-prefixed with varint encoding
//! - **Enums**: Variant index encoded as varint, followed by variant data
//! - **Booleans**: Single byte (0 for false, 1 for true)
//! - **Floating Point**: IEEE 754 format in little-endian
//!
//! # Varint Encoding
//!
//! Variable-length integers (varints) are used for length prefixes and enum
//! variants. Each byte contains 7 bits of data with the high bit indicating if
//! more bytes follow. This provides compact encoding for small numbers while
//! supporting arbitrarily large values.

use std::io::{self, Write};

use crate::ser::{
    Map, Seq, Serializer, Struct, StructVariant, Tuple, TupleStruct,
    TupleVariant,
};

/// A binary serializer that writes to any `Write` implementation.
///
/// The serializer uses a simple binary format:
/// - Integers are written in little-endian format
/// - Strings are prefixed with their length as a varint
/// - Sequences and maps are prefixed with their length as a varint
/// - Enums are prefixed with their variant index as a varint
/// - Booleans are written as single bytes (0 or 1)
/// - Floating point numbers are written in IEEE 754 format
pub struct BinarySerializer<W, E = ()> {
    writer: W,
    extension: E,
}

impl<W: Write + 'static, E: 'static> BinarySerializer<W, E> {
    /// Create a new binary serializer that writes to the given writer with the
    /// specified extension.
    pub fn with_extension(writer: W, extension: E) -> Self {
        Self { writer, extension }
    }

    /// Consume the serializer and return the underlying writer and extension.
    pub fn into_parts(self) -> (W, E) { (self.writer, self.extension) }

    /// Consume the serializer and return the underlying writer.
    pub fn into_inner(self) -> W { self.writer }

    /// Get a reference to the underlying writer.
    pub fn writer(&self) -> &W { &self.writer }

    /// Get a mutable reference to the underlying writer.
    pub fn writer_mut(&mut self) -> &mut W { &mut self.writer }

    /// Get a reference to the extension.
    pub fn extension(&self) -> &E { &self.extension }

    /// Get a mutable reference to the extension.
    pub fn extension_mut(&mut self) -> &mut E { &mut self.extension }

    /// Write a varint (variable-length integer) encoding of the given value.
    ///
    /// This uses a simple encoding where each byte contains 7 bits of data
    /// and the high bit indicates whether more bytes follow.
    fn write_varint(&mut self, mut value: u64) -> Result<(), io::Error> {
        loop {
            let byte = (value & 0x7F) as u8;
            value >>= 7;
            if value == 0 {
                self.writer.write_all(&[byte])?;
                break;
            } else {
                self.writer.write_all(&[byte | 0x80])?;
            }
        }
        Ok(())
    }

    /// Write raw bytes to the writer.
    fn write_bytes(&mut self, bytes: &[u8]) -> Result<(), io::Error> {
        self.writer.write_all(bytes)
    }
}

// Compound serializer implementations
pub struct BinarySeq<'a, W, E> {
    serializer: &'a mut BinarySerializer<W, E>,
}

impl<W: Write + 'static, E: 'static> Seq for BinarySeq<'_, W, E> {
    type Parent = BinarySerializer<W, E>;

    fn serialize_element<T: crate::ser::Serialize<Self::Parent> + ?Sized>(
        &mut self,
        value: &T,
    ) -> Result<(), io::Error> {
        value.serialize(self.serializer)
    }
}

pub struct BinaryTuple<'a, W, E> {
    serializer: &'a mut BinarySerializer<W, E>,
}

impl<W: Write + 'static, E: 'static> Tuple for BinaryTuple<'_, W, E> {
    type Parent = BinarySerializer<W, E>;

    fn serialize_element<T: crate::ser::Serialize<Self::Parent> + ?Sized>(
        &mut self,
        value: &T,
    ) -> Result<(), io::Error> {
        value.serialize(self.serializer)
    }
}

pub struct BinaryTupleStruct<'a, W, E> {
    serializer: &'a mut BinarySerializer<W, E>,
}

impl<W: Write + 'static, E: 'static> TupleStruct
    for BinaryTupleStruct<'_, W, E>
{
    type Parent = BinarySerializer<W, E>;

    fn serialize_field<T: crate::ser::Serialize<Self::Parent> + ?Sized>(
        &mut self,
        value: &T,
    ) -> Result<(), io::Error> {
        value.serialize(self.serializer)
    }
}

pub struct BinaryStruct<'a, W, E> {
    serializer: &'a mut BinarySerializer<W, E>,
}

impl<W: Write + 'static, E: 'static> Struct for BinaryStruct<'_, W, E> {
    type Parent = BinarySerializer<W, E>;

    fn serialize_field<T: crate::ser::Serialize<Self::Parent> + ?Sized>(
        &mut self,
        _name: &'static str,
        value: &T,
    ) -> Result<(), io::Error> {
        value.serialize(self.serializer)
    }

    fn skip_field(&mut self, _name: &'static str) -> Result<(), io::Error> {
        // For binary format, we don't output anything for skipped fields
        Ok(())
    }
}

pub struct BinaryMap<'a, W, E> {
    serializer: &'a mut BinarySerializer<W, E>,
}

impl<W: Write + 'static, E: 'static> Map for BinaryMap<'_, W, E> {
    type Parent = BinarySerializer<W, E>;

    fn serialize_entry<
        K: crate::ser::Serialize<Self::Parent> + ?Sized,
        V: crate::ser::Serialize<Self::Parent> + ?Sized,
    >(
        &mut self,
        key: &K,
        value: &V,
    ) -> Result<(), io::Error> {
        key.serialize(self.serializer)?;
        value.serialize(self.serializer)
    }
}

pub struct BinaryTupleVariant<'a, W, E> {
    serializer: &'a mut BinarySerializer<W, E>,
}

impl<W: Write + 'static, E: 'static> TupleVariant
    for BinaryTupleVariant<'_, W, E>
{
    type Parent = BinarySerializer<W, E>;

    fn serialize_field<T: crate::ser::Serialize<Self::Parent> + ?Sized>(
        &mut self,
        value: &T,
    ) -> Result<(), io::Error> {
        value.serialize(self.serializer)
    }
}

pub struct BinaryStructVariant<'a, W, E> {
    serializer: &'a mut BinarySerializer<W, E>,
}

impl<W: Write + 'static, E: 'static> StructVariant
    for BinaryStructVariant<'_, W, E>
{
    type Parent = BinarySerializer<W, E>;

    fn serialize_field<T: crate::ser::Serialize<Self::Parent> + ?Sized>(
        &mut self,
        _name: &'static str,
        value: &T,
    ) -> Result<(), io::Error> {
        value.serialize(self.serializer)
    }

    fn skip_field(&mut self, _name: &'static str) -> Result<(), io::Error> {
        // For binary format, we don't output anything for skipped fields
        Ok(())
    }
}

impl<W: Write + 'static, E: 'static> Serializer for BinarySerializer<W, E> {
    type Error = io::Error;
    type Extension = E;

    type Seq<'s> = BinarySeq<'s, W, E>;
    type Tuple<'s> = BinaryTuple<'s, W, E>;
    type TupleStruct<'s> = BinaryTupleStruct<'s, W, E>;
    type Struct<'s> = BinaryStruct<'s, W, E>;
    type Map<'s> = BinaryMap<'s, W, E>;
    type TupleVariant<'s> = BinaryTupleVariant<'s, W, E>;
    type StructVariant<'s> = BinaryStructVariant<'s, W, E>;

    fn emit_i8(&mut self, value: i8) -> Result<(), Self::Error> {
        self.write_bytes(&value.to_le_bytes())
    }

    fn emit_i16(&mut self, value: i16) -> Result<(), Self::Error> {
        self.write_bytes(&value.to_le_bytes())
    }

    fn emit_i32(&mut self, value: i32) -> Result<(), Self::Error> {
        self.write_bytes(&value.to_le_bytes())
    }

    fn emit_i64(&mut self, value: i64) -> Result<(), Self::Error> {
        self.write_bytes(&value.to_le_bytes())
    }

    fn emit_u8(&mut self, value: u8) -> Result<(), Self::Error> {
        self.write_bytes(&[value])
    }

    fn emit_u16(&mut self, value: u16) -> Result<(), Self::Error> {
        self.write_bytes(&value.to_le_bytes())
    }

    fn emit_u32(&mut self, value: u32) -> Result<(), Self::Error> {
        self.write_bytes(&value.to_le_bytes())
    }

    fn emit_u64(&mut self, value: u64) -> Result<(), Self::Error> {
        self.write_bytes(&value.to_le_bytes())
    }

    fn emit_isize(&mut self, value: isize) -> Result<(), Self::Error> {
        self.write_bytes(&value.to_le_bytes())
    }

    fn emit_usize(&mut self, value: usize) -> Result<(), Self::Error> {
        self.write_bytes(&value.to_le_bytes())
    }

    fn emit_f32(&mut self, value: f32) -> Result<(), Self::Error> {
        self.write_bytes(&value.to_le_bytes())
    }

    fn emit_f64(&mut self, value: f64) -> Result<(), Self::Error> {
        self.write_bytes(&value.to_le_bytes())
    }

    fn emit_bool(&mut self, value: bool) -> Result<(), Self::Error> {
        self.write_bytes(&[if value { 1 } else { 0 }])
    }

    fn emit_char(&mut self, value: char) -> Result<(), Self::Error> {
        // Encode char as UTF-8 bytes prefixed with length
        let mut buf = [0u8; 4];
        let s = value.encode_utf8(&mut buf);
        self.write_varint(s.len() as u64)?;
        self.write_bytes(s.as_bytes())
    }

    fn emit_str(&mut self, value: &str) -> Result<(), Self::Error> {
        self.write_varint(value.len() as u64)?;
        self.write_bytes(value.as_bytes())
    }

    fn emit_bytes(&mut self, value: &[u8]) -> Result<(), Self::Error> {
        self.write_varint(value.len() as u64)?;
        self.write_bytes(value)
    }

    fn emit_unit(&mut self) -> Result<(), Self::Error> {
        // Unit type requires no data
        Ok(())
    }

    fn emit_none(&mut self) -> Result<(), Self::Error> {
        // Write 0 to indicate None
        self.write_bytes(&[0])
    }

    fn emit_some<T: crate::ser::Serialize<Self> + ?Sized>(
        &mut self,
        value: &T,
    ) -> Result<(), Self::Error> {
        // Write 1 to indicate Some, then serialize the value
        self.write_bytes(&[1])?;
        value.serialize(self)
    }

    fn extension(&mut self) -> &mut Self::Extension { &mut self.extension }

    fn emit_seq<'s>(
        &'s mut self,
        len: usize,
        f: impl FnOnce(Self::Seq<'s>) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        self.write_varint(len as u64)?;
        let seq = BinarySeq { serializer: self };
        f(seq)
    }

    fn emit_tuple<'s>(
        &'s mut self,
        _len: usize,
        f: impl FnOnce(Self::Tuple<'s>) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        // Tuples have fixed size, so no need to write length
        let tuple = BinaryTuple { serializer: self };
        f(tuple)
    }

    fn emit_tuple_struct<'s>(
        &'s mut self,
        _name: &'static str,
        _len: usize,
        f: impl FnOnce(Self::TupleStruct<'s>) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        // Tuple structs have fixed size, so no need to write length or name
        let tuple_struct = BinaryTupleStruct { serializer: self };
        f(tuple_struct)
    }

    fn emit_unit_struct(
        &mut self,
        _name: &'static str,
    ) -> Result<(), Self::Error> {
        // Unit structs require no data
        Ok(())
    }

    fn emit_unit_variant(
        &mut self,
        _name: &'static str,
        index: u32,
    ) -> Result<(), Self::Error> {
        // Write the variant index
        self.write_varint(index as u64)
    }

    fn emit_map<'s>(
        &'s mut self,
        len: usize,
        f: impl FnOnce(Self::Map<'s>) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        self.write_varint(len as u64)?;
        let map = BinaryMap { serializer: self };
        f(map)
    }

    fn emit_struct<'s>(
        &'s mut self,
        _name: &'static str,
        _len: usize,
        f: impl FnOnce(Self::Struct<'s>) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        // Structs have fixed fields, so no need to write length or name
        let struct_ser = BinaryStruct { serializer: self };
        f(struct_ser)
    }

    fn emit_tuple_variant<'s>(
        &'s mut self,
        _name: &'static str,
        _variant: &'static str,
        index: u32,
        _len: usize,
        f: impl FnOnce(Self::TupleVariant<'s>) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        self.write_varint(index as u64)?;
        let tuple_variant = BinaryTupleVariant { serializer: self };
        f(tuple_variant)
    }

    fn emit_struct_variant<'s>(
        &'s mut self,
        _name: &'static str,
        _variant: &'static str,
        index: u32,
        _len: usize,
        f: impl FnOnce(Self::StructVariant<'s>) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        self.write_varint(index as u64)?;
        let struct_variant = BinaryStructVariant { serializer: self };
        f(struct_variant)
    }
}

impl<W: Write + 'static> BinarySerializer<W, ()> {
    /// Create a new binary serializer that writes to the given writer with a
    /// unit extension.
    pub fn new(writer: W) -> Self { Self::with_extension(writer, ()) }
}

impl crate::ser::Error for io::Error {
    fn custom<T>(msg: T) -> Self
    where
        T: std::fmt::Display,
    {
        io::Error::new(io::ErrorKind::Other, msg.to_string())
    }
}

#[cfg(test)]
mod test;
