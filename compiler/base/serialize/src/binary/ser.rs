//! Binary serializer implementation.
//!
//! This module provides a compact binary serializer that writes data to any
//! type implementing `std::io::Write`. The format is designed to be simple,
//! compact, and efficient.
//!
//! # Examples
//!
//! ```rust
//! use pernixc_serialize::{binary::ser::BinarySerializer, ser::Serialize};
//!
//! let mut buffer = Vec::new();
//! let mut serializer = BinarySerializer::new(buffer);
//!
//! // Serialize a primitive value - u32 is now varint encoded
//! 42u32.serialize(&mut serializer, &mut ()).unwrap();
//! let buffer = serializer.into_inner();
//! assert_eq!(buffer, vec![42]); // 42 < 128, so single byte varint
//! ```
//!
//! # Binary Format
//!
//! The binary format uses the following conventions:
//!
//! - **Integers (i8, u8)**: Single byte, stored as-is
//! - **Integers (i16, i32, i64, i128, isize)**: Varint encoded with zigzag
//!   encoding for signed types
//! - **Integers (u16, u32, u64, u128, usize)**: Varint encoded
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
pub struct BinarySerializer<W> {
    writer: W,
}

impl<W: Write + 'static> BinarySerializer<W> {
    /// Consume the serializer and return the underlying writer.
    pub fn into_inner(self) -> W { self.writer }

    /// Get a reference to the underlying writer.
    pub fn writer(&self) -> &W { &self.writer }

    /// Get a mutable reference to the underlying writer.
    pub fn writer_mut(&mut self) -> &mut W { &mut self.writer }

    /// Write a varint (variable-length integer) encoding using a generic
    /// implementation.
    ///
    /// This uses a simple encoding where each byte contains 7 bits of data
    /// and the high bit indicates whether more bytes follow.
    fn write_varint_generic<T>(&mut self, mut value: T) -> Result<(), io::Error>
    where
        T: Copy
            + PartialEq
            + From<u8>
            + std::ops::BitAnd<Output = T>
            + std::ops::ShrAssign<u8>,
        u8: TryFrom<T>,
    {
        loop {
            let byte = u8::try_from(value & T::from(0x7F)).map_err(|_| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    "Invalid varint conversion",
                )
            })?;
            value >>= 7;
            if value == T::from(0) {
                self.writer.write_all(&[byte])?;
                break;
            } else {
                self.writer.write_all(&[byte | 0x80])?;
            }
        }
        Ok(())
    }

    /// Write a varint (variable-length integer) encoding of the given u64
    /// value.
    fn write_varint(&mut self, value: u64) -> Result<(), io::Error> {
        self.write_varint_generic(value)
    }

    /// Write a 128-bit varint (variable-length integer) encoding.
    fn write_varint_u128(&mut self, value: u128) -> Result<(), io::Error> {
        self.write_varint_generic(value)
    }

    /// Zigzag encode a signed integer to move the sign bit to the LSB.
    /// This allows small negative numbers to be encoded compactly.
    fn zigzag_encode_i16(value: i16) -> u16 {
        ((value << 1) ^ (value >> 15)) as u16
    }

    fn zigzag_encode_i32(value: i32) -> u32 {
        ((value << 1) ^ (value >> 31)) as u32
    }

    fn zigzag_encode_i64(value: i64) -> u64 {
        ((value << 1) ^ (value >> 63)) as u64
    }

    fn zigzag_encode_i128(value: i128) -> u128 {
        ((value << 1) ^ (value >> 127)) as u128
    }

    fn zigzag_encode_isize(value: isize) -> usize {
        ((value << 1) ^ (value >> (std::mem::size_of::<isize>() * 8 - 1)))
            as usize
    }

    /// Write a varint-encoded u16
    fn write_varint_u16(&mut self, value: u16) -> Result<(), io::Error> {
        self.write_varint(value as u64)
    }

    /// Write a varint-encoded u32
    fn write_varint_u32(&mut self, value: u32) -> Result<(), io::Error> {
        self.write_varint(value as u64)
    }

    /// Write a varint-encoded u64
    fn write_varint_u64(&mut self, value: u64) -> Result<(), io::Error> {
        self.write_varint(value)
    }

    /// Write a varint-encoded usize
    fn write_varint_usize(&mut self, value: usize) -> Result<(), io::Error> {
        self.write_varint(value as u64)
    }

    /// Write a zigzag + varint encoded i16
    fn write_varint_i16(&mut self, value: i16) -> Result<(), io::Error> {
        let encoded = Self::zigzag_encode_i16(value);
        self.write_varint_u16(encoded)
    }

    /// Write a zigzag + varint encoded i32
    fn write_varint_i32(&mut self, value: i32) -> Result<(), io::Error> {
        let encoded = Self::zigzag_encode_i32(value);
        self.write_varint_u32(encoded)
    }

    /// Write a zigzag + varint encoded i64
    fn write_varint_i64(&mut self, value: i64) -> Result<(), io::Error> {
        let encoded = Self::zigzag_encode_i64(value);
        self.write_varint_u64(encoded)
    }

    /// Write a zigzag + varint encoded isize
    fn write_varint_isize(&mut self, value: isize) -> Result<(), io::Error> {
        let encoded = Self::zigzag_encode_isize(value);
        self.write_varint_usize(encoded)
    }

    /// Write a zigzag + varint encoded i128
    fn write_varint_i128(&mut self, value: i128) -> Result<(), io::Error> {
        let encoded = Self::zigzag_encode_i128(value);
        self.write_varint_u128(encoded)
    }

    /// Write raw bytes to the writer.
    fn write_bytes(&mut self, bytes: &[u8]) -> Result<(), io::Error> {
        self.writer.write_all(bytes)
    }
}

// Compound serializer implementations
pub struct BinarySeq<'a, W> {
    serializer: &'a mut BinarySerializer<W>,
}

impl<W: Write + 'static, E> Seq<E> for BinarySeq<'_, W> {
    type Parent = BinarySerializer<W>;

    fn serialize_element<T: crate::ser::Serialize<Self::Parent, E>>(
        &mut self,
        value: &T,
        extension: &E,
    ) -> Result<(), io::Error> {
        value.serialize(self.serializer, extension)
    }
}

pub struct BinaryTuple<'a, W> {
    serializer: &'a mut BinarySerializer<W>,
}

impl<W: Write + 'static, E> Tuple<E> for BinaryTuple<'_, W> {
    type Parent = BinarySerializer<W>;

    fn serialize_element<T: crate::ser::Serialize<Self::Parent, E>>(
        &mut self,
        value: &T,
        extension: &E,
    ) -> Result<(), io::Error> {
        value.serialize(self.serializer, extension)
    }
}

pub struct BinaryTupleStruct<'a, W> {
    serializer: &'a mut BinarySerializer<W>,
}

impl<W: Write + 'static, E> TupleStruct<E> for BinaryTupleStruct<'_, W> {
    type Parent = BinarySerializer<W>;

    fn serialize_field<T: crate::ser::Serialize<Self::Parent, E>>(
        &mut self,
        value: &T,
        extension: &E,
    ) -> Result<(), io::Error> {
        value.serialize(self.serializer, extension)
    }
}

pub struct BinaryStruct<'a, W> {
    serializer: &'a mut BinarySerializer<W>,
}

impl<W: Write + 'static, E> Struct<E> for BinaryStruct<'_, W> {
    type Parent = BinarySerializer<W>;

    fn serialize_field<T: crate::ser::Serialize<Self::Parent, E>>(
        &mut self,
        _name: &'static str,
        value: &T,
        extension: &E,
    ) -> Result<(), io::Error> {
        value.serialize(self.serializer, extension)
    }

    fn skip_field(
        &mut self,
        _name: &'static str,
        _extension: &E,
    ) -> Result<(), io::Error> {
        // For binary format, we don't output anything for skipped fields
        Ok(())
    }
}

pub struct BinaryMap<'a, W> {
    serializer: &'a mut BinarySerializer<W>,
}

impl<W: Write + 'static, E> Map<E> for BinaryMap<'_, W> {
    type Parent = BinarySerializer<W>;

    fn serialize_entry<
        K: crate::ser::Serialize<Self::Parent, E>,
        V: crate::ser::Serialize<Self::Parent, E>,
    >(
        &mut self,
        key: &K,
        value: &V,
        extension: &E,
    ) -> Result<(), io::Error> {
        key.serialize(self.serializer, extension)?;
        value.serialize(self.serializer, extension)
    }
}

pub struct BinaryTupleVariant<'a, W> {
    serializer: &'a mut BinarySerializer<W>,
}

impl<W: Write + 'static, E> TupleVariant<E> for BinaryTupleVariant<'_, W> {
    type Parent = BinarySerializer<W>;

    fn serialize_field<T: crate::ser::Serialize<Self::Parent, E>>(
        &mut self,
        value: &T,
        extension: &E,
    ) -> Result<(), io::Error> {
        value.serialize(self.serializer, extension)
    }
}

pub struct BinaryStructVariant<'a, W> {
    serializer: &'a mut BinarySerializer<W>,
}

impl<W: Write + 'static, E> StructVariant<E> for BinaryStructVariant<'_, W> {
    type Parent = BinarySerializer<W>;

    fn serialize_field<T: crate::ser::Serialize<Self::Parent, E>>(
        &mut self,
        _name: &'static str,
        value: &T,
        extension: &E,
    ) -> Result<(), io::Error> {
        value.serialize(self.serializer, extension)
    }

    fn skip_field(
        &mut self,
        _name: &'static str,
        _extension: &E,
    ) -> Result<(), io::Error> {
        // For binary format, we don't output anything for skipped fields
        Ok(())
    }
}

impl<W: Write + 'static, E> Serializer<E> for BinarySerializer<W> {
    type Error = io::Error;

    type Seq<'s> = BinarySeq<'s, W>;
    type Tuple<'s> = BinaryTuple<'s, W>;
    type TupleStruct<'s> = BinaryTupleStruct<'s, W>;
    type Struct<'s> = BinaryStruct<'s, W>;
    type Map<'s> = BinaryMap<'s, W>;
    type TupleVariant<'s> = BinaryTupleVariant<'s, W>;
    type StructVariant<'s> = BinaryStructVariant<'s, W>;

    fn emit_i8(&mut self, value: i8) -> Result<(), Self::Error> {
        // i8 is stored as-is (single byte)
        self.write_bytes(&value.to_le_bytes())
    }

    fn emit_i16(&mut self, value: i16) -> Result<(), Self::Error> {
        // i16 is varint encoded with zigzag
        self.write_varint_i16(value)
    }

    fn emit_i32(&mut self, value: i32) -> Result<(), Self::Error> {
        // i32 is varint encoded with zigzag
        self.write_varint_i32(value)
    }

    fn emit_i64(&mut self, value: i64) -> Result<(), Self::Error> {
        // i64 is varint encoded with zigzag
        self.write_varint_i64(value)
    }

    fn emit_u8(&mut self, value: u8) -> Result<(), Self::Error> {
        // u8 is stored as-is (single byte)
        self.write_bytes(&[value])
    }

    fn emit_u16(&mut self, value: u16) -> Result<(), Self::Error> {
        // u16 is varint encoded
        self.write_varint_u16(value)
    }

    fn emit_u32(&mut self, value: u32) -> Result<(), Self::Error> {
        // u32 is varint encoded
        self.write_varint_u32(value)
    }

    fn emit_u64(&mut self, value: u64) -> Result<(), Self::Error> {
        // u64 is varint encoded
        self.write_varint_u64(value)
    }

    fn emit_i128(&mut self, value: i128) -> Result<(), Self::Error> {
        // i128 is varint encoded with zigzag
        self.write_varint_i128(value)
    }

    fn emit_u128(&mut self, value: u128) -> Result<(), Self::Error> {
        // u128 is varint encoded
        self.write_varint_u128(value)
    }

    fn emit_isize(&mut self, value: isize) -> Result<(), Self::Error> {
        // isize is varint encoded with zigzag
        self.write_varint_isize(value)
    }

    fn emit_usize(&mut self, value: usize) -> Result<(), Self::Error> {
        // usize is varint encoded
        self.write_varint_usize(value)
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

    fn emit_some<T: crate::ser::Serialize<Self, E>>(
        &mut self,
        value: &T,
        extension: &E,
    ) -> Result<(), Self::Error> {
        // Write 1 to indicate Some, then serialize the value
        self.write_bytes(&[1])?;
        value.serialize(self, extension)
    }

    fn emit_seq(
        &mut self,
        len: usize,
        extension: &E,
        f: impl FnOnce(Self::Seq<'_>, &E) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        self.write_varint(len as u64)?;
        let seq = BinarySeq { serializer: self };
        f(seq, extension)
    }

    fn emit_tuple(
        &mut self,
        _len: usize,
        extension: &E,
        f: impl FnOnce(Self::Tuple<'_>, &E) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        // Tuples have fixed size, so no need to write length
        let tuple = BinaryTuple { serializer: self };
        f(tuple, extension)
    }

    fn emit_tuple_struct(
        &mut self,
        _name: &'static str,
        _len: usize,
        extension: &E,
        f: impl FnOnce(Self::TupleStruct<'_>, &E) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        // Tuple structs have fixed size, so no need to write length or name
        let tuple_struct = BinaryTupleStruct { serializer: self };
        f(tuple_struct, extension)
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
        _variant: &'static str,
        index: u32,
    ) -> Result<(), Self::Error> {
        // Write the variant index (binary format doesn't need variant name)
        self.write_varint(index as u64)
    }

    fn emit_map(
        &mut self,
        len: usize,
        extension: &E,
        f: impl FnOnce(Self::Map<'_>, &E) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        self.write_varint(len as u64)?;
        let map = BinaryMap { serializer: self };
        f(map, extension)
    }

    fn emit_struct(
        &mut self,
        _name: &'static str,
        _len: usize,
        extension: &E,
        f: impl FnOnce(Self::Struct<'_>, &E) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        // Structs have fixed fields, so no need to write length or name
        let struct_ser = BinaryStruct { serializer: self };
        f(struct_ser, extension)
    }

    fn emit_tuple_variant(
        &mut self,
        _name: &'static str,
        _variant: &'static str,
        index: u32,
        _len: usize,
        extension: &E,
        f: impl FnOnce(Self::TupleVariant<'_>, &E) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        self.write_varint(index as u64)?;
        let tuple_variant = BinaryTupleVariant { serializer: self };
        f(tuple_variant, extension)
    }

    fn emit_struct_variant(
        &mut self,
        _name: &'static str,
        _variant: &'static str,
        index: u32,
        _len: usize,
        extension: &E,
        f: impl FnOnce(Self::StructVariant<'_>, &E) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        self.write_varint(index as u64)?;
        let struct_variant = BinaryStructVariant { serializer: self };

        f(struct_variant, extension)
    }
}

impl<W: Write + 'static> BinarySerializer<W> {
    /// Create a new binary serializer that writes to the given writer with a
    /// unit extension.
    pub fn new(writer: W) -> Self { BinarySerializer { writer } }
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
