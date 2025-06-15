//! Binary deserializer implementation.
//!
//! This module provides a binary deserializer that reads data from any
//! type implementing `std::io::Read`. The format matches the binary serializer
//! and is designed to be simple, compact, and efficient.

use std::io::{self, Read};

use crate::de::{
    Deserialize, Deserializer, EnumAccess, FieldAccess, Identifier, MapAccess,
    SeqAccess, StructAccess, StructVariantAccess, TupleAccess,
    TupleStructAccess, TupleVariantAccess, ValueAccess,
};

/// A binary deserializer that reads from any `Read` implementation.
///
/// This deserializer reads data in the same format as the binary serializer:
/// - Integers in little-endian format
/// - Strings with varint length prefix
/// - Sequences and maps with varint length prefix
/// - Enums with varint variant index
/// - Booleans as single bytes (0 or 1)
/// - Floating point numbers in IEEE 754 format
pub struct BinaryDeserializer<R> {
    reader: R,
    buffer: Vec<u8>,
}

impl<R: Read + 'static> BinaryDeserializer<R> {
    /// Create a new binary deserializer that reads from the given reader with
    /// the specified extension.
    pub fn with_extension(reader: R) -> Self {
        Self { reader, buffer: Vec::new() }
    }

    /// Consume the deserializer and return the underlying reader.
    pub fn into_inner(self) -> R { self.reader }

    /// Get a reference to the underlying reader.
    pub fn reader(&self) -> &R { &self.reader }

    /// Get a mutable reference to the underlying reader.
    pub fn reader_mut(&mut self) -> &mut R { &mut self.reader }

    /// Read a varint (variable-length integer) from the reader.
    fn read_varint(&mut self) -> Result<u64, io::Error> {
        let mut result = 0u64;
        let mut shift = 0;

        loop {
            let mut byte = [0u8; 1];
            self.reader.read_exact(&mut byte)?;
            let byte = byte[0];

            result |= ((byte & 0x7F) as u64) << shift;

            if byte & 0x80 == 0 {
                break;
            }

            shift += 7;
            if shift >= 64 {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "Varint too long",
                ));
            }
        }

        Ok(result)
    }

    /// Read exact number of bytes from the reader.
    fn read_bytes(&mut self, buf: &mut [u8]) -> Result<(), io::Error> {
        self.reader.read_exact(buf)
    }
}

impl<R: Read + 'static> BinaryDeserializer<R> {
    /// Create a new binary deserializer that reads from the given reader with a
    /// unit extension.
    pub fn new(reader: R) -> Self { Self { reader, buffer: Vec::new() } }
}

// Helper structs for compound deserialization
pub struct BinarySeqAccess<'a, R> {
    deserializer: &'a mut BinaryDeserializer<R>,
    remaining: usize,
}

impl<R: Read + 'static, E> SeqAccess<E> for BinarySeqAccess<'_, R> {
    type Parent = BinaryDeserializer<R>;

    fn next_element<T: Deserialize<Self::Parent, E>>(
        &mut self,
        extension: &mut E,
    ) -> Result<Option<T>, <Self::Parent as Deserializer<E>>::Error> {
        if self.remaining == 0 {
            Ok(None)
        } else {
            self.remaining -= 1;
            T::deserialize(self.deserializer, extension).map(Some)
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }
}

pub struct BinaryTupleAccess<'a, R> {
    deserializer: &'a mut BinaryDeserializer<R>,
}

impl<R: Read + 'static, E> TupleAccess<E> for BinaryTupleAccess<'_, R> {
    type Parent = BinaryDeserializer<R>;

    fn next_element<T: Deserialize<Self::Parent, E>>(
        &mut self,
        extension: &mut E,
    ) -> Result<T, <Self::Parent as Deserializer<E>>::Error> {
        T::deserialize(self.deserializer, extension)
    }
}

pub struct BinaryTupleStructAccess<'a, R> {
    deserializer: &'a mut BinaryDeserializer<R>,
}

impl<R: Read + 'static, E> TupleStructAccess<E>
    for BinaryTupleStructAccess<'_, R>
{
    type Parent = BinaryDeserializer<R>;

    fn next_field<T: Deserialize<Self::Parent, E>>(
        &mut self,
        extension: &mut E,
    ) -> Result<T, <Self::Parent as Deserializer<E>>::Error> {
        T::deserialize(self.deserializer, extension)
    }
}

pub struct BinaryFieldAccess<'a, R> {
    deserializer: &'a mut BinaryDeserializer<R>,
}

impl<R: Read + 'static, E> FieldAccess<E> for BinaryFieldAccess<'_, R> {
    type Parent = BinaryDeserializer<R>;

    fn deserialize<T: Deserialize<Self::Parent, E>>(
        self,
        extension: &mut E,
    ) -> Result<T, <Self::Parent as Deserializer<E>>::Error> {
        T::deserialize(self.deserializer, extension)
    }
}

pub struct BinaryStructAccess<'a, R> {
    deserializer: &'a mut BinaryDeserializer<R>,
    fields_remaining: usize,
    total_fields: usize,
}

impl<R: Read + 'static, E> StructAccess<E> for BinaryStructAccess<'_, R> {
    type Parent = BinaryDeserializer<R>;
    type FieldAccess<'s> = BinaryFieldAccess<'s, R>;

    fn next_field<'e, Ret>(
        &mut self,
        extension: &'e mut E,
        next: impl FnOnce(
            Option<(Identifier, Self::FieldAccess<'_>, &'e mut E)>,
        )
            -> Result<Ret, <Self::Parent as Deserializer<E>>::Error>,
    ) -> Result<Ret, <Self::Parent as Deserializer<E>>::Error> {
        if self.fields_remaining == 0 {
            next(None)
        } else {
            let field_access =
                BinaryFieldAccess { deserializer: self.deserializer };
            // In binary format, we use indices for field identification
            // Calculate the current field index based on how many fields we've
            // processed
            let index = self.total_fields - self.fields_remaining;
            self.fields_remaining -= 1;
            next(Some((
                Identifier::from_index(index as u32),
                field_access,
                extension,
            )))
        }
    }
}

pub struct BinaryValueAccess<'a, R> {
    deserializer: &'a mut BinaryDeserializer<R>,
}

impl<R: Read + 'static, E> ValueAccess<E> for BinaryValueAccess<'_, R> {
    type Parent = BinaryDeserializer<R>;

    fn deserialize<V: Deserialize<Self::Parent, E>>(
        self,
        extension: &mut E,
    ) -> Result<V, <Self::Parent as Deserializer<E>>::Error> {
        V::deserialize(self.deserializer, extension)
    }
}

pub struct BinaryMapAccess<'a, R> {
    deserializer: &'a mut BinaryDeserializer<R>,
    remaining: usize,
}

impl<R: Read + 'static, E> MapAccess<E> for BinaryMapAccess<'_, R> {
    type Parent = BinaryDeserializer<R>;
    type ValueAccess<'s> = BinaryValueAccess<'s, R>;

    fn next_entry<'e, K: Deserialize<Self::Parent, E>, Ret>(
        &mut self,
        extension: &'e mut E,
        next: impl FnOnce(
            Option<(K, Self::ValueAccess<'_>, &'e mut E)>,
        )
            -> Result<Ret, <Self::Parent as Deserializer<E>>::Error>,
    ) -> Result<Ret, <Self::Parent as Deserializer<E>>::Error> {
        if self.remaining == 0 {
            next(None)
        } else {
            self.remaining -= 1;
            let key = K::deserialize(self.deserializer, extension)?;
            let value_access =
                BinaryValueAccess { deserializer: self.deserializer };
            next(Some((key, value_access, extension)))
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }
}

pub struct BinaryTupleVariantAccess<'a, R> {
    deserializer: &'a mut BinaryDeserializer<R>,
}

impl<R: Read + 'static, E> TupleVariantAccess<E>
    for BinaryTupleVariantAccess<'_, R>
{
    type Parent = BinaryDeserializer<R>;

    fn next_field<T: Deserialize<Self::Parent, E>>(
        &mut self,
        extension: &mut E,
    ) -> Result<T, <Self::Parent as Deserializer<E>>::Error> {
        T::deserialize(self.deserializer, extension)
    }
}

pub struct BinaryStructVariantAccess<'a, R> {
    deserializer: &'a mut BinaryDeserializer<R>,
    remaining: usize,
    total_fields: usize,
}

impl<R: Read + 'static, E> StructVariantAccess<E>
    for BinaryStructVariantAccess<'_, R>
{
    type Parent = BinaryDeserializer<R>;
    type FieldAccess<'s> = BinaryFieldAccess<'s, R>;

    fn next_field<'e, Ret>(
        &mut self,
        extension: &'e mut E,
        next: impl FnOnce(
            Option<(Identifier, Self::FieldAccess<'_>, &'e mut E)>,
        )
            -> Result<Ret, <Self::Parent as Deserializer<E>>::Error>,
    ) -> Result<Ret, <Self::Parent as Deserializer<E>>::Error> {
        if self.remaining == 0 {
            next(None)
        } else {
            self.remaining -= 1;
            let field_access =
                BinaryFieldAccess { deserializer: self.deserializer };
            // In binary format, we use indices for field identification
            // Calculate the correct index: total_fields - remaining - 1
            let index = self.total_fields - self.remaining - 1;
            next(Some((
                Identifier::from_index(index as u32),
                field_access,
                extension,
            )))
        }
    }
}

pub struct BinaryEnumAccess<'a, R> {
    deserializer: &'a mut BinaryDeserializer<R>,
}

impl<'s, R: Read + 'static, E> EnumAccess<E> for BinaryEnumAccess<'s, R> {
    type Parent = BinaryDeserializer<R>;
    type TupleVariantAccess = BinaryTupleVariantAccess<'s, R>;
    type StructVariantAccess = BinaryStructVariantAccess<'s, R>;

    fn unit_variant(
        self,
    ) -> Result<(), <Self::Parent as Deserializer<E>>::Error> {
        Ok(())
    }

    fn tuple_variant<'e, Ret>(
        self,
        _len: usize,
        extension: &'e mut E,
        f: impl FnOnce(
            Self::TupleVariantAccess,
            &'e mut E,
        )
            -> Result<Ret, <Self::Parent as Deserializer<E>>::Error>,
    ) -> Result<Ret, <Self::Parent as Deserializer<E>>::Error> {
        let tuple_access =
            BinaryTupleVariantAccess { deserializer: self.deserializer };

        f(tuple_access, extension)
    }

    fn struct_variant<'e, Ret>(
        self,
        fields: &'static [&'static str],
        extension: &'e mut E,
        f: impl FnOnce(
            Self::StructVariantAccess,
            &'e mut E,
        )
            -> Result<Ret, <Self::Parent as Deserializer<E>>::Error>,
    ) -> Result<Ret, <Self::Parent as Deserializer<E>>::Error> {
        let struct_access = BinaryStructVariantAccess {
            deserializer: self.deserializer,
            remaining: fields.len(),
            total_fields: fields.len(),
        };

        f(struct_access, extension)
    }
}

impl<R: Read + 'static, E> Deserializer<E> for BinaryDeserializer<R> {
    type Error = io::Error;

    type SeqAccess<'s> = BinarySeqAccess<'s, R>;
    type TupleAccess<'s> = BinaryTupleAccess<'s, R>;
    type TupleStructAccess<'s> = BinaryTupleStructAccess<'s, R>;
    type StructAccess<'s> = BinaryStructAccess<'s, R>;
    type MapAccess<'s> = BinaryMapAccess<'s, R>;
    type TupleVariantAccess<'s> = BinaryTupleVariantAccess<'s, R>;
    type StructVariantAccess<'s> = BinaryStructVariantAccess<'s, R>;
    type EnumAccess<'s> = BinaryEnumAccess<'s, R>;

    fn expect_i8(&mut self) -> Result<i8, Self::Error> {
        let mut buf = [0u8; 1];
        self.read_bytes(&mut buf)?;
        Ok(i8::from_le_bytes(buf))
    }

    fn expect_i16(&mut self) -> Result<i16, Self::Error> {
        let mut buf = [0u8; 2];
        self.read_bytes(&mut buf)?;
        Ok(i16::from_le_bytes(buf))
    }

    fn expect_i32(&mut self) -> Result<i32, Self::Error> {
        let mut buf = [0u8; 4];
        self.read_bytes(&mut buf)?;
        Ok(i32::from_le_bytes(buf))
    }

    fn expect_i64(&mut self) -> Result<i64, Self::Error> {
        let mut buf = [0u8; 8];
        self.read_bytes(&mut buf)?;
        Ok(i64::from_le_bytes(buf))
    }

    fn expect_u8(&mut self) -> Result<u8, Self::Error> {
        let mut buf = [0u8; 1];
        self.read_bytes(&mut buf)?;
        Ok(buf[0])
    }

    fn expect_u16(&mut self) -> Result<u16, Self::Error> {
        let mut buf = [0u8; 2];
        self.read_bytes(&mut buf)?;
        Ok(u16::from_le_bytes(buf))
    }

    fn expect_u32(&mut self) -> Result<u32, Self::Error> {
        let mut buf = [0u8; 4];
        self.read_bytes(&mut buf)?;
        Ok(u32::from_le_bytes(buf))
    }

    fn expect_u64(&mut self) -> Result<u64, Self::Error> {
        let mut buf = [0u8; 8];
        self.read_bytes(&mut buf)?;
        Ok(u64::from_le_bytes(buf))
    }

    fn expect_isize(&mut self) -> Result<isize, Self::Error> {
        #[cfg(target_pointer_width = "64")]
        let mut buf = [0u8; 8];

        #[cfg(target_pointer_width = "32")]
        let mut buf = [0u8; 4];

        self.read_bytes(&mut buf)?;

        Ok(isize::from_le_bytes(buf))
    }

    fn expect_usize(&mut self) -> Result<usize, Self::Error> {
        #[cfg(target_pointer_width = "64")]
        let mut buf = [0u8; 8];

        #[cfg(target_pointer_width = "32")]
        let mut buf = [0u8; 4];

        self.read_bytes(&mut buf)?;

        Ok(usize::from_le_bytes(buf))
    }

    fn expect_f32(&mut self) -> Result<f32, Self::Error> {
        let mut buf = [0u8; 4];
        self.read_bytes(&mut buf)?;
        Ok(f32::from_le_bytes(buf))
    }

    fn expect_f64(&mut self) -> Result<f64, Self::Error> {
        let mut buf = [0u8; 8];
        self.read_bytes(&mut buf)?;
        Ok(f64::from_le_bytes(buf))
    }

    fn expect_bool(&mut self) -> Result<bool, Self::Error> {
        let byte = Deserializer::<E>::expect_u8(self)?;

        match byte {
            0 => Ok(false),
            1 => Ok(true),
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("Invalid boolean value: {}", byte),
            )),
        }
    }

    fn expect_char(&mut self) -> Result<char, Self::Error> {
        // Read UTF-8 bytes with varint length prefix (matching serializer
        // format)
        let len = self.read_varint()? as usize;
        let mut buf = vec![0u8; len];
        self.read_bytes(&mut buf)?;
        let s = std::str::from_utf8(&buf).map_err(|e| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("Invalid UTF-8: {}", e),
            )
        })?;
        let mut chars = s.chars();
        match (chars.next(), chars.next()) {
            (Some(c), None) => Ok(c),
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Invalid char: string must contain exactly one character",
            )),
        }
    }

    fn expect_string(&mut self) -> Result<String, Self::Error> {
        let len = self.read_varint()? as usize;
        let mut buf = vec![0u8; len];
        self.read_bytes(&mut buf)?;
        String::from_utf8(buf).map_err(|e| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("Invalid UTF-8: {}", e),
            )
        })
    }

    fn expect_bytes(&mut self) -> Result<Vec<u8>, Self::Error> {
        let len = self.read_varint()? as usize;
        let mut buf = vec![0u8; len];
        self.read_bytes(&mut buf)?;
        Ok(buf)
    }

    fn expect_unit(&mut self) -> Result<(), Self::Error> { Ok(()) }

    fn expect_option<T: Deserialize<Self, E>>(
        &mut self,
        extension: &mut E,
    ) -> Result<Option<T>, Self::Error> {
        let variant = self.read_varint()?;
        match variant {
            0 => Ok(None),
            1 => Ok(Some(T::deserialize(self, extension)?)),
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("Invalid option variant: {}", variant),
            )),
        }
    }

    fn expect_seq<'e, Ret>(
        &mut self,
        extension: &'e mut E,
        f: impl FnOnce(Self::SeqAccess<'_>, &'e mut E) -> Result<Ret, Self::Error>,
    ) -> Result<Ret, Self::Error> {
        let len = self.read_varint()? as usize;
        let seq_access = BinarySeqAccess { deserializer: self, remaining: len };
        f(seq_access, extension)
    }

    fn expect_tuple<'e, Ret>(
        &mut self,
        _len: usize,
        extension: &'e mut E,
        f: impl FnOnce(Self::TupleAccess<'_>, &'e mut E) -> Result<Ret, Self::Error>,
    ) -> Result<Ret, Self::Error> {
        let tuple_access = BinaryTupleAccess { deserializer: self };
        f(tuple_access, extension)
    }

    fn expect_tuple_struct<'e, Ret>(
        &mut self,
        _name: &'static str,
        _len: usize,
        extension: &'e mut E,
        f: impl FnOnce(
            Self::TupleStructAccess<'_>,
            &'e mut E,
        ) -> Result<Ret, Self::Error>,
    ) -> Result<Ret, Self::Error> {
        let tuple_struct_access =
            BinaryTupleStructAccess { deserializer: self };
        f(tuple_struct_access, extension)
    }

    fn expect_unit_struct(
        &mut self,
        _name: &'static str,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn expect_struct<'e, Ret>(
        &mut self,
        _name: &'static str,
        fields: &'static [&'static str],
        extension: &'e mut E,
        f: impl FnOnce(
            Self::StructAccess<'_>,
            &'e mut E,
        ) -> Result<Ret, Self::Error>,
    ) -> Result<Ret, Self::Error> {
        let struct_access = BinaryStructAccess {
            deserializer: self,
            fields_remaining: fields.len(),
            total_fields: fields.len(),
        };
        f(struct_access, extension)
    }

    fn expect_map<'e, Ret>(
        &mut self,
        extension: &'e mut E,
        f: impl FnOnce(Self::MapAccess<'_>, &'e mut E) -> Result<Ret, Self::Error>,
    ) -> Result<Ret, Self::Error> {
        let len = self.read_varint()? as usize;
        let map_access = BinaryMapAccess { deserializer: self, remaining: len };
        f(map_access, extension)
    }

    fn expect_enum<'e, Ret>(
        &mut self,
        _name: &'static str,
        _variants: &'static [&'static str],
        extension: &'e mut E,
        f: impl FnOnce(
            Identifier,
            Self::EnumAccess<'_>,
            &'e mut E,
        ) -> Result<Ret, Self::Error>,
    ) -> Result<Ret, Self::Error> {
        let variant_index = self.read_varint()?;
        let enum_access = BinaryEnumAccess { deserializer: self };

        f(Identifier::from_index(variant_index as u32), enum_access, extension)
    }

    fn expect_str(&mut self) -> Result<&str, Self::Error> {
        let len = self.read_varint()? as usize;

        if len > self.buffer.len() {
            self.buffer.resize(len, 0);
        }

        self.reader.read_exact(&mut self.buffer[..len])?;

        let s = std::str::from_utf8(&self.buffer[..len])
            .map_err(|x| io::Error::new(io::ErrorKind::InvalidData, x))?;

        Ok(s)
    }
}

impl crate::de::Error for io::Error {
    fn custom<T>(msg: T) -> Self
    where
        T: std::fmt::Display,
    {
        io::Error::new(io::ErrorKind::Other, msg.to_string())
    }
}

// #[cfg(test)]
// mod test;
