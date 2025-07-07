//! RON (Rusty Object Notation) serializer implementation.
//!
//! This module provides a human-readable serializer that formats data in a way
//! similar to Rust's Debug output or RON format. The output is designed to be
//! easily readable with proper indentation and formatting.

use std::io::{self, Write};

use crate::ser::{
    Map, Seq, Serialize, Serializer, Struct, StructVariant, Tuple, TupleStruct,
    TupleVariant,
};

/// Configuration options for the RON serializer.
#[derive(Debug, Clone)]
pub enum RonConfig {
    /// Compact mode: no newlines, indentation, spaces, or trailing commas
    Compact,
    /// Pretty mode: uses newlines, indentation, spaces, and trailing commas
    /// The string parameter specifies the indentation characters (e.g., "    "
    /// for 4 spaces)
    Pretty(String),
}

impl Default for RonConfig {
    fn default() -> Self { Self::Pretty("    ".to_string()) }
}

/// A RON (Rusty Object Notation) serializer that writes human-readable output.
///
/// The serializer produces output similar to Rust's Debug format or RON
/// notation, with proper indentation and formatting for readability.
pub struct RonSerializer<W> {
    writer: W,
    config: RonConfig,
    current_indent: usize,
}

impl<W: Write> RonSerializer<W> {
    /// Create a new RON serializer with default configuration (pretty printing
    /// with 4-space indentation).
    pub fn new(writer: W) -> Self {
        Self::with_config(writer, RonConfig::default())
    }

    /// Create a new RON serializer with custom configuration.
    pub fn with_config(writer: W, config: RonConfig) -> Self {
        Self { writer, config, current_indent: 0 }
    }

    /// Create a new compact RON serializer (no pretty printing).
    pub fn compact(writer: W) -> Self {
        Self { writer, config: RonConfig::Compact, current_indent: 0 }
    }

    /// Consume the serializer and return the underlying writer.
    pub fn into_inner(self) -> W { self.writer }

    /// Get a reference to the underlying writer.
    pub fn writer(&self) -> &W { &self.writer }

    /// Get a mutable reference to the underlying writer.
    pub fn writer_mut(&mut self) -> &mut W { &mut self.writer }

    /// Write the current indentation to the writer.
    fn write_indent(&mut self) -> Result<(), io::Error> {
        if let RonConfig::Pretty(ref indent_str) = self.config {
            let indent = indent_str.repeat(self.current_indent);
            self.writer.write_all(indent.as_bytes())?;
        }
        Ok(())
    }

    /// Write a newline if in pretty mode.
    fn write_newline(&mut self) -> Result<(), io::Error> {
        if matches!(self.config, RonConfig::Pretty(_)) {
            self.writer.write_all(b"\n")?;
        }
        Ok(())
    }

    /// Write a space if in pretty mode.
    fn write_space(&mut self) -> Result<(), io::Error> {
        if matches!(self.config, RonConfig::Pretty(_)) {
            self.writer.write_all(b" ")?;
        }
        Ok(())
    }

    /// Increase indentation level.
    fn indent(&mut self) { self.current_indent += 1; }

    /// Decrease indentation level.
    fn dedent(&mut self) {
        if self.current_indent > 0 {
            self.current_indent -= 1;
        }
    }

    /// Write a string, escaping special characters.
    fn write_escaped_string(&mut self, s: &str) -> Result<(), io::Error> {
        self.writer.write_all(b"\"")?;
        for ch in s.chars() {
            match ch {
                '"' => self.writer.write_all(b"\\\"")?,
                '\\' => self.writer.write_all(b"\\\\")?,
                '\n' => self.writer.write_all(b"\\n")?,
                '\r' => self.writer.write_all(b"\\r")?,
                '\t' => self.writer.write_all(b"\\t")?,
                c if c.is_control() => {
                    write!(self.writer, "\\u{{{:04x}}}", c as u32)?;
                }
                c => {
                    let mut buf = [0; 4];
                    let str_slice = c.encode_utf8(&mut buf);
                    self.writer.write_all(str_slice.as_bytes())?;
                }
            }
        }
        self.writer.write_all(b"\"")?;
        Ok(())
    }

    /// Write a character, escaping if necessary.
    fn write_escaped_char(&mut self, c: char) -> Result<(), io::Error> {
        self.writer.write_all(b"'")?;
        match c {
            '\'' => self.writer.write_all(b"\\'")?,
            '\\' => self.writer.write_all(b"\\\\")?,
            '\n' => self.writer.write_all(b"\\n")?,
            '\r' => self.writer.write_all(b"\\r")?,
            '\t' => self.writer.write_all(b"\\t")?,
            c if c.is_control() => {
                write!(self.writer, "\\u{{{:04x}}}", c as u32)?;
            }
            c => {
                let mut buf = [0; 4];
                let str_slice = c.encode_utf8(&mut buf);
                self.writer.write_all(str_slice.as_bytes())?;
            }
        }
        self.writer.write_all(b"'")?;
        Ok(())
    }
}

impl<W: Write + 'static, E> Serializer<E> for RonSerializer<W> {
    type Error = io::Error;
    type Seq<'s> = RonSeq<'s, W>;
    type Tuple<'s> = RonTuple<'s, W>;
    type TupleStruct<'s> = RonTupleStruct<'s, W>;
    type Struct<'s> = RonStruct<'s, W>;
    type Map<'s> = RonMap<'s, W>;
    type TupleVariant<'s> = RonTupleVariant<'s, W>;
    type StructVariant<'s> = RonStructVariant<'s, W>;

    fn emit_i8(&mut self, value: i8) -> Result<(), Self::Error> {
        write!(self.writer, "{value}")
    }

    fn emit_i16(&mut self, value: i16) -> Result<(), Self::Error> {
        write!(self.writer, "{value}")
    }

    fn emit_i32(&mut self, value: i32) -> Result<(), Self::Error> {
        write!(self.writer, "{value}")
    }

    fn emit_i64(&mut self, value: i64) -> Result<(), Self::Error> {
        write!(self.writer, "{value}")
    }

    fn emit_u8(&mut self, value: u8) -> Result<(), Self::Error> {
        write!(self.writer, "{value}")
    }

    fn emit_u16(&mut self, value: u16) -> Result<(), Self::Error> {
        write!(self.writer, "{value}")
    }

    fn emit_u32(&mut self, value: u32) -> Result<(), Self::Error> {
        write!(self.writer, "{value}")
    }

    fn emit_u64(&mut self, value: u64) -> Result<(), Self::Error> {
        write!(self.writer, "{value}")
    }

    fn emit_i128(&mut self, value: i128) -> Result<(), Self::Error> {
        write!(self.writer, "{value}")
    }

    fn emit_u128(&mut self, value: u128) -> Result<(), Self::Error> {
        write!(self.writer, "{value}")
    }

    fn emit_isize(&mut self, value: isize) -> Result<(), Self::Error> {
        write!(self.writer, "{value}")
    }

    fn emit_usize(&mut self, value: usize) -> Result<(), Self::Error> {
        write!(self.writer, "{value}")
    }

    fn emit_f32(&mut self, value: f32) -> Result<(), Self::Error> {
        if value.is_nan() {
            write!(self.writer, "NaN")
        } else if value.is_infinite() {
            if value.is_sign_positive() {
                write!(self.writer, "inf")
            } else {
                write!(self.writer, "-inf")
            }
        } else {
            write!(self.writer, "{value}")
        }
    }

    fn emit_f64(&mut self, value: f64) -> Result<(), Self::Error> {
        if value.is_nan() {
            write!(self.writer, "NaN")
        } else if value.is_infinite() {
            if value.is_sign_positive() {
                write!(self.writer, "inf")
            } else {
                write!(self.writer, "-inf")
            }
        } else {
            write!(self.writer, "{value}")
        }
    }

    fn emit_bool(&mut self, value: bool) -> Result<(), Self::Error> {
        write!(self.writer, "{value}")
    }

    fn emit_char(&mut self, value: char) -> Result<(), Self::Error> {
        self.write_escaped_char(value)
    }

    fn emit_str(&mut self, value: &str) -> Result<(), Self::Error> {
        self.write_escaped_string(value)
    }

    fn emit_bytes(&mut self, value: &[u8]) -> Result<(), Self::Error> {
        self.writer.write_all(b"b\"")?;
        for &byte in value {
            match byte {
                b'"' => self.writer.write_all(b"\\\"")?,
                b'\\' => self.writer.write_all(b"\\\\")?,
                b'\n' => self.writer.write_all(b"\\n")?,
                b'\r' => self.writer.write_all(b"\\r")?,
                b'\t' => self.writer.write_all(b"\\t")?,
                b if b.is_ascii_graphic() || b == b' ' => {
                    self.writer.write_all(&[b])?;
                }
                b => {
                    write!(self.writer, "\\x{b:02x}")?;
                }
            }
        }
        self.writer.write_all(b"\"")?;
        Ok(())
    }

    fn emit_unit(&mut self) -> Result<(), Self::Error> {
        self.writer.write_all(b"()")
    }

    fn emit_none(&mut self) -> Result<(), Self::Error> {
        self.writer.write_all(b"None")
    }

    fn emit_some<T: Serialize<Self, E>>(
        &mut self,
        value: &T,
        extension: &E,
    ) -> Result<(), Self::Error> {
        self.writer.write_all(b"Some(")?;
        value.serialize(self, extension)?;
        self.writer.write_all(b")")?;
        Ok(())
    }

    fn emit_seq(
        &mut self,
        len: usize,
        extension: &E,
        f: impl FnOnce(Self::Seq<'_>, &E) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        if len == 0 {
            self.writer.write_all(b"[]")?;
            return Ok(());
        }

        self.writer.write_all(b"[")?;

        let seq = RonSeq::new(self, len);
        f(seq, extension)?;

        self.writer.write_all(b"]")?;
        Ok(())
    }

    fn emit_tuple(
        &mut self,
        len: usize,
        extension: &E,
        f: impl FnOnce(Self::Tuple<'_>, &E) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        if len == 0 {
            self.writer.write_all(b"()")?;
            return Ok(());
        }

        self.writer.write_all(b"(")?;

        let tuple = RonTuple::new(self, len);
        f(tuple, extension)?;

        self.writer.write_all(b")")?;
        Ok(())
    }

    fn emit_tuple_struct(
        &mut self,
        name: &'static str,
        len: usize,
        extension: &E,
        f: impl FnOnce(Self::TupleStruct<'_>, &E) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        write!(self.writer, "{name}(")?;
        let tuple_struct = RonTupleStruct::new(self, len);
        f(tuple_struct, extension)?;
        self.writer.write_all(b")")?;
        Ok(())
    }

    fn emit_unit_struct(
        &mut self,
        name: &'static str,
    ) -> Result<(), Self::Error> {
        write!(self.writer, "{name}")
    }

    fn emit_unit_variant(
        &mut self,
        _name: &'static str,
        variant: &'static str,
        _index: u32,
    ) -> Result<(), Self::Error> {
        // Now we can use the actual variant name for human-readable output
        write!(self.writer, "{variant}")
    }

    fn emit_map(
        &mut self,
        len: usize,
        extension: &E,
        f: impl FnOnce(Self::Map<'_>, &E) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        if len == 0 {
            self.writer.write_all(b"{}")?;
            return Ok(());
        }

        self.writer.write_all(b"{")?;

        let map = RonMap::new(self, len);
        f(map, extension)?;

        self.writer.write_all(b"}")?;
        Ok(())
    }

    fn emit_struct(
        &mut self,
        name: &'static str,
        len: usize,
        extension: &E,
        f: impl FnOnce(Self::Struct<'_>, &E) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        if len == 0 {
            write!(self.writer, "{name} {{}}")?;
            return Ok(());
        }

        write!(self.writer, "{name}")?;
        self.write_space()?;
        self.writer.write_all(b"{")?;

        let struct_ser = RonStruct::new(self, len);
        f(struct_ser, extension)?;

        self.writer.write_all(b"}")?;
        Ok(())
    }

    fn emit_tuple_variant(
        &mut self,
        _name: &'static str,
        variant: &'static str,
        _index: u32,
        len: usize,
        extension: &E,
        f: impl FnOnce(Self::TupleVariant<'_>, &E) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        write!(self.writer, "{variant}(")?;
        let tuple_variant = RonTupleVariant::new(self, len);
        f(tuple_variant, extension)?;
        self.writer.write_all(b")")?;
        Ok(())
    }

    fn emit_struct_variant(
        &mut self,
        _name: &'static str,
        variant: &'static str,
        _index: u32,
        len: usize,
        extension: &E,
        f: impl FnOnce(Self::StructVariant<'_>, &E) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        if len == 0 {
            write!(self.writer, "{variant} {{}}")?;
            return Ok(());
        }

        write!(self.writer, "{variant}")?;
        self.write_space()?;
        self.writer.write_all(b"{")?;

        let struct_variant = RonStructVariant::new(self, len);
        f(struct_variant, extension)?;

        self.writer.write_all(b"}")?;
        Ok(())
    }
}

/// Utility function to serialize a value to RON format with default
/// configuration.
///
/// # Examples
///
/// ```
/// use pernixc_serialize::{ron::ser::to_ron_string, ser::Serialize};
///
/// let value = 42;
/// let result = to_ron_string(&value);
/// assert!(result.is_ok());
/// assert_eq!(result.unwrap(), "42");
/// ```
pub fn to_ron_string<T>(value: &T) -> Result<String, io::Error>
where
    T: Serialize<RonSerializer<Vec<u8>>, ()>,
{
    let buffer = Vec::new();
    let mut serializer = RonSerializer::new(buffer);
    value.serialize(&mut serializer, &())?;
    let buffer = serializer.into_inner();
    String::from_utf8(buffer)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
}

/// Utility function to serialize a value to RON format with custom
/// configuration.
pub fn to_ron_string_with_config<T>(
    value: &T,
    config: RonConfig,
) -> Result<String, io::Error>
where
    T: Serialize<RonSerializer<Vec<u8>>, ()>,
{
    let buffer = Vec::new();
    let mut serializer = RonSerializer::with_config(buffer, config);
    value.serialize(&mut serializer, &())?;
    let buffer = serializer.into_inner();
    String::from_utf8(buffer)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
}

/// Utility function to serialize a value to compact RON format (no pretty
/// printing).
pub fn to_ron_string_compact<T>(value: &T) -> Result<String, io::Error>
where
    T: Serialize<RonSerializer<Vec<u8>>, ()>,
{
    let buffer = Vec::new();
    let mut serializer = RonSerializer::compact(buffer);
    value.serialize(&mut serializer, &())?;
    let buffer = serializer.into_inner();
    String::from_utf8(buffer)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
}

// =============================================================================
// Common formatting logic for sequence-like types (arrays, tuples)
// =============================================================================

/// Common logic for formatting elements in sequences or maps
/// Handles both compact and pretty mode formatting with proper indentation  
fn serialize_element_with_formatting<W: Write + 'static>(
    serializer: &mut RonSerializer<W>,
    count: &mut usize,
    len: usize,
    is_pretty: bool,
    serialize_content: impl FnOnce(&mut RonSerializer<W>) -> Result<(), io::Error>,
) -> Result<(), io::Error> {
    // Add separator for compact mode (no space after comma)
    if !is_pretty && *count > 0 {
        serializer.writer.write_all(b",")?;
    }

    // For pretty mode, handle newlines and indentation
    if is_pretty {
        if *count == 0 {
            // First element: add newline and indent
            serializer.write_newline()?;
            serializer.indent();
            serializer.write_indent()?;
        } else {
            // Subsequent elements: add comma, newline, and indent
            serializer.writer.write_all(b",")?;
            serializer.write_newline()?;
            serializer.write_indent()?;
        }
    }

    // Execute the custom serialization logic
    serialize_content(serializer)?;

    *count += 1;

    // For pretty mode, handle the closing
    if is_pretty && *count == len {
        // Add trailing comma and dedent
        serializer.writer.write_all(b",")?;
        serializer.write_newline()?;
        serializer.dedent();
        serializer.write_indent()?;
    }

    Ok(())
}

/// Helper function for serializing sequence elements using the common
/// formatting logic
fn serialize_sequence_element<
    W: Write + 'static,
    E,
    T: Serialize<RonSerializer<W>, E> + ?Sized,
>(
    serializer: &mut RonSerializer<W>,
    value: &T,
    extension: &E,
    count: &mut usize,
    len: usize,
    is_pretty: bool,
) -> Result<(), io::Error> {
    serialize_element_with_formatting(
        serializer,
        count,
        len,
        is_pretty,
        |ser| value.serialize(ser, extension),
    )
}

// =============================================================================
// Compound serializer implementations
// =============================================================================

pub struct RonSeq<'a, W> {
    serializer: &'a mut RonSerializer<W>,
    len: usize,
    count: usize,
    is_pretty: bool,
}

impl<'a, W: Write> RonSeq<'a, W> {
    fn new(serializer: &'a mut RonSerializer<W>, len: usize) -> Self {
        let is_pretty = matches!(serializer.config, RonConfig::Pretty(_));
        Self { serializer, len, count: 0, is_pretty }
    }
}

impl<W: Write + 'static, E> Seq<E> for RonSeq<'_, W> {
    type Parent = RonSerializer<W>;

    fn serialize_element<T: Serialize<Self::Parent, E> + ?Sized>(
        &mut self,
        value: &T,
        extension: &E,
    ) -> Result<(), io::Error> {
        serialize_sequence_element(
            self.serializer,
            value,
            extension,
            &mut self.count,
            self.len,
            self.is_pretty,
        )
    }
}

pub struct RonTuple<'a, W> {
    serializer: &'a mut RonSerializer<W>,
    len: usize,
    count: usize,
    is_pretty: bool,
}

impl<'a, W: Write> RonTuple<'a, W> {
    fn new(serializer: &'a mut RonSerializer<W>, len: usize) -> Self {
        let is_pretty = matches!(serializer.config, RonConfig::Pretty(_));
        Self { serializer, len, count: 0, is_pretty }
    }
}

impl<W: Write + 'static, E> Tuple<E> for RonTuple<'_, W> {
    type Parent = RonSerializer<W>;

    fn serialize_element<T: Serialize<Self::Parent, E> + ?Sized>(
        &mut self,
        value: &T,
        extension: &E,
    ) -> Result<(), io::Error> {
        serialize_sequence_element(
            self.serializer,
            value,
            extension,
            &mut self.count,
            self.len,
            self.is_pretty,
        )
    }
}

pub struct RonTupleStruct<'a, W> {
    serializer: &'a mut RonSerializer<W>,
    len: usize,
    count: usize,
    is_pretty: bool,
}

impl<'a, W: Write> RonTupleStruct<'a, W> {
    fn new(serializer: &'a mut RonSerializer<W>, len: usize) -> Self {
        let is_pretty = matches!(serializer.config, RonConfig::Pretty(_));
        Self { serializer, len, count: 0, is_pretty }
    }
}

impl<W: Write + 'static, E> TupleStruct<E> for RonTupleStruct<'_, W> {
    type Parent = RonSerializer<W>;

    fn serialize_field<T: Serialize<Self::Parent, E> + ?Sized>(
        &mut self,
        value: &T,
        extension: &E,
    ) -> Result<(), io::Error> {
        serialize_sequence_element(
            self.serializer,
            value,
            extension,
            &mut self.count,
            self.len,
            self.is_pretty,
        )
    }
}

pub struct RonStruct<'a, W> {
    serializer: &'a mut RonSerializer<W>,
    len: usize,
    count: usize,
    is_pretty: bool,
}

impl<'a, W: Write> RonStruct<'a, W> {
    fn new(serializer: &'a mut RonSerializer<W>, len: usize) -> Self {
        let is_pretty = matches!(serializer.config, RonConfig::Pretty(_));
        Self { serializer, len, count: 0, is_pretty }
    }
}

impl<W: Write + 'static, E> Struct<E> for RonStruct<'_, W> {
    type Parent = RonSerializer<W>;

    fn serialize_field<T: Serialize<Self::Parent, E> + ?Sized>(
        &mut self,
        name: &'static str,
        value: &T,
        extension: &E,
    ) -> Result<(), io::Error> {
        serialize_element_with_formatting(
            self.serializer,
            &mut self.count,
            self.len,
            self.is_pretty,
            |ser| {
                // Write field name (without quotes)
                write!(ser.writer, "{name}:")?;

                // Add space after colon only in pretty mode
                if self.is_pretty {
                    ser.writer.write_all(b" ")?;
                }

                // Serialize value
                value.serialize(ser, extension)
            },
        )
    }

    fn skip_field(
        &mut self,
        _name: &'static str,
        _extension: &E,
    ) -> Result<(), io::Error> {
        // For RON, we simply don't serialize skipped fields
        Ok(())
    }
}

pub struct RonMap<'a, W> {
    serializer: &'a mut RonSerializer<W>,
    len: usize,
    count: usize,
    is_pretty: bool,
}

impl<'a, W: Write> RonMap<'a, W> {
    fn new(serializer: &'a mut RonSerializer<W>, len: usize) -> Self {
        let is_pretty = matches!(serializer.config, RonConfig::Pretty(_));
        Self { serializer, len, count: 0, is_pretty }
    }
}

impl<W: Write + 'static, E> Map<E> for RonMap<'_, W> {
    type Parent = RonSerializer<W>;

    fn serialize_entry<
        K: Serialize<Self::Parent, E> + ?Sized,
        V: Serialize<Self::Parent, E> + ?Sized,
    >(
        &mut self,
        key: &K,
        value: &V,
        extension: &E,
    ) -> Result<(), io::Error> {
        serialize_element_with_formatting(
            self.serializer,
            &mut self.count,
            self.len,
            self.is_pretty,
            |ser| {
                // Serialize key
                key.serialize(ser, extension)?;

                // Add colon separator
                ser.writer.write_all(b":")?;

                // Add space after colon only in pretty mode
                if self.is_pretty {
                    ser.writer.write_all(b" ")?;
                }

                // Serialize value
                value.serialize(ser, extension)
            },
        )
    }
}

pub struct RonTupleVariant<'a, W> {
    serializer: &'a mut RonSerializer<W>,
    len: usize,
    count: usize,
    is_pretty: bool,
}

impl<'a, W: Write> RonTupleVariant<'a, W> {
    fn new(serializer: &'a mut RonSerializer<W>, len: usize) -> Self {
        let is_pretty = matches!(serializer.config, RonConfig::Pretty(_));
        Self { serializer, len, count: 0, is_pretty }
    }
}

impl<W: Write + 'static, E> TupleVariant<E> for RonTupleVariant<'_, W> {
    type Parent = RonSerializer<W>;

    fn serialize_field<T: Serialize<Self::Parent, E> + ?Sized>(
        &mut self,
        value: &T,
        extension: &E,
    ) -> Result<(), io::Error> {
        serialize_sequence_element(
            self.serializer,
            value,
            extension,
            &mut self.count,
            self.len,
            self.is_pretty,
        )
    }
}

pub struct RonStructVariant<'a, W> {
    serializer: &'a mut RonSerializer<W>,
    len: usize,
    count: usize,
    is_pretty: bool,
}

impl<'a, W: Write> RonStructVariant<'a, W> {
    fn new(serializer: &'a mut RonSerializer<W>, len: usize) -> Self {
        let is_pretty = matches!(serializer.config, RonConfig::Pretty(_));
        Self { serializer, len, count: 0, is_pretty }
    }
}

impl<W: Write + 'static, E> StructVariant<E> for RonStructVariant<'_, W> {
    type Parent = RonSerializer<W>;

    fn serialize_field<T: Serialize<Self::Parent, E> + ?Sized>(
        &mut self,
        name: &'static str,
        value: &T,
        extension: &E,
    ) -> Result<(), io::Error> {
        serialize_element_with_formatting(
            self.serializer,
            &mut self.count,
            self.len,
            self.is_pretty,
            |ser| {
                // Write field name (without quotes)
                write!(ser.writer, "{name}:")?;

                // Add space after colon only in pretty mode
                if self.is_pretty {
                    ser.writer.write_all(b" ")?;
                }

                // Serialize value
                value.serialize(ser, extension)
            },
        )
    }

    fn skip_field(
        &mut self,
        _name: &'static str,
        _extension: &E,
    ) -> Result<(), io::Error> {
        // For RON, we simply don't serialize skipped fields
        Ok(())
    }
}

#[cfg(test)]
mod test;
