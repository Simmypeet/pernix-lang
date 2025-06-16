//! RON (Rusty Object Notation) serializer implementation.
//!
//! This module provides a human-readable serializer that formats data in a way
//! similar to Rust's Debug output or RON format. The output is designed to be
//! easily readable with proper indentation and formatting.

use std::fmt::{self, Write};

use crate::ser::{
    Error, Map, Seq, Serialize, Serializer, Struct, StructVariant, Tuple,
    TupleStruct, TupleVariant,
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

/// Error type for RON serialization.
#[derive(Debug)]
pub struct RonError {
    message: String,
}

impl fmt::Display for RonError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "RON serialization error: {}", self.message)
    }
}

impl std::error::Error for RonError {}

impl Error for RonError {
    fn custom<T>(msg: T) -> Self
    where
        T: fmt::Display,
    {
        Self { message: msg.to_string() }
    }
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
    fn write_indent(&mut self) -> Result<(), RonError> {
        if let RonConfig::Pretty(ref indent_str) = self.config {
            let indent = indent_str.repeat(self.current_indent);
            self.writer.write_str(&indent).map_err(RonError::custom)?;
        }
        Ok(())
    }

    /// Write a newline if in pretty mode.
    fn write_newline(&mut self) -> Result<(), RonError> {
        if matches!(self.config, RonConfig::Pretty(_)) {
            self.writer.write_char('\n').map_err(RonError::custom)?;
        }
        Ok(())
    }

    /// Write a space if in pretty mode.
    fn write_space(&mut self) -> Result<(), RonError> {
        if matches!(self.config, RonConfig::Pretty(_)) {
            self.writer.write_char(' ').map_err(RonError::custom)?;
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
    fn write_escaped_string(&mut self, s: &str) -> Result<(), RonError> {
        self.writer.write_char('"').map_err(|e| RonError::custom(e))?;
        for ch in s.chars() {
            match ch {
                '"' => self
                    .writer
                    .write_str("\\\"")
                    .map_err(|e| RonError::custom(e))?,
                '\\' => self
                    .writer
                    .write_str("\\\\")
                    .map_err(|e| RonError::custom(e))?,
                '\n' => self
                    .writer
                    .write_str("\\n")
                    .map_err(|e| RonError::custom(e))?,
                '\r' => self
                    .writer
                    .write_str("\\r")
                    .map_err(|e| RonError::custom(e))?,
                '\t' => self
                    .writer
                    .write_str("\\t")
                    .map_err(|e| RonError::custom(e))?,
                c if c.is_control() => {
                    write!(self.writer, "\\u{{{:04x}}}", c as u32)
                        .map_err(|e| RonError::custom(e))?;
                }
                c => self
                    .writer
                    .write_char(c)
                    .map_err(|e| RonError::custom(e))?,
            }
        }
        self.writer.write_char('"').map_err(|e| RonError::custom(e))?;
        Ok(())
    }

    /// Write a character, escaping if necessary.
    fn write_escaped_char(&mut self, c: char) -> Result<(), RonError> {
        self.writer.write_char('\'').map_err(|e| RonError::custom(e))?;
        match c {
            '\'' => {
                self.writer.write_str("\\'").map_err(|e| RonError::custom(e))?
            }
            '\\' => self
                .writer
                .write_str("\\\\")
                .map_err(|e| RonError::custom(e))?,
            '\n' => {
                self.writer.write_str("\\n").map_err(|e| RonError::custom(e))?
            }
            '\r' => {
                self.writer.write_str("\\r").map_err(|e| RonError::custom(e))?
            }
            '\t' => {
                self.writer.write_str("\\t").map_err(|e| RonError::custom(e))?
            }
            c if c.is_control() => {
                write!(self.writer, "\\u{{{:04x}}}", c as u32)
                    .map_err(|e| RonError::custom(e))?;
            }
            c => self.writer.write_char(c).map_err(|e| RonError::custom(e))?,
        }
        self.writer.write_char('\'').map_err(|e| RonError::custom(e))?;
        Ok(())
    }
}

impl<W: Write + 'static, E> Serializer<E> for RonSerializer<W> {
    type Error = RonError;
    type Seq<'s> = RonSeq<'s, W>;
    type Tuple<'s> = RonTuple<'s, W>;
    type TupleStruct<'s> = RonTupleStruct<'s, W>;
    type Struct<'s> = RonStruct<'s, W>;
    type Map<'s> = RonMap<'s, W>;
    type TupleVariant<'s> = RonTupleVariant<'s, W>;
    type StructVariant<'s> = RonStructVariant<'s, W>;

    fn emit_i8(&mut self, value: i8) -> Result<(), Self::Error> {
        write!(self.writer, "{value}").map_err(|e| RonError::custom(e))
    }

    fn emit_i16(&mut self, value: i16) -> Result<(), Self::Error> {
        write!(self.writer, "{value}").map_err(|e| RonError::custom(e))
    }

    fn emit_i32(&mut self, value: i32) -> Result<(), Self::Error> {
        write!(self.writer, "{value}").map_err(|e| RonError::custom(e))
    }

    fn emit_i64(&mut self, value: i64) -> Result<(), Self::Error> {
        write!(self.writer, "{value}").map_err(|e| RonError::custom(e))
    }

    fn emit_u8(&mut self, value: u8) -> Result<(), Self::Error> {
        write!(self.writer, "{value}").map_err(|e| RonError::custom(e))
    }

    fn emit_u16(&mut self, value: u16) -> Result<(), Self::Error> {
        write!(self.writer, "{value}").map_err(|e| RonError::custom(e))
    }

    fn emit_u32(&mut self, value: u32) -> Result<(), Self::Error> {
        write!(self.writer, "{value}").map_err(|e| RonError::custom(e))
    }

    fn emit_u64(&mut self, value: u64) -> Result<(), Self::Error> {
        write!(self.writer, "{value}").map_err(|e| RonError::custom(e))
    }

    fn emit_isize(&mut self, value: isize) -> Result<(), Self::Error> {
        write!(self.writer, "{value}").map_err(|e| RonError::custom(e))
    }

    fn emit_usize(&mut self, value: usize) -> Result<(), Self::Error> {
        write!(self.writer, "{value}").map_err(|e| RonError::custom(e))
    }

    fn emit_f32(&mut self, value: f32) -> Result<(), Self::Error> {
        if value.is_nan() {
            write!(self.writer, "NaN").map_err(|e| RonError::custom(e))
        } else if value.is_infinite() {
            if value.is_sign_positive() {
                write!(self.writer, "inf").map_err(|e| RonError::custom(e))
            } else {
                write!(self.writer, "-inf").map_err(|e| RonError::custom(e))
            }
        } else {
            write!(self.writer, "{value}").map_err(|e| RonError::custom(e))
        }
    }

    fn emit_f64(&mut self, value: f64) -> Result<(), Self::Error> {
        if value.is_nan() {
            write!(self.writer, "NaN").map_err(|e| RonError::custom(e))
        } else if value.is_infinite() {
            if value.is_sign_positive() {
                write!(self.writer, "inf").map_err(|e| RonError::custom(e))
            } else {
                write!(self.writer, "-inf").map_err(|e| RonError::custom(e))
            }
        } else {
            write!(self.writer, "{value}").map_err(|e| RonError::custom(e))
        }
    }

    fn emit_bool(&mut self, value: bool) -> Result<(), Self::Error> {
        write!(self.writer, "{value}").map_err(|e| RonError::custom(e))
    }

    fn emit_char(&mut self, value: char) -> Result<(), Self::Error> {
        self.write_escaped_char(value)
    }

    fn emit_str(&mut self, value: &str) -> Result<(), Self::Error> {
        self.write_escaped_string(value)
    }

    fn emit_bytes(&mut self, value: &[u8]) -> Result<(), Self::Error> {
        self.writer.write_str("b\"").map_err(|e| RonError::custom(e))?;
        for &byte in value {
            match byte {
                b'"' => self
                    .writer
                    .write_str("\\\"")
                    .map_err(|e| RonError::custom(e))?,
                b'\\' => self
                    .writer
                    .write_str("\\\\")
                    .map_err(|e| RonError::custom(e))?,
                b'\n' => self
                    .writer
                    .write_str("\\n")
                    .map_err(|e| RonError::custom(e))?,
                b'\r' => self
                    .writer
                    .write_str("\\r")
                    .map_err(|e| RonError::custom(e))?,
                b'\t' => self
                    .writer
                    .write_str("\\t")
                    .map_err(|e| RonError::custom(e))?,
                b if b.is_ascii_graphic() || b == b' ' => {
                    self.writer
                        .write_char(b as char)
                        .map_err(|e| RonError::custom(e))?;
                }
                b => {
                    write!(self.writer, "\\x{:02x}", b)
                        .map_err(|e| RonError::custom(e))?;
                }
            }
        }
        self.writer.write_char('"').map_err(|e| RonError::custom(e))?;
        Ok(())
    }

    fn emit_unit(&mut self) -> Result<(), Self::Error> {
        self.writer.write_str("()").map_err(|e| RonError::custom(e))
    }

    fn emit_none(&mut self) -> Result<(), Self::Error> {
        self.writer.write_str("None").map_err(|e| RonError::custom(e))
    }

    fn emit_some<T: Serialize<Self, E>>(
        &mut self,
        value: &T,
        extension: &mut E,
    ) -> Result<(), Self::Error> {
        self.writer.write_str("Some(").map_err(|e| RonError::custom(e))?;
        value.serialize(self, extension)?;
        self.writer.write_char(')').map_err(|e| RonError::custom(e))?;
        Ok(())
    }

    fn emit_seq<'e>(
        &mut self,
        len: usize,
        extension: &'e mut E,
        f: impl FnOnce(Self::Seq<'_>, &'e mut E) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        if len == 0 {
            self.writer.write_str("[]").map_err(RonError::custom)?;
            return Ok(());
        }

        self.writer.write_char('[').map_err(RonError::custom)?;

        let seq = RonSeq::new(self, len);
        f(seq, extension)?;

        self.writer.write_char(']').map_err(RonError::custom)?;
        Ok(())
    }

    fn emit_tuple<'e>(
        &mut self,
        len: usize,
        extension: &'e mut E,
        f: impl FnOnce(Self::Tuple<'_>, &'e mut E) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        if len == 0 {
            self.writer.write_str("()").map_err(|e| RonError::custom(e))?;
            return Ok(());
        }

        self.writer.write_char('(').map_err(|e| RonError::custom(e))?;

        let tuple = RonTuple::new(self, len);
        f(tuple, extension)?;

        self.writer.write_char(')').map_err(|e| RonError::custom(e))?;
        Ok(())
    }

    fn emit_tuple_struct<'e>(
        &mut self,
        name: &'static str,
        len: usize,
        extension: &'e mut E,
        f: impl FnOnce(Self::TupleStruct<'_>, &'e mut E) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        write!(self.writer, "{name}(").map_err(|e| RonError::custom(e))?;
        let tuple_struct = RonTupleStruct::new(self, len);
        f(tuple_struct, extension)?;
        self.writer.write_char(')').map_err(|e| RonError::custom(e))?;
        Ok(())
    }

    fn emit_unit_struct(
        &mut self,
        name: &'static str,
    ) -> Result<(), Self::Error> {
        write!(self.writer, "{name}").map_err(|e| RonError::custom(e))
    }

    fn emit_unit_variant(
        &mut self,
        _name: &'static str,
        variant: &'static str,
        _index: u32,
    ) -> Result<(), Self::Error> {
        // Now we can use the actual variant name for human-readable output
        write!(self.writer, "{variant}").map_err(|e| RonError::custom(e))
    }

    fn emit_map<'e>(
        &mut self,
        len: usize,
        extension: &'e mut E,
        f: impl FnOnce(Self::Map<'_>, &'e mut E) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        if len == 0 {
            self.writer.write_str("{}").map_err(|e| RonError::custom(e))?;
            return Ok(());
        }

        let multiline = matches!(self.config, RonConfig::Pretty(_)) && len > 2;
        self.writer.write_char('{').map_err(|e| RonError::custom(e))?;

        if multiline {
            self.indent();
        }

        let map = RonMap::new(self, len);
        f(map, extension)?;

        if multiline {
            self.write_newline()?;
            self.dedent();
            self.write_indent()?;
        }
        self.writer.write_char('}').map_err(|e| RonError::custom(e))?;
        Ok(())
    }

    fn emit_struct<'e>(
        &mut self,
        name: &'static str,
        len: usize,
        extension: &'e mut E,
        f: impl FnOnce(Self::Struct<'_>, &'e mut E) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        if len == 0 {
            write!(self.writer, "{name} {{}}").map_err(RonError::custom)?;
            return Ok(());
        }

        let multiline = matches!(self.config, RonConfig::Pretty(_)) && len > 2;
        write!(self.writer, "{name}").map_err(RonError::custom)?;
        self.write_space()?;
        self.writer.write_char('{').map_err(RonError::custom)?;

        if multiline {
            self.indent();
        }

        let struct_ser = RonStruct::new(self, len);
        f(struct_ser, extension)?;

        if multiline {
            self.write_newline()?;
            self.dedent();
            self.write_indent()?;
        }
        self.writer.write_char('}').map_err(RonError::custom)?;
        Ok(())
    }

    fn emit_tuple_variant<'e>(
        &mut self,
        _name: &'static str,
        variant: &'static str,
        _index: u32,
        len: usize,
        extension: &'e mut E,
        f: impl FnOnce(Self::TupleVariant<'_>, &'e mut E) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        write!(self.writer, "{variant}(").map_err(|e| RonError::custom(e))?;
        let tuple_variant = RonTupleVariant::new(self, len);
        f(tuple_variant, extension)?;
        self.writer.write_char(')').map_err(|e| RonError::custom(e))?;
        Ok(())
    }

    fn emit_struct_variant<'e>(
        &mut self,
        _name: &'static str,
        variant: &'static str,
        _index: u32,
        len: usize,
        extension: &'e mut E,
        f: impl FnOnce(
            Self::StructVariant<'_>,
            &'e mut E,
        ) -> Result<(), Self::Error>,
    ) -> Result<(), Self::Error> {
        if len == 0 {
            write!(self.writer, "{variant} {{}}").map_err(RonError::custom)?;
            return Ok(());
        }

        let multiline = matches!(self.config, RonConfig::Pretty(_)) && len > 2;
        write!(self.writer, "{variant}").map_err(RonError::custom)?;
        self.write_space()?;
        self.writer.write_char('{').map_err(RonError::custom)?;

        if multiline {
            self.indent();
        }

        let struct_variant = RonStructVariant::new(self, len);
        f(struct_variant, extension)?;

        if multiline {
            self.write_newline()?;
            self.dedent();
            self.write_indent()?;
        }
        self.writer.write_char('}').map_err(|e| RonError::custom(e))?;
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
/// assert_eq!(result, Ok("42".to_string()));
/// ```
pub fn to_ron_string<T>(value: &T) -> Result<String, RonError>
where
    T: Serialize<RonSerializer<String>, ()>,
{
    let mut serializer = RonSerializer::new(String::new());
    value.serialize(&mut serializer, &mut ())?;
    Ok(serializer.into_inner())
}

/// Utility function to serialize a value to RON format with custom
/// configuration.
pub fn to_ron_string_with_config<T>(
    value: &T,
    config: RonConfig,
) -> Result<String, RonError>
where
    T: Serialize<RonSerializer<String>, ()>,
{
    let mut serializer = RonSerializer::with_config(String::new(), config);
    value.serialize(&mut serializer, &mut ())?;
    Ok(serializer.into_inner())
}

/// Utility function to serialize a value to compact RON format (no pretty
/// printing).
pub fn to_ron_string_compact<T>(value: &T) -> Result<String, RonError>
where
    T: Serialize<RonSerializer<String>, ()>,
{
    let mut serializer = RonSerializer::compact(String::new());
    value.serialize(&mut serializer, &mut ())?;
    Ok(serializer.into_inner())
}

// =============================================================================
// Common formatting logic for sequence-like types (arrays, tuples)
// =============================================================================

/// Common logic for formatting sequence-like elements (arrays, tuples)
/// Handles both compact and pretty mode formatting with proper indentation
fn serialize_sequence_element<W: Write + 'static, E, T: Serialize<RonSerializer<W>, E>>(
    serializer: &mut RonSerializer<W>,
    value: &T,
    extension: &mut E,
    count: &mut usize,
    len: usize,
    is_pretty: bool,
) -> Result<(), RonError> {
    // Add separator for compact mode (no space after comma)
    if !is_pretty && *count > 0 {
        serializer.writer.write_char(',').map_err(RonError::custom)?;
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
            serializer.writer.write_char(',').map_err(RonError::custom)?;
            serializer.write_newline()?;
            serializer.write_indent()?;
        }
    }

    value.serialize(serializer, extension)?;
    *count += 1;

    // For pretty mode, handle the closing
    if is_pretty && *count == len {
        // Add trailing comma and dedent
        serializer.writer.write_char(',').map_err(RonError::custom)?;
        serializer.write_newline()?;
        serializer.dedent();
        serializer.write_indent()?;
    }

    Ok(())
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

    fn serialize_element<T: Serialize<Self::Parent, E>>(
        &mut self,
        value: &T,
        extension: &mut E,
    ) -> Result<(), RonError> {
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

    fn serialize_element<T: Serialize<Self::Parent, E>>(
        &mut self,
        value: &T,
        extension: &mut E,
    ) -> Result<(), RonError> {
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
    count: usize,
}

impl<'a, W> RonTupleStruct<'a, W> {
    fn new(serializer: &'a mut RonSerializer<W>, _len: usize) -> Self {
        Self { serializer, count: 0 }
    }
}

impl<W: Write + 'static, E> TupleStruct<E> for RonTupleStruct<'_, W> {
    type Parent = RonSerializer<W>;

    fn serialize_field<T: Serialize<Self::Parent, E>>(
        &mut self,
        value: &T,
        extension: &mut E,
    ) -> Result<(), RonError> {
        if self.count > 0 {
            self.serializer
                .writer
                .write_str(", ")
                .map_err(|e| RonError::custom(e))?;
        }

        value.serialize(self.serializer, extension)?;
        self.count += 1;
        Ok(())
    }
}

pub struct RonStruct<'a, W> {
    serializer: &'a mut RonSerializer<W>,
    len: usize,
    count: usize,
    multiline: bool,
}

impl<'a, W: Write> RonStruct<'a, W> {
    fn new(serializer: &'a mut RonSerializer<W>, len: usize) -> Self {
        let multiline =
            matches!(serializer.config, RonConfig::Pretty(_)) && len > 2;
        Self { serializer, len, count: 0, multiline }
    }
}

impl<W: Write + 'static, E> Struct<E> for RonStruct<'_, W> {
    type Parent = RonSerializer<W>;

    fn serialize_field<T: Serialize<Self::Parent, E>>(
        &mut self,
        name: &'static str,
        value: &T,
        extension: &mut E,
    ) -> Result<(), RonError> {
        if self.count > 0 {
            self.serializer
                .writer
                .write_char(',')
                .map_err(|e| RonError::custom(e))?;
        }

        if self.multiline {
            self.serializer.write_newline()?;
            self.serializer.write_indent()?;
        } else if self.count > 0 {
            self.serializer.write_space()?;
        }

        write!(self.serializer.writer, "{name}:")
            .map_err(|e| RonError::custom(e))?;
        self.serializer.write_space()?;
        value.serialize(self.serializer, extension)?;
        self.count += 1;

        // Add trailing comma for the last field in multiline mode
        if self.count == self.len
            && self.multiline
            && matches!(self.serializer.config, RonConfig::Pretty(_))
        {
            self.serializer
                .writer
                .write_char(',')
                .map_err(|e| RonError::custom(e))?;
        }

        Ok(())
    }

    fn skip_field(
        &mut self,
        _name: &'static str,
        _extension: &mut E,
    ) -> Result<(), RonError> {
        // For RON, we simply don't serialize skipped fields
        Ok(())
    }
}

pub struct RonMap<'a, W> {
    serializer: &'a mut RonSerializer<W>,
    len: usize,
    count: usize,
    multiline: bool,
}

impl<'a, W: Write> RonMap<'a, W> {
    fn new(serializer: &'a mut RonSerializer<W>, len: usize) -> Self {
        let multiline =
            matches!(serializer.config, RonConfig::Pretty(_)) && len > 2;
        // Don't indent here since it's handled in the parent method
        Self { serializer, len, count: 0, multiline }
    }
}

impl<W: Write + 'static, E> Map<E> for RonMap<'_, W> {
    type Parent = RonSerializer<W>;

    fn serialize_entry<
        K: Serialize<Self::Parent, E>,
        V: Serialize<Self::Parent, E>,
    >(
        &mut self,
        key: &K,
        value: &V,
        extension: &mut E,
    ) -> Result<(), RonError> {
        if self.multiline {
            self.serializer.write_newline()?;
            self.serializer.write_indent()?;
        } else if self.count > 0 {
            self.serializer
                .writer
                .write_str(", ")
                .map_err(|e| RonError::custom(e))?;
        }

        key.serialize(self.serializer, extension)?;
        self.serializer
            .writer
            .write_str(":")
            .map_err(|e| RonError::custom(e))?;
        self.serializer.write_space()?;
        value.serialize(self.serializer, extension)?;
        self.count += 1;

        if self.count < self.len {
            self.serializer
                .writer
                .write_char(',')
                .map_err(|e| RonError::custom(e))?;
        } else if self.multiline
            && matches!(self.serializer.config, RonConfig::Pretty(_))
        {
            self.serializer
                .writer
                .write_char(',')
                .map_err(|e| RonError::custom(e))?;
        }

        Ok(())
    }
}

pub struct RonTupleVariant<'a, W> {
    serializer: &'a mut RonSerializer<W>,
    count: usize,
}

impl<'a, W> RonTupleVariant<'a, W> {
    fn new(serializer: &'a mut RonSerializer<W>, _len: usize) -> Self {
        Self { serializer, count: 0 }
    }
}

impl<W: Write + 'static, E> TupleVariant<E> for RonTupleVariant<'_, W> {
    type Parent = RonSerializer<W>;

    fn serialize_field<T: Serialize<Self::Parent, E>>(
        &mut self,
        value: &T,
        extension: &mut E,
    ) -> Result<(), RonError> {
        if self.count > 0 {
            self.serializer
                .writer
                .write_str(", ")
                .map_err(|e| RonError::custom(e))?;
        }

        value.serialize(self.serializer, extension)?;
        self.count += 1;
        Ok(())
    }
}

pub struct RonStructVariant<'a, W> {
    serializer: &'a mut RonSerializer<W>,
    len: usize,
    count: usize,
    multiline: bool,
}

impl<'a, W: Write> RonStructVariant<'a, W> {
    fn new(serializer: &'a mut RonSerializer<W>, len: usize) -> Self {
        let multiline =
            matches!(serializer.config, RonConfig::Pretty(_)) && len > 2;
        Self { serializer, len, count: 0, multiline }
    }
}

impl<W: Write + 'static, E> StructVariant<E> for RonStructVariant<'_, W> {
    type Parent = RonSerializer<W>;

    fn serialize_field<T: Serialize<Self::Parent, E>>(
        &mut self,
        name: &'static str,
        value: &T,
        extension: &mut E,
    ) -> Result<(), RonError> {
        if self.count > 0 {
            self.serializer
                .writer
                .write_char(',')
                .map_err(|e| RonError::custom(e))?;
        }

        if self.multiline {
            self.serializer.write_newline()?;
            self.serializer.write_indent()?;
        } else if self.count > 0 {
            self.serializer.write_space()?;
        }

        write!(self.serializer.writer, "{name}:")
            .map_err(|e| RonError::custom(e))?;
        self.serializer.write_space()?;
        value.serialize(self.serializer, extension)?;
        self.count += 1;

        // Add trailing comma for the last field in multiline mode
        if self.count == self.len
            && self.multiline
            && matches!(self.serializer.config, RonConfig::Pretty(_))
        {
            self.serializer
                .writer
                .write_char(',')
                .map_err(|e| RonError::custom(e))?;
        }

        Ok(())
    }

    fn skip_field(
        &mut self,
        _name: &'static str,
        _extension: &mut E,
    ) -> Result<(), RonError> {
        // For RON, we simply don't serialize skipped fields
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn basic_functionality() {
        // This is a basic smoke test
        let config = RonConfig::default();
        assert!(
            matches!(config, RonConfig::Pretty(ref indent) if indent == "    ")
        );

        let error = RonError::custom("test error");
        assert!(error.to_string().contains("test error"));
    }

    #[test]
    fn serializer_creation() {
        let mut buffer = String::new();
        let _serializer = RonSerializer::new(&mut buffer);

        let mut buffer2 = String::new();
        let config = RonConfig::Pretty("  ".to_string());
        let _serializer2 = RonSerializer::with_config(&mut buffer2, config);

        let mut buffer3 = String::new();
        let _serializer3 = RonSerializer::compact(&mut buffer3);
    }

    #[test]
    fn primitives() {
        insta::assert_snapshot!(to_ron_string(&42).unwrap(), @"42");
        insta::assert_snapshot!(to_ron_string(&true).unwrap(), @"true");
        insta::assert_snapshot!(to_ron_string(&false).unwrap(), @"false");
        insta::assert_snapshot!(to_ron_string(&2.718).unwrap(), @"2.718");
        insta::assert_snapshot!(to_ron_string(&"hello world").unwrap(), @r#""hello world""#);
        insta::assert_snapshot!(to_ron_string(&'A').unwrap(), @"'A'");
    }

    #[test]
    fn arrays_compact() {
        let simple = vec![1, 2, 3];
        let nested = vec![vec![1, 2], vec![3, 4, 5], vec![]];

        insta::assert_snapshot!(to_ron_string_compact(&simple).unwrap());
        insta::assert_snapshot!(to_ron_string_compact(&nested).unwrap());
    }

    #[test]
    fn arrays_pretty() {
        let simple = vec![1, 2, 3];
        let nested = vec![vec![1, 2], vec![3, 4, 5], vec![]];

        let config = RonConfig::Pretty("    ".to_string());
        insta::assert_snapshot!(to_ron_string_with_config(
            &simple,
            config.clone()
        )
        .unwrap());
        insta::assert_snapshot!(to_ron_string_with_config(
            &nested,
            config.clone()
        )
        .unwrap());
    }

    #[test]
    fn string_escaping() {
        insta::assert_snapshot!(to_ron_string(&"hello \"world\"").unwrap(), @r#""hello \"world\"""#);
        insta::assert_snapshot!(to_ron_string(&"line1\nline2").unwrap(), @r#""line1\nline2""#);
        insta::assert_snapshot!(to_ron_string(&"tab\there").unwrap(), @r#""tab\there""#);
        insta::assert_snapshot!(to_ron_string(&"backslash\\test").unwrap(), @r#""backslash\\test""#);
    }

    #[test]
    fn simple_arrays_compact() {
        let empty: Vec<i32> = vec![];
        let single = vec![42];
        let multiple = vec![1, 2, 3, 4, 5];

        insta::assert_snapshot!(to_ron_string_compact(&empty).unwrap(), @"[]");
        insta::assert_snapshot!(to_ron_string_compact(&single).unwrap(), @"[42]");
        insta::assert_snapshot!(to_ron_string_compact(&multiple).unwrap(), @"[1,2,3,4,5]");
    }

    #[test]
    fn nested_arrays_compact() {
        let nested = vec![vec![1, 2], vec![3, 4, 5], vec![]];

        insta::assert_snapshot!(to_ron_string_compact(&nested).unwrap(), @"[[1,2],[3,4,5],[]]");
    }

    // ========================================================================
    // Tuple formatting tests
    // ========================================================================

    #[test]
    fn tuples_compact() {
        let empty = ();
        let single = (42,);
        let pair = (1, 2);
        let triple = (1, "hello", true);
        let nested = ((1, 2), (3, 4));

        insta::assert_snapshot!(to_ron_string_compact(&empty).unwrap());
        insta::assert_snapshot!(to_ron_string_compact(&single).unwrap());
        insta::assert_snapshot!(to_ron_string_compact(&pair).unwrap());
        insta::assert_snapshot!(to_ron_string_compact(&triple).unwrap());
        insta::assert_snapshot!(to_ron_string_compact(&nested).unwrap());
    }

    #[test]
    fn tuples_pretty() {
        let empty = ();
        let single = (42,);
        let pair = (1, 2);
        let triple = (1, "hello", true);
        let nested = ((1, 2), (3, 4));

        let config = RonConfig::Pretty("    ".to_string());
        insta::assert_snapshot!(to_ron_string_with_config(
            &empty,
            config.clone()
        )
        .unwrap());
        insta::assert_snapshot!(to_ron_string_with_config(
            &single,
            config.clone()
        )
        .unwrap());
        insta::assert_snapshot!(to_ron_string_with_config(
            &pair,
            config.clone()
        )
        .unwrap());
        insta::assert_snapshot!(to_ron_string_with_config(
            &triple,
            config.clone()
        )
        .unwrap());
        insta::assert_snapshot!(
            to_ron_string_with_config(&nested, config).unwrap()
        );
    }
}
