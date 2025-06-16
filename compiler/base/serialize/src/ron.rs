//! RON (Rusty Object Notation) serialization module for human-readable output.
//!
//! This module provides a human-readable serializer that outputs data in a
//! format similar to RON (Rusty Object Notation) or Debug format. The
//! serializer focuses on readability with proper indentation, spacing, and
//! formatting.
//!
//! # Features
//!
//! - **Human-readable**: Output is formatted for easy reading
//! - **Pretty printing**: Proper indentation and line breaks
//! - **Debug-like format**: Similar to Rust's Debug trait output
//! - **Configurable**: Customizable indentation and formatting options
//!
//! # Examples
//!
//! ```rust
//! use pernixc_serialize::{ron::ser::RonSerializer, ser::Serialize};
//! use pernixc_serialize_derive::Serialize;
//!
//! let buffer = Vec::new();
//! let mut serializer = RonSerializer::new(buffer);
//!
//! // Serialize a struct
//! #[derive(Serialize)]
//! struct Person {
//!     name: String,
//!     age: u32,
//!     active: bool,
//! }
//!
//! let person = Person { name: "Alice".to_string(), age: 30, active: true };
//!
//! person.serialize(&mut serializer, &mut ()).unwrap();
//! let output = String::from_utf8(serializer.into_inner()).unwrap();
//! // Output:
//! // Person {
//! //     name: "Alice",
//! //     age: 30,
//! //     active: true,
//! // }
//! ```

pub mod ser;
