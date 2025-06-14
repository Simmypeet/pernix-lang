//! Defines the serialization/deserialization logic for the Pernixc compiler.

// this allows to use `pernixc_serialize` as a crate name
extern crate self as pernixc_serialize;

// Re-export derive macro when the derive feature is enabled
pub use pernixc_serialize_derive::Serialize;
pub use ser::Serialize;

#[doc(hidden)]
pub mod __internal {
    // Re-export traits needed by derived code
    pub use crate::ser::{
        Serialize, Serializer, Struct, StructVariant, TupleStruct, TupleVariant,
    };
}

pub mod binary;
pub mod de;
pub mod ser;

#[cfg(test)]
mod derive_test;
