//! The semantic analyzer of the Pernix compiler.
//!
//! The crate includes following features:
//!
//! - Symbol table and resolution
//! - Typing and Constant model
//! - Intermediate Representation
//! - Compile-time evaluation (IR evaluation)

pub mod arena;
pub mod error;
pub mod semantic;
pub mod symbol;
pub mod table;
