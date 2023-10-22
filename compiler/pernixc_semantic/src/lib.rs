//! The semantic analyzer of the Pernix compiler.
//!
//! The crate includes following features:
//!
//! - Symbol table and resolution
//! - Typing and Constant model
//! - Intermediate Representation
//! - Compile-time evaluation (IR evaluation)

#![deny(
    missing_docs,
    missing_debug_implementations,
    missing_copy_implementations,
    clippy::all,
    clippy::pedantic,
    clippy::nursery,
    rustdoc::broken_intra_doc_links,
    clippy::missing_errors_doc
)]
#![allow(clippy::missing_panics_doc, clippy::missing_const_for_fn)]

pub mod arena;
pub mod entity;
pub mod logic;
pub mod symbol;
pub mod table;
