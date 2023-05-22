//! This crates implements the syntactic analysis phase of the compiler.

#![deny(
    missing_debug_implementations,
    missing_copy_implementations,
    missing_docs,
    clippy::all,
    clippy::pedantic,
    clippy::nursery,
    rustdoc::broken_intra_doc_links,
    clippy::missing_errors_doc
)]
#![allow(clippy::missing_panics_doc, clippy::missing_const_for_fn)]

pub mod error;
pub mod parser;
#[allow(missing_docs)]
pub mod syntax_tree;
