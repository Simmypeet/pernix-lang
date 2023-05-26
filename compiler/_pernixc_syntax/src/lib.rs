//! This crate provides the syntax tree and parser for the Pernix programming language. This phase
//! is responsible for parsing the token stream into a syntax tree.
//!
//! The final output of this phase is a [`target_parsing::TargetParsing`], representing the syntax
//! tree of a source file.

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

pub mod error;
pub mod parser;
#[allow(missing_docs)]
pub mod syntax_tree;
pub mod target_parsing;