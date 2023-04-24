//! This crate provides the lexical analysis phase of the compiler. This phase is responsible for
//! tokenizing the source code into a stream of tokens.
//!
//! The final output of this phase is a [`token_stream::TokenStream`], representing the list of
//! tokens of a source file.

#![deny(
    missing_debug_implementations,
    missing_copy_implementations,
    clippy::all,
    clippy::pedantic,
    clippy::nursery,
    rustdoc::broken_intra_doc_links,
    clippy::missing_errors_doc
)]
#![allow(clippy::missing_panics_doc, clippy::missing_const_for_fn)]

pub mod errors;
pub mod token;
pub mod token_stream;
