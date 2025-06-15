//! This crate implements the lexical analysis phase of the compiler. This phase
//! is responsible for tokenizing the source code into a stream of tokens.
//!
//! The final output of this phase is a [`token_stream::TokenStream`],
//! representing the list of tokens of a source file.

pub mod error;
pub mod kind;
pub mod token;
pub mod tree;
