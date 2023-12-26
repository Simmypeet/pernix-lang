//! This crate provides the syntax tree and parser for the Pernix programming language. This phase
//! is responsible for parsing the token stream into a syntax tree.
//!
//! The final output of this phase is a [`target_parsing::TargetParsing`], representing the syntax
//! tree of a source file.

pub mod error;
pub mod parser;
pub mod syntax_tree;
