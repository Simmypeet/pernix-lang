//! This module contains the binding phase of the compiler. Bininding is the
//! most important component of the semantic analysis phase. It is responsible
//! for checking semantic correctness of the program and generating the bound
//! abstract syntax tree (AST) for the program.
//!
//! The process of binding is done by attaching semantic information to the
//! abstract syntax tree (AST) nodes. For example, the type of an expression
//! is attached to the expression node.

pub(crate) mod binder;
pub mod bound_declaration;
pub mod bound_expression;
pub mod bound_statement;
