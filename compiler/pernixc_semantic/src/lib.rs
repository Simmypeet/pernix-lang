//! Implementation of the semantic analysis phase of the compiler.

pub mod arena;
pub mod component;
pub mod diagnostic;
// pub mod error;
// pub mod ir;
// pub mod symbol;
pub mod table;
pub mod transitive_closure;
pub mod type_system;
pub mod unordered_pair;

#[cfg(test)]
pub(crate) mod test;
