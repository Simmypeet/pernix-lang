//! Implementation of the semantic analysis phase of the compiler.

pub mod component;
pub mod transitive_closure;
pub mod type_system;
pub mod unordered_pair;

#[cfg(test)]
pub(crate) mod test;
