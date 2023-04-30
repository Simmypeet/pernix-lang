//! Contains all the definitions of bound syntax trees.

use std::marker::PhantomData;

use super::TypeSystem;

/// Represents a bound syntax tree.
///
/// The bound syntax tree is attached with type information and additional semantics.
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum Binding<T: TypeSystem> {
    _p(PhantomData<T>),
}
