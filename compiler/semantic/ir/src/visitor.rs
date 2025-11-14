//! Implements a visitor pattern for inspecting the sub-values of an
//! [`Element`].

use std::borrow::Cow;

use crate::{address::Address, value::Value};

/// Implemented by the [`Address`] and [`Assignment`] to inspect the value that
/// it contains.
pub trait Element {
    /// Accepts a visitor to inspect the value.
    fn accept(&self, visitor: &mut impl Visitor);
}

/// A visitor that can inspect the subelements of an [`Element`].
pub trait Visitor {
    /// Visits a value.
    fn visit_value(&mut self, value: Cow<Value>);

    /// Visits an address.
    fn visit_address(&mut self, address: Cow<Address>);
}

// NOTE: If in the future we would want to support `async` visitors,
// we can add async version of `accept` and `Visitor`.
