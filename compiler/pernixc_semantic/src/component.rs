//! Contains all the definition of the components that can be attached to a
//! symbol.
//!
//! # Classification of Components
//!
//! ## Local vs Presistent Components
//!
//! The components are said to be **local** if they are only used during the
//! compilation of the current target and is not being serialized.
//!
//! On the other hand, the components are said to be **persistent** if they can
//! be serialized and deserialized.
//!
//! ## Input vs Derived Components
//!
//! The **input** components are the starting components that are used to derive
//! other components from them.

use std::any::Any;

pub mod derived;
pub mod input;

/// Represents a component that can be later added to the table by being built
/// by the [`crate::table::query::Builder`] trait.
pub trait Derived: Any + Send + Sync {
    /// Returns the name of the component; used for debugging and diagnostics.
    fn component_name() -> &'static str;
}

/// A maker trait for the **input** components.
pub trait Input {}

/// A maker trait for the **input** components that can be mutated.
pub trait InputMut: Input {}
