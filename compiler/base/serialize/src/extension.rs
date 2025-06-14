//! Extension traits for specialized serialization behavior.
//!
//! This module provides extension traits that can be used with the serializer's
//! extension mechanism to enable specialized handling for specific types and
//! patterns.

pub mod shared_pointer;

pub use shared_pointer::{SharedPointerDeserialize, SharedPointerSerialize};
