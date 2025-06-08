//! Extension mechanism for custom deserialization behavior.
//!
//! This module provides extension traits and types that allow customization
//! of deserialization behavior for specific types. Extensions can maintain
//! state across deserialization operations and provide specialized handling
//! for complex scenarios like shared pointer reconstruction.

pub mod shared;
