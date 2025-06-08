//! Extension mechanism for custom serialization behavior.
//!
//! This module provides extension traits and types that allow customization
//! of serialization behavior for specific types. Extensions can maintain
//! state across serialization operations and provide specialized handling
//! for complex scenarios like shared pointer deduplication.

pub mod shared;
