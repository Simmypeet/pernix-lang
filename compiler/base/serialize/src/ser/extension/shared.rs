//! Shared pointer serialization extension for Arc and Rc types.
//!
//! This module provides specialized serialization for `Arc<T>` and `Rc<T>`
//! that tracks memory pointers and serializes shared pointers only once
//! on first encounter, with subsequent encounters serialized as references.

use std::{collections::HashSet, rc::Rc, sync::Arc};

use crate::ser::{Serialize, Serializer, StructVariant};

/// Extension trait for serializers that support shared pointer tracking.
///
/// This trait allows serializers to track `Arc<T>` and `Rc<T>` pointers
/// and avoid duplicate serialization of the same shared data.
pub trait SharedPointerExtension {
    /// Register an Arc pointer and return whether it's the first encounter.
    ///
    /// # Arguments
    ///
    /// * `ptr` - Raw pointer to the Arc's data
    ///
    /// # Returns
    ///
    /// `true` if this is the first time the pointer is encountered,
    /// `false` if it has been seen before.
    fn register_arc_pointer(&mut self, ptr: *const ()) -> bool;

    /// Register an Rc pointer and return whether it's the first encounter.
    ///
    /// # Arguments
    ///
    /// * `ptr` - Raw pointer to the Rc's data
    ///
    /// # Returns
    ///
    /// `true` if this is the first time the pointer is encountered,
    /// `false` if it has been seen before.
    fn register_rc_pointer(&mut self, ptr: *const ()) -> bool;
}

/// Default implementation of shared pointer tracking using hash sets.
///
/// This struct maintains separate tracking for Arc and Rc pointers.
#[derive(Debug, Default)]
pub struct DefaultSharedPointerTracker {
    /// Set of Arc pointer addresses that have been seen
    arc_pointers: HashSet<*const ()>,
    /// Set of Rc pointer addresses that have been seen
    rc_pointers: HashSet<*const ()>,
}

impl SharedPointerExtension for DefaultSharedPointerTracker {
    fn register_arc_pointer(&mut self, ptr: *const ()) -> bool {
        self.arc_pointers.insert(ptr)
    }

    fn register_rc_pointer(&mut self, ptr: *const ()) -> bool {
        self.rc_pointers.insert(ptr)
    }
}

impl<S, T> Serialize<S> for Arc<T>
where
    S: Serializer,
    S::Extension: SharedPointerExtension,
    T: Serialize<S> + ?Sized,
{
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
        let ptr = Self::as_ptr(self).cast::<()>();
        let extension = serializer.extension();

        if extension.register_arc_pointer(ptr) {
            // First encounter - serialize full value
            serializer.emit_struct_variant(
                "Arc",
                "Owned",
                0,
                2,
                |mut struct_variant| {
                    struct_variant
                        .serialize_field("pointer", &(ptr as usize))?;
                    struct_variant.serialize_field("value", &**self)
                },
            )
        } else {
            // Subsequent encounter - serialize as reference
            serializer.emit_struct_variant(
                "Arc",
                "Reference",
                1,
                1,
                |mut struct_variant| {
                    struct_variant.serialize_field("pointer", &(ptr as usize))
                },
            )
        }
    }
}

impl<S, T> Serialize<S> for Rc<T>
where
    S: Serializer,
    S::Extension: SharedPointerExtension,
    T: Serialize<S> + ?Sized,
{
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
        let ptr = Self::as_ptr(self).cast::<()>();
        let extension = serializer.extension();

        if extension.register_rc_pointer(ptr) {
            // First encounter - serialize full value
            serializer.emit_struct_variant(
                "Rc",
                "Owned",
                0,
                2,
                |mut struct_variant| {
                    struct_variant
                        .serialize_field("pointer", &(ptr as usize))?;
                    struct_variant.serialize_field("value", &**self)
                },
            )
        } else {
            // Subsequent encounter - serialize as reference
            serializer.emit_struct_variant(
                "Rc",
                "Reference",
                1,
                1,
                |mut struct_variant| {
                    struct_variant.serialize_field("pointer", &(ptr as usize))
                },
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_shared_pointer_tracker() {
        let mut tracker = DefaultSharedPointerTracker::default();

        let ptr1 = 0x1000 as *const ();
        let ptr2 = 0x2000 as *const ();

        // First encounter should return true (is first time)
        assert!(tracker.register_arc_pointer(ptr1));

        // Second encounter should return false (not first time)
        assert!(!tracker.register_arc_pointer(ptr1));

        // Different pointer should return true (is first time)
        assert!(tracker.register_arc_pointer(ptr2));

        // Rc pointers should be tracked separately
        assert!(tracker.register_rc_pointer(ptr1));
    }
}
