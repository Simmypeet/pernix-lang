//! Shared pointer serialization extension for Arc and Rc types.
//!
//! This module provides specialized serialization for `Arc<T>` and `Rc<T>`
//! that tracks memory pointers and serializes shared pointers only once
//! on first encounter, with subsequent encounters serialized as references.

use std::{collections::HashMap, rc::Rc, sync::Arc};

use crate::ser::{Serialize, Serializer, StructVariant};

/// Extension trait for serializers that support shared pointer tracking.
///
/// This trait allows serializers to track `Arc<T>` and `Rc<T>` pointers
/// and avoid duplicate serialization of the same shared data.
pub trait SharedPointerExtension {
    /// Check if an Arc pointer has been seen before.
    ///
    /// # Arguments
    ///
    /// * `ptr` - Raw pointer to the Arc's data
    ///
    /// # Returns
    ///
    /// `Some(id)` if the pointer was seen before, `None` if it's the first
    /// time.
    fn check_arc_pointer(&mut self, ptr: *const ()) -> Option<usize>;

    /// Register a new Arc pointer and return its unique ID.
    ///
    /// # Arguments
    ///
    /// * `ptr` - Raw pointer to the Arc's data
    ///
    /// # Returns
    ///
    /// Unique ID assigned to this pointer.
    fn register_arc_pointer(&mut self, ptr: *const ()) -> usize;

    /// Check if an Rc pointer has been seen before.
    ///
    /// # Arguments
    ///
    /// * `ptr` - Raw pointer to the Rc's data
    ///
    /// # Returns
    ///
    /// `Some(id)` if the pointer was seen before, `None` if it's the first
    /// time.
    fn check_rc_pointer(&mut self, ptr: *const ()) -> Option<usize>;

    /// Register a new Rc pointer and return its unique ID.
    ///
    /// # Arguments
    ///
    /// * `ptr` - Raw pointer to the Rc's data
    ///
    /// # Returns
    ///
    /// Unique ID assigned to this pointer.
    fn register_rc_pointer(&mut self, ptr: *const ()) -> usize;
}

/// Default implementation of shared pointer tracking using hash maps.
///
/// This struct maintains separate tracking for Arc and Rc pointers,
/// assigning unique sequential IDs to each new pointer encountered.
#[derive(Debug, Default)]
pub struct DefaultSharedPointerTracker {
    /// Map from Arc pointer addresses to their assigned IDs
    arc_pointers: HashMap<*const (), usize>,
    /// Map from Rc pointer addresses to their assigned IDs
    rc_pointers: HashMap<*const (), usize>,
    /// Next ID to assign to an Arc pointer
    next_arc_id: usize,
    /// Next ID to assign to an Rc pointer
    next_rc_id: usize,
}

impl SharedPointerExtension for DefaultSharedPointerTracker {
    fn check_arc_pointer(&mut self, ptr: *const ()) -> Option<usize> {
        self.arc_pointers.get(&ptr).copied()
    }

    fn register_arc_pointer(&mut self, ptr: *const ()) -> usize {
        let id = self.next_arc_id;
        self.arc_pointers.insert(ptr, id);
        self.next_arc_id += 1;
        id
    }

    fn check_rc_pointer(&mut self, ptr: *const ()) -> Option<usize> {
        self.rc_pointers.get(&ptr).copied()
    }

    fn register_rc_pointer(&mut self, ptr: *const ()) -> usize {
        let id = self.next_rc_id;
        self.rc_pointers.insert(ptr, id);
        self.next_rc_id += 1;
        id
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

        if extension.check_arc_pointer(ptr).is_some() {
            // Subsequent encounter - serialize as reference
            serializer.emit_struct_variant(
                "Arc",
                "Reference",
                1,
                1,
                |struct_variant| {
                    struct_variant.serialize_field("pointer", &(ptr as usize))
                },
            )
        } else {
            // First encounter - register and serialize full value
            extension.register_arc_pointer(ptr);
            serializer.emit_struct_variant(
                "Arc",
                "Owned",
                0,
                2,
                |struct_variant| {
                    struct_variant
                        .serialize_field("pointer", &(ptr as usize))?;
                    struct_variant.serialize_field("value", &**self)
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

        if extension.check_rc_pointer(ptr).is_some() {
            // Subsequent encounter - serialize as reference
            serializer.emit_struct_variant(
                "Rc",
                "Reference",
                1,
                1,
                |struct_variant| {
                    struct_variant.serialize_field("pointer", &(ptr as usize))
                },
            )
        } else {
            // First encounter - register and serialize full value
            extension.register_rc_pointer(ptr);
            serializer.emit_struct_variant(
                "Rc",
                "Owned",
                0,
                2,
                |struct_variant| {
                    struct_variant
                        .serialize_field("pointer", &(ptr as usize))?;
                    struct_variant.serialize_field("value", &**self)
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

        // First encounter should return None and register
        assert_eq!(tracker.check_arc_pointer(ptr1), None);
        let id1 = tracker.register_arc_pointer(ptr1);
        assert_eq!(id1, 0);

        // Second encounter should return the ID
        assert_eq!(tracker.check_arc_pointer(ptr1), Some(0));

        // Different pointer should get different ID
        assert_eq!(tracker.check_arc_pointer(ptr2), None);
        let id2 = tracker.register_arc_pointer(ptr2);
        assert_eq!(id2, 1);

        // Rc pointers should be tracked separately
        assert_eq!(tracker.check_rc_pointer(ptr1), None);
        let rc_id1 = tracker.register_rc_pointer(ptr1);
        assert_eq!(rc_id1, 0);
    }
}
