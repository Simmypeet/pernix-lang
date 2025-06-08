//! Shared pointer deserialization extension for Arc and Rc types.
//!
//! This module provides specialized deserialization for `Arc<T>` and `Rc<T>`
//! that can reconstruct shared pointers by tracking previously deserialized
//! objects and reusing them when the same pointer address is encountered.

use std::{collections::HashMap, rc::Rc, sync::Arc};

use crate::de::{Deserialize, Deserializer};

/// Extension trait for deserializers that support shared pointer reconstruction.
///
/// This trait allows deserializers to track `Arc<T>` and `Rc<T>` pointers
/// and reconstruct shared references when the same pointer address is
/// encountered multiple times.
pub trait SharedPointerExtension {
    /// Store an Arc value by its pointer address for later reference.
    ///
    /// # Arguments
    ///
    /// * `ptr` - The pointer address as a usize
    /// * `value` - The Arc value to store
    fn store_arc<T: 'static>(&mut self, ptr: usize, value: Arc<T>);

    /// Retrieve a previously stored Arc value by its pointer address.
    ///
    /// # Arguments
    ///
    /// * `ptr` - The pointer address as a usize
    ///
    /// # Returns
    ///
    /// `Some(Arc<T>)` if the pointer was previously stored, `None` otherwise.
    fn get_arc<T: 'static>(&self, ptr: usize) -> Option<Arc<T>>;

    /// Store an Rc value by its pointer address for later reference.
    ///
    /// # Arguments
    ///
    /// * `ptr` - The pointer address as a usize
    /// * `value` - The Rc value to store
    fn store_rc<T: 'static>(&mut self, ptr: usize, value: Rc<T>);

    /// Retrieve a previously stored Rc value by its pointer address.
    ///
    /// # Arguments
    ///
    /// * `ptr` - The pointer address as a usize
    ///
    /// # Returns
    ///
    /// `Some(Rc<T>)` if the pointer was previously stored, `None` otherwise.
    fn get_rc<T: 'static>(&self, ptr: usize) -> Option<Rc<T>>;
}

/// Default implementation of shared pointer tracking using type-erased storage.
///
/// This struct maintains separate tracking for Arc and Rc pointers using
/// type-erased storage to handle multiple types.
#[derive(Debug, Default)]
pub struct DefaultSharedPointerTracker {
    /// Map of Arc pointer addresses to type-erased Arc values
    arc_storage: HashMap<usize, Box<dyn std::any::Any + Send + Sync>>,
    /// Map of Rc pointer addresses to type-erased Rc values
    rc_storage: HashMap<usize, Box<dyn std::any::Any>>,
}

impl SharedPointerExtension for DefaultSharedPointerTracker {
    fn store_arc<T: 'static>(&mut self, ptr: usize, value: Arc<T>) {
        self.arc_storage.insert(ptr, Box::new(value));
    }

    fn get_arc<T: 'static>(&self, ptr: usize) -> Option<Arc<T>> {
        self.arc_storage
            .get(&ptr)
            .and_then(|any| any.downcast_ref::<Arc<T>>())
            .cloned()
    }

    fn store_rc<T: 'static>(&mut self, ptr: usize, value: Rc<T>) {
        self.rc_storage.insert(ptr, Box::new(value));
    }

    fn get_rc<T: 'static>(&self, ptr: usize) -> Option<Rc<T>> {
        self.rc_storage
            .get(&ptr)
            .and_then(|any| any.downcast_ref::<Rc<T>>())
            .cloned()
    }
}

impl<D, T> Deserialize<D> for Arc<T>
where
    D: Deserializer,
    D::Extension: SharedPointerExtension,
    T: Deserialize<D> + 'static,
{
    fn deserialize(deserializer: &mut D) -> Result<Self, D::Error> {
        let (variant, index) = deserializer.expect_enum("Arc", &["Owned", "Reference"])?;
        
        match (variant, index) {
            ("Owned", 0) => {
                let mut struct_access = deserializer.expect_struct_variant(&["pointer", "value"])?;
                
                // Get the pointer address
                let pointer: usize = struct_access.field("pointer")?.unwrap_or_default();
                
                // Deserialize the value
                let value: T = struct_access.field("value")?.unwrap_or_else(|| {
                    // This needs proper error handling
                    panic!("Missing value field in Arc::Owned variant")
                });
                
                let arc = Arc::new(value);
                deserializer.extension().store_arc(pointer, arc.clone());
                Ok(arc)
            }
            ("Reference", 1) => {
                let mut struct_access = deserializer.expect_struct_variant(&["pointer"])?;
                let pointer: usize = struct_access.field("pointer")?.unwrap_or_default();
                
                deserializer.extension().get_arc(pointer).ok_or_else(|| {
                    // This needs proper error handling
                    panic!("Referenced Arc pointer not found: {}", pointer)
                })
            }
            _ => {
                // This needs proper error handling
                panic!("Unknown Arc variant: {}", variant)
            }
        }
    }
}

impl<D, T> Deserialize<D> for Rc<T>
where
    D: Deserializer,
    D::Extension: SharedPointerExtension,
    T: Deserialize<D> + 'static,
{
    fn deserialize(deserializer: &mut D) -> Result<Self, D::Error> {
        let (variant, index) = deserializer.expect_enum("Rc", &["Owned", "Reference"])?;
        
        match (variant, index) {
            ("Owned", 0) => {
                let mut struct_access = deserializer.expect_struct_variant(&["pointer", "value"])?;
                
                // Get the pointer address
                let pointer: usize = struct_access.field("pointer")?.unwrap_or_default();
                
                // Deserialize the value
                let value: T = struct_access.field("value")?.unwrap_or_else(|| {
                    // This needs proper error handling
                    panic!("Missing value field in Rc::Owned variant")
                });
                
                let rc = Rc::new(value);
                deserializer.extension().store_rc(pointer, rc.clone());
                Ok(rc)
            }
            ("Reference", 1) => {
                let mut struct_access = deserializer.expect_struct_variant(&["pointer"])?;
                let pointer: usize = struct_access.field("pointer")?.unwrap_or_default();
                
                deserializer.extension().get_rc(pointer).ok_or_else(|| {
                    // This needs proper error handling
                    panic!("Referenced Rc pointer not found: {}", pointer)
                })
            }
            _ => {
                // This needs proper error handling
                panic!("Unknown Rc variant: {}", variant)
            }
        }
    }
}
