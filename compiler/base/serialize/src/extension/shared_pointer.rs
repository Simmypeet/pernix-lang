//! Shared pointer extension for efficient Arc/Rc serialization.
//!
//! This extension allows Arc and Rc pointers that share the same memory address
//! to be serialized only once, with subsequent references serialized as
//! pointers to the original data. This prevents duplication and handles cycles
//! in data structures.
//!
//! # Important Requirements
//!
//! **Linear Serialization Order**: This extension requires that the serializer
//! processes items in a deterministic, linear fashion. This ensures that when
//! deserializing, the first encounter of any pointer address is always the
//! "Owned" variant containing the actual data, and subsequent encounters are
//! "Reference" variants that can successfully look up the previously stored
//! data.
//!
//! If serialization order is not deterministic or if reference variants are
//! encountered before their corresponding owned variants during
//! deserialization, the deserialization will fail with an error indicating that
//! the referenced pointer was not found.
//!
//! # Example
//!
//! ```rust
//! use std::{io::Cursor, sync::Arc};
//!
//! use pernixc_serialize::{
//!     binary::{de::BinaryDeserializer, ser::BinarySerializer},
//!     de::Deserialize,
//!     extension::shared_pointer::{SharedPointerStore, SharedPointerTracker},
//!     ser::Serialize,
//! };
//!
//! // Create shared data
//! let shared_data = Arc::new(String::from("shared"));
//! let arc1 = shared_data.clone();
//! let arc2 = shared_data.clone();
//! let data = vec![arc1, arc2];
//!
//! // Serialize with shared pointer tracking
//! let tracker = SharedPointerTracker::new();
//! let buffer = Vec::new();
//! let mut serializer = BinarySerializer::with_extension(buffer, tracker);
//! data.serialize(&mut serializer).unwrap();
//! let (buffer, _) = serializer.into_parts();
//!
//! // Deserialize with shared pointer reconstruction
//! let store = SharedPointerStore::new();
//! let cursor = Cursor::new(buffer);
//! let mut deserializer = BinaryDeserializer::with_extension(cursor, store);
//! let result: Vec<Arc<String>> = Vec::deserialize(&mut deserializer).unwrap();
//!
//! // Verify sharing is preserved
//! assert!(Arc::ptr_eq(&result[0], &result[1]));
//! ```

use std::{collections::HashMap, rc::Rc, sync::Arc};

use crate::{
    de::{Deserialize, Deserializer, EnumAccess, TupleVariantAccess},
    ser::{Serialize, Serializer, TupleVariant},
};

/// Trait for serialization extensions that can track shared pointers.
///
/// This trait enables serializers to maintain state for tracking Arc/Rc
/// instances by their memory addresses, allowing efficient serialization by
/// avoiding duplication.
pub trait SharedPointerSerialize {
    /// Check if an Arc has been seen before and record it if not.
    ///
    /// Returns `true` if this is the first time seeing this Arc (should
    /// serialize owned), `false` if it has been seen before (should
    /// serialize reference).
    fn register_arc<T>(&mut self, arc: &Arc<T>) -> bool;

    /// Check if an Rc has been seen before and record it if not.
    ///
    /// Returns `true` if this is the first time seeing this Rc (should
    /// serialize owned), `false` if it has been seen before (should
    /// serialize reference).
    fn register_rc<T>(&mut self, rc: &Rc<T>) -> bool;
}

/// Trait for deserialization extensions that can reconstruct shared pointers.
///
/// This trait enables deserializers to maintain state for reconstructing Arc/Rc
/// instances from their serialized representations, ensuring proper sharing
/// relationships.
///
/// # Important Note
///
/// This trait assumes that deserialization occurs in the same linear order as
/// serialization. The first encounter of any pointer address must always be
/// an "Owned" variant that stores the actual data. Subsequent "Reference"
/// variants for the same pointer address will look up the previously stored
/// data. If a reference is encountered before its corresponding owned variant,
/// deserialization will fail.
pub trait SharedPointerDeserialize {
    /// Store a deserialized Arc by its pointer address.
    fn store_arc<T: 'static + Send + Sync>(
        &mut self,
        pointer: usize,
        arc: Arc<T>,
    );

    /// Retrieve a previously stored Arc by its pointer address.
    fn get_arc<T: 'static + Send + Sync>(
        &self,
        pointer: usize,
    ) -> Option<Arc<T>>;

    /// Store a deserialized Rc by its pointer address.
    fn store_rc<T: 'static>(&mut self, pointer: usize, rc: Rc<T>);

    /// Retrieve a previously stored Rc by its pointer address.
    fn get_rc<T: 'static>(&self, pointer: usize) -> Option<Rc<T>>;
}

/// Default implementation of shared pointer tracking for serialization.
///
/// This tracks Arc/Rc instances by their memory addresses using HashSets.
#[derive(Debug, Default)]
pub struct SharedPointerTracker {
    /// Set of Arc pointer addresses that have been seen
    arc_pointers: std::collections::HashSet<usize>,
    /// Set of Rc pointer addresses that have been seen
    rc_pointers: std::collections::HashSet<usize>,
}

impl SharedPointerTracker {
    /// Create a new empty tracker.
    pub fn new() -> Self {
        Self {
            arc_pointers: std::collections::HashSet::new(),
            rc_pointers: std::collections::HashSet::new(),
        }
    }
}

impl SharedPointerSerialize for SharedPointerTracker {
    fn register_arc<T>(&mut self, arc: &Arc<T>) -> bool {
        let pointer = Arc::as_ptr(arc) as usize;
        self.arc_pointers.insert(pointer)
    }

    fn register_rc<T>(&mut self, rc: &Rc<T>) -> bool {
        let pointer = Rc::as_ptr(rc) as usize;
        self.rc_pointers.insert(pointer)
    }
}

/// Default implementation of shared pointer reconstruction for deserialization.
///
/// This stores and retrieves Arc/Rc instances by their pointer addresses using
/// efficient unsized coercion to avoid unnecessary heap allocations.
#[derive(Debug, Default)]
pub struct SharedPointerStore {
    /// Map from pointer addresses to stored Arc instances
    /// Using unsized coercion Arc<T> -> Arc<dyn Any + Send + Sync> to avoid
    /// boxing
    arc_store: HashMap<usize, Arc<dyn std::any::Any + Send + Sync>>,
    /// Map from pointer addresses to stored Rc instances  
    /// Using unsized coercion Rc<T> -> Rc<dyn Any> to avoid boxing
    rc_store: HashMap<usize, Rc<dyn std::any::Any>>,
}

impl SharedPointerStore {
    /// Create a new empty store.
    pub fn new() -> Self {
        Self { arc_store: HashMap::new(), rc_store: HashMap::new() }
    }
}

impl SharedPointerDeserialize for SharedPointerStore {
    fn store_arc<T: 'static + Send + Sync>(
        &mut self,
        pointer: usize,
        arc: Arc<T>,
    ) {
        // Use unsized coercion to convert Arc<T> to Arc<dyn Any + Send + Sync>
        let any_arc: Arc<dyn std::any::Any + Send + Sync> = arc;
        self.arc_store.insert(pointer, any_arc);
    }

    fn get_arc<T: 'static + Send + Sync>(
        &self,
        pointer: usize,
    ) -> Option<Arc<T>> {
        let any_arc = self.arc_store.get(&pointer)?.clone();
        Arc::downcast(any_arc).ok()
    }

    fn store_rc<T: 'static>(&mut self, pointer: usize, rc: Rc<T>) {
        // Use unsized coercion to convert Rc<T> to Rc<dyn Any>
        let any_rc: Rc<dyn std::any::Any> = rc;
        self.rc_store.insert(pointer, any_rc);
    }

    fn get_rc<T: 'static>(&self, pointer: usize) -> Option<Rc<T>> {
        let any_rc = self.rc_store.get(&pointer)?.clone();
        Rc::downcast(any_rc).ok()
    }
}

/// Combined extension that implements both serialization and deserialization
/// shared pointer tracking.
#[derive(Debug, Default)]
pub struct SharedPointerExtension {
    /// Tracker for serialization
    pub tracker: SharedPointerTracker,
    /// Store for deserialization
    pub store: SharedPointerStore,
}

impl SharedPointerExtension {
    /// Create a new combined extension.
    pub fn new() -> Self {
        Self {
            tracker: SharedPointerTracker::new(),
            store: SharedPointerStore::new(),
        }
    }
}

impl SharedPointerSerialize for SharedPointerExtension {
    fn register_arc<T>(&mut self, arc: &Arc<T>) -> bool {
        self.tracker.register_arc(arc)
    }

    fn register_rc<T>(&mut self, rc: &Rc<T>) -> bool {
        self.tracker.register_rc(rc)
    }
}

impl SharedPointerDeserialize for SharedPointerExtension {
    fn store_arc<T: 'static + Send + Sync>(
        &mut self,
        pointer: usize,
        arc: Arc<T>,
    ) {
        self.store.store_arc(pointer, arc);
    }

    fn get_arc<T: 'static + Send + Sync>(
        &self,
        pointer: usize,
    ) -> Option<Arc<T>> {
        self.store.get_arc(pointer)
    }

    fn store_rc<T: 'static>(&mut self, pointer: usize, rc: Rc<T>) {
        self.store.store_rc(pointer, rc);
    }

    fn get_rc<T: 'static>(&self, pointer: usize) -> Option<Rc<T>> {
        self.store.get_rc(pointer)
    }
}

// Implement Serialize for Arc<T> when extension supports shared pointers
//
// IMPORTANT: This implementation relies on deterministic, linear serialization
// order to ensure that the first encounter of any Arc instance is serialized
// as "Owned" and subsequent encounters as "Reference". This guarantees that
// during deserialization, owned variants are always processed before their
// corresponding reference variants.
impl<T, S> Serialize<S> for Arc<T>
where
    T: Serialize<S>,
    S: Serializer,
    S::Extension: SharedPointerSerialize,
{
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
        let pointer = Arc::as_ptr(self) as usize;

        if serializer.extension().register_arc(self) {
            // First time seeing this Arc - serialize as Owned variant (tuple
            // variant)
            serializer.emit_tuple_variant(
                "ArcSerialize",
                "Owned",
                0,
                2,
                |mut variant| {
                    variant.serialize_field(&pointer)?;
                    variant.serialize_field(self.as_ref())?;
                    Ok(())
                },
            )
        } else {
            // Already seen this Arc - serialize as Reference variant (tuple
            // variant)
            serializer.emit_tuple_variant(
                "ArcSerialize",
                "Reference",
                1,
                1,
                |mut variant| {
                    variant.serialize_field(&pointer)?;
                    Ok(())
                },
            )
        }
    }
}

// Implement Serialize for Rc<T> when extension supports shared pointers
//
// IMPORTANT: This implementation relies on deterministic, linear serialization
// order to ensure that the first encounter of any Rc instance is serialized
// as "Owned" and subsequent encounters as "Reference". This guarantees that
// during deserialization, owned variants are always processed before their
// corresponding reference variants.
impl<T, S> Serialize<S> for Rc<T>
where
    T: Serialize<S>,
    S: Serializer,
    S::Extension: SharedPointerSerialize,
{
    fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
        let pointer = Rc::as_ptr(self) as usize;

        if serializer.extension().register_rc(self) {
            // First time seeing this Rc - serialize as Owned variant (tuple
            // variant)
            serializer.emit_tuple_variant(
                "RcSerialize",
                "Owned",
                0,
                2,
                |mut variant| {
                    variant.serialize_field(&pointer)?;
                    variant.serialize_field(self.as_ref())?;
                    Ok(())
                },
            )
        } else {
            // Already seen this Rc - serialize as Reference variant (tuple
            // variant)
            serializer.emit_tuple_variant(
                "RcSerialize",
                "Reference",
                1,
                1,
                |mut variant| {
                    variant.serialize_field(&pointer)?;
                    Ok(())
                },
            )
        }
    }
}

// Implement Deserialize for Arc<T> when extension supports shared pointers
impl<T, D> Deserialize<D> for Arc<T>
where
    T: Deserialize<D> + 'static + Send + Sync,
    D: Deserializer,
    D::Extension: SharedPointerDeserialize,
{
    fn deserialize(deserializer: &mut D) -> Result<Self, D::Error> {
        let result =
            deserializer.expect_enum(
                "ArcSerialize",
                &["Owned", "Reference"],
                |variant, enum_access| match variant {
                    crate::de::Identifier::Name("Owned")
                    | crate::de::Identifier::Index(0) => enum_access
                        .tuple_variant(2, |mut tuple_access| {
                            let pointer: usize = tuple_access.next_field()?;
                            let value: T = tuple_access.next_field()?;
                            Ok((pointer, Some(value)))
                        }),
                    crate::de::Identifier::Name("Reference")
                    | crate::de::Identifier::Index(1) => enum_access
                        .tuple_variant(1, |mut tuple_access| {
                            let pointer: usize = tuple_access.next_field()?;
                            Ok((pointer, None))
                        }),
                    _ => {
                        use crate::de::Error;
                        Err(D::Error::custom("Unknown ArcSerialize variant"))
                    }
                },
            )?;

        match result {
            (pointer, Some(value)) => {
                let arc = Arc::new(value);
                deserializer.extension().store_arc(pointer, arc.clone());
                Ok(arc)
            }
            (pointer, None) => {
                deserializer.extension().get_arc(pointer).ok_or_else(|| {
                    use crate::de::Error;
                    D::Error::custom(format!(
                        "Arc reference not found for pointer {:#x}",
                        pointer
                    ))
                })
            }
        }
    }
}

// Implement Deserialize for Rc<T> when extension supports shared pointers
impl<T, D> Deserialize<D> for Rc<T>
where
    T: Deserialize<D> + 'static,
    D: Deserializer,
    D::Extension: SharedPointerDeserialize,
{
    fn deserialize(deserializer: &mut D) -> Result<Self, D::Error> {
        let result =
            deserializer.expect_enum(
                "RcSerialize",
                &["Owned", "Reference"],
                |variant, enum_access| match variant {
                    crate::de::Identifier::Name("Owned")
                    | crate::de::Identifier::Index(0) => enum_access
                        .tuple_variant(2, |mut tuple_access| {
                            let pointer: usize = tuple_access.next_field()?;
                            let value: T = tuple_access.next_field()?;
                            Ok((pointer, Some(value)))
                        }),
                    crate::de::Identifier::Name("Reference")
                    | crate::de::Identifier::Index(1) => enum_access
                        .tuple_variant(1, |mut tuple_access| {
                            let pointer: usize = tuple_access.next_field()?;
                            Ok((pointer, None))
                        }),
                    _ => {
                        use crate::de::Error;
                        Err(D::Error::custom("Unknown RcSerialize variant"))
                    }
                },
            )?;

        match result {
            (pointer, Some(value)) => {
                let rc = Rc::new(value);
                deserializer.extension().store_rc(pointer, rc.clone());
                Ok(rc)
            }
            (pointer, None) => {
                deserializer.extension().get_rc(pointer).ok_or_else(|| {
                    use crate::de::Error;
                    D::Error::custom(format!(
                        "Rc reference not found for pointer {:#x}",
                        pointer
                    ))
                })
            }
        }
    }
}

#[cfg(test)]
mod test;
