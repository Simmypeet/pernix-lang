//! Contains multiple important traits used by the query system

use std::hash::Hash;

use pernixc_arena::ID;
use pernixc_stable_type_id::{Identifiable, StableTypeID};
use pernixc_target::Global;

/// A tag struct used for signifying that a key is an input key.
///
/// An input key is a key that has the value explicitly set by the user and
/// can be thought of the starting point of the whole query system.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Input;

/// A trait representing a key that can be used to store and retrieve values
/// inside a [`map::Map`].
///
/// To implement this trait, use the [`Key`] derive macro. This will
/// automatically implement the [`Key`] trait for your type. The derive macro
/// will also generate a unique type name for the key.
///
/// ``` ignore
/// #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Key)]
/// #[value(MyValue)]
/// pub struct MyKey;
/// ```
pub trait Key:
    'static + Send + Sync + Eq + Clone + std::hash::Hash + Identifiable
{
    /// The corresponding value type for this key
    type Value: 'static + Send + Sync + Clone + Default + Eq;

    /// Merges a new value with an existing value for this key.
    ///
    /// This method is called when attempting to set a value for a key that
    /// already has a value in the database. The default implementation
    /// performs an equality check and returns an error if the values
    /// differ, ensuring data consistency.
    ///
    /// # Arguments
    ///
    /// * `old` - A mutable reference to the existing value that will be updated
    /// * `new` - The new value to merge with the existing one
    ///
    /// # Returns
    ///
    /// * `Ok(())` if the merge was successful (values are equal in the default
    ///   implementation)
    /// * `Err(String)` if the merge failed due to incompatible values, with an
    ///   error message
    ///
    /// # Default Behavior
    ///
    /// The default implementation compares the old and new values for equality:
    /// - If they are equal, the merge succeeds without modifying the old value
    /// - If they differ, an error is returned indicating incompatible values
    ///
    /// # Custom Implementations
    ///
    /// Types can override this method to implement custom merge logic, such as:
    /// - Combining numeric values (e.g., summing, taking maximum)
    /// - Merging collections (e.g., union of sets, concatenation of lists)
    /// - Applying domain-specific merge strategies
    ///
    /// # Examples
    ///
    /// ```ignore
    /// // Default behavior - values must be equal
    /// let mut old_value = 42;
    /// assert!(MyKey::merge_value(&mut old_value, 42).is_ok());
    /// assert!(MyKey::merge_value(&mut old_value, 24).is_err());
    ///
    /// // Custom implementation for additive merging
    /// impl Key for AdditiveKey {
    ///     type Value = i32;
    ///
    ///     fn merge_value(old: &mut Self::Value, new: Self::Value) -> Result<(), String> {
    ///         *old += new;
    ///         Ok(())
    ///     }
    /// }
    /// ```
    fn merge_value(
        old: &mut Self::Value,
        new: Self::Value,
    ) -> Result<bool, String> {
        if old == &new {
            Ok(true)
        } else {
            Err(format!(
                "Encountered an incompatible value for key {}",
                std::any::type_name::<Self>()
            ))
        }
    }
}

/// A type alias for a [`smallbox::SmallBox`] with a [`Global<ID<()>>`] as its
/// size for the local storage.
///
/// This smallbox is used mainly for performance reasons to avoid heap
/// allocation (premature optimization?). Since most of the queries are just
/// global IDs, the [`Global<ID<()>>`] should be enough to store the data
/// without allocating a heap object.
pub type SmallBox<T> = smallbox::SmallBox<T, Global<ID<()>>>;

/// A trait allowing store multiple types of as a key in a hashmap. This is
/// automatically implemented for all types that implement the [`Key`] trait.
#[doc(hidden)]
pub trait Dynamic {
    #[doc(hidden)]
    fn any(&self) -> &dyn std::any::Any;
    #[doc(hidden)]
    fn eq(&self, other: &dyn Dynamic) -> bool;
    #[doc(hidden)]
    fn hash(&self, state: &mut dyn std::hash::Hasher);
    #[doc(hidden)]
    fn smallbox_clone(&self) -> SmallBox<dyn Dynamic>;
    #[doc(hidden)]
    fn stable_type_id(&self) -> StableTypeID;
    #[doc(hidden)]
    fn type_name(&self) -> &'static str;
}

/// A new type wrapper around [`SmallBox<dyn Dynamic>`] that allows it to be
/// serializable and deserializable.
#[derive(
    Debug, PartialEq, Eq, Hash, derive_more::Deref, derive_more::DerefMut,
)]
pub struct DynamicBox(pub SmallBox<dyn Dynamic>);

impl Clone for DynamicBox {
    fn clone(&self) -> Self { Self(self.0.smallbox_clone()) }
}

impl<K: Key> Dynamic for K {
    fn any(&self) -> &dyn std::any::Any { self as &dyn std::any::Any }

    fn eq(&self, other: &dyn Dynamic) -> bool {
        other.any().downcast_ref::<Self>().is_some_and(|other| self.eq(other))
    }

    fn hash(&self, mut state: &mut dyn std::hash::Hasher) {
        let id = std::any::TypeId::of::<Self>();

        id.hash(&mut state);
        Hash::hash(self, &mut state);
    }

    fn smallbox_clone(&self) -> SmallBox<dyn Dynamic> {
        smallbox::smallbox!(self.clone())
    }

    fn stable_type_id(&self) -> StableTypeID { Self::STABLE_TYPE_ID }

    fn type_name(&self) -> &'static str { std::any::type_name::<Self>() }
}

impl std::fmt::Debug for dyn Dynamic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Dynamic").field(&self.any()).finish_non_exhaustive()
    }
}

impl PartialEq for dyn Dynamic {
    fn eq(&self, other: &Self) -> bool { Dynamic::eq(self, other) }
}

impl Eq for dyn Dynamic {}

impl Hash for dyn Dynamic {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Dynamic::hash(self, state);
    }
}
