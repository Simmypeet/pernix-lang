//! Contains multiple important traits used by the query system

use std::hash::Hash;

use pernixc_arena::ID;
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
pub trait Key: 'static + Send + Sync + Eq + Clone + std::hash::Hash {
    /// The corresponding value type for this key
    type Value: 'static + Send + Sync + Clone + Default + Eq;

    /// Gets the stable unique type name of the key.
    fn unique_type_name() -> &'static str;
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
    fn unique_type_name(&self) -> &'static str;
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

    fn unique_type_name(&self) -> &'static str { K::unique_type_name() }
}

impl std::fmt::Debug for dyn Dynamic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Dynamic")
            .field(&self.unique_type_name())
            .finish_non_exhaustive()
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
