//! Contains multiple important traits used by the query system

use std::{any::Any, fmt::Debug, hash::Hash, sync::Arc};

use pernixc_arena::ID;
use pernixc_stable_hash::StableHash;
use pernixc_stable_type_id::{Identifiable, StableTypeID};
use pernixc_target::Global;

use crate::fingerprint;

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
    'static
    + Send
    + Sync
    + Eq
    + Clone
    + std::hash::Hash
    + Identifiable
    + StableHash
    + Debug
{
    /// The corresponding value type for this key
    type Value: 'static + Send + Sync + Debug + StableHash;

    /// A value returned by the key when the key is a part of a strongly
    /// connected component (SCC) in the cyclic dependencies.
    ///
    /// By default, this method panics, as the most queries are not supposed
    /// to allow cyclic dependencies.
    #[must_use]
    fn scc_value() -> Arc<Self::Value> {
        panic!(
            "SCC `{}` value for cyclic dependencies is not defined",
            std::any::type_name::<Self>()
        )
    }
}
