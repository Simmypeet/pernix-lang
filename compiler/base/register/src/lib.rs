//! A crate for registering executors and setting-up various query key.

use std::sync::Arc;

pub use inventory::submit;
use pernixc_query::{
    Key,
    runtime::{
        executor::{self, Executor, Registry},
        persistence::{self, Persistence, serde::DynamicRegistry},
    },
};
use pernixc_serialize::{Deserialize, Serialize};

/// The type of the serde registry that will be used for the persistence layer
/// of the query engine.
pub type SerdeRegistry = persistence::serde::SelfRegistry<
    persistence::Serializer,
    persistence::Deserializer,
>;

/// A type containing function pointer to register necessary executors and
/// serialization helpers.
///
/// This struct acts as a global plugin registry via
/// `inventory::collect!(Registration)`. All the registrations will be used
/// by the main compiler driver in order to set up the query engine.
#[derive(Debug, Clone)]
pub struct Registration<
    // this is such a leaky abstraction, the underlying serde registry types
    // are exposed
    R = SerdeRegistry,
> {
    register_executor: fn(&mut executor::Registry),
    register_serde_registry: fn(&mut R),
    skip_persistence: Option<fn(&mut Persistence)>,
    always_recompute: Option<fn(&mut Registry)>,
}

inventory::collect!(Registration);

impl Registration {
    /// Creates a [`Registration`] that registers a single key type [`T`] with
    /// the necessary executors and serialization helpers.
    ///
    /// By default, the value is cached.
    #[must_use]
    pub const fn register_key<
        T: Key
            + Serialize<persistence::Serializer, SerdeRegistry>
            + Deserialize<persistence::Deserializer, SerdeRegistry>,
        E: Executor<T> + Default,
    >(
        skip_persistence: bool,
        always_recompute: bool,
    ) -> Self
    where
        T::Value: Serialize<persistence::Serializer, SerdeRegistry>
            + Deserialize<persistence::Deserializer, SerdeRegistry>,
    {
        Self {
            register_executor: register_executor::<T, E>,
            register_serde_registry: register_serde_registry::<T>,
            skip_persistence: {
                if skip_persistence {
                    Some(crate::skip_persistence::<T>)
                } else {
                    None
                }
            },
            always_recompute: {
                if always_recompute {
                    Some(crate::always_recompute::<T>)
                } else {
                    None
                }
            },
        }
    }

    /// Registers the key without executor specified.
    ///
    /// This typically means that the key's value will be explicitly
    /// specified as an input.
    #[must_use]
    pub const fn register_key_no_executor<
        T: Key
            + Serialize<persistence::Serializer, SerdeRegistry>
            + Deserialize<persistence::Deserializer, SerdeRegistry>,
    >() -> Self
    where
        T::Value: Serialize<persistence::Serializer, SerdeRegistry>
            + Deserialize<persistence::Deserializer, SerdeRegistry>,
    {
        Self {
            register_executor: |_: &mut executor::Registry| {},
            register_serde_registry: register_serde_registry::<T>,
            skip_persistence: None,
            always_recompute: None,
        }
    }

    /// Registers all the `serde` helpers set-up so far.
    pub fn register_serde_registry(serde_registry: &mut SerdeRegistry) {
        for registration in inventory::iter::<Self> {
            (registration.register_serde_registry)(serde_registry);
        }
    }

    /// Registers all the executors set-up so far.
    pub fn register_executor(executor: &mut executor::Registry) {
        for registration in inventory::iter::<Self> {
            (registration.register_executor)(executor);
        }
    }

    /// Registers all the key type to be skipped for persistence.
    pub fn register_skip_persistence(persistence: &mut Persistence) {
        for registration in inventory::iter::<Self> {
            let Some(skip_fn) = registration.skip_persistence else {
                continue;
            };

            skip_fn(persistence);
        }
    }

    /// Registers all the key types that should always recompute their value
    pub fn register_always_recompute(registry: &mut Registry) {
        for registration in inventory::iter::<Self> {
            let Some(recompute_fn) = registration.always_recompute else {
                continue;
            };

            recompute_fn(registry);
        }
    }
}

fn register_executor<T: Key, E: Executor<T> + Default>(
    executor: &mut executor::Registry,
) {
    executor.register::<T, E>(Arc::new(E::default()));
}

fn register_serde_registry<
    T: Key
        + Serialize<persistence::Serializer, SerdeRegistry>
        + Deserialize<persistence::Deserializer, SerdeRegistry>,
>(
    registry: &mut SerdeRegistry,
) where
    T::Value: Serialize<persistence::Serializer, SerdeRegistry>
        + Deserialize<persistence::Deserializer, SerdeRegistry>,
{
    registry.register::<T>();
}

fn skip_persistence<T: Key>(persistence: &mut Persistence) {
    persistence.skip_cache_value::<T>();
}

fn always_recompute<T: Key>(registtry: &mut Registry) {
    registtry.set_always_recompute::<T>(true);
}

/// Registers the key and executor for the main compiler driver.
#[macro_export]
macro_rules! register {
    ($key:ty) => {
        $crate::submit! {
            $crate::Registration::register_key_no_executor::<$key>()
        }
    };

    ($key:ty, $executor:ty) => {
        $crate::submit! {
            $crate::Registration::register_key::<$key, $executor>(false, false)
        }
    };

    ($key:ty, $executor:ty, skip_cache) => {
        $crate::submit! {
            $crate::Registration::register_key::<$key, $executor>(true, false)
        }
    };

    ($key:ty, $executor:ty, skip_cache, always_recompute) => {
        $crate::submit! {
            $crate::Registration::register_key::<$key, $executor>(true, true)
        }
    };

    ($key:ty, $executor:ty, always_recompute) => {
        $crate::submit! {
            $crate::Registration::register_key::<$key, $executor>(false, true)
        }
    };
}
