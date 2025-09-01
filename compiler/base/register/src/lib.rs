//! A crate for registering executors and setting-up various query key.

use std::sync::Arc;

pub use inventory::submit;
use pernixc_query::{
    runtime::{
        executor::{self, Executor},
        persistence::{self, serde::DynamicRegistry, Persistence},
    },
    Key,
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
    skip_persistence: fn(&mut Persistence),
    skip_persistence_toggle: bool,
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
    >() -> Self
    where
        T::Value: Serialize<persistence::Serializer, SerdeRegistry>
            + Deserialize<persistence::Deserializer, SerdeRegistry>,
    {
        Self {
            register_executor: register_executor::<T, E>,
            register_serde_registry: register_serde_registry::<T>,
            skip_persistence: skip_persistence::<T>,
            skip_persistence_toggle: false,
        }
    }

    /// Registers the helper with a flag to skip persistence.
    #[must_use]
    pub const fn skip_persistence(mut self) -> Self {
        self.skip_persistence_toggle = true;
        self
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
            (registration.skip_persistence)(persistence);
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

/// Registers the key and executor for the main compiler driver.
#[macro_export]
macro_rules! register {
    ($key:ty, $executor:ty) => {
        $crate::submit! {
            $crate::Registration::register_key::<$key, $executor>()
        }
    };

    ($key:ty, $executor:ty, skip_cache) => {
        $crate::submit! {
            $crate::Registration::register_key::<$key, $executor>()
                .skip_persistence()
        }
    };
}
