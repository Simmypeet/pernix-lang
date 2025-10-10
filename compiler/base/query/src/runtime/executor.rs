//! Contains the [`Executor`] trait and methods to register and manage executors

use std::{
    any::{Any, TypeId},
    pin::Pin,
    sync::Arc,
};

use pernixc_hash::HashMap;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::{
    database::{self, Dynamic, DynamicValue, TrackedEngine},
    Engine, Key,
};

/// A unit struct for signaling cyclic dependencies in query execution.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    thiserror::Error,
)]
#[error("cyclic dependency in the query system detected")]
pub struct CyclicError;

/// A super trait representing a future returned by an executor.
pub trait Future<'a, V: 'static>:
    std::future::Future<Output = Result<V, CyclicError>> + Send + 'a
{
}

impl<'a, F, V> Future<'a, V> for F
where
    F: std::future::Future<Output = Result<V, CyclicError>> + Send + 'a,
    V: 'static,
{
}

/// Implements by the query executors to compute the value of the given key.
pub trait Executor<K: Key>: Any + Send + Sync + std::fmt::Debug {
    /// If `true`, when the query's version needs to be verified, the query
    /// engine will always re-compute the value by invoking the executor
    /// again.
    ///
    /// This can be beneficial for executors that interact with external state
    /// such as file system or network, where the value may change outside
    /// the control of the query system.
    ///
    /// Defaults to `false`.
    const ALWAYS_RECOMPUTE: bool = false;

    /// Computes the result [`K::Value`] for the given [`K`] key.
    ///
    /// Got invoked when the key query is requested to the database and the
    /// value needs to be computed.
    ///
    /// # Remarks
    ///
    /// It's recommended that the executor is stateless and this method is
    /// a pure function. This allows the query system to cache the result.
    ///
    /// # Returns
    ///
    /// Returns `Ok(value)` on successful computation, or `Err(CyclicError)`
    /// when the query is part of a strongly connected component (SCC) with
    /// cyclic dependencies.
    fn execute<'s, 't, 'k>(
        &'s self,
        engine: &'t TrackedEngine,
        key: &'k K,
    ) -> impl std::future::Future<Output = Result<K::Value, CyclicError>>
           + Send
           + use<'s, 't, 'k, Self, K>;
}

fn invoke_executor<'a, E: Executor<K> + 'static, K: Key + 'static>(
    key: &'a dyn Any,
    executor: &'a dyn Any,
    engine: &'a mut TrackedEngine,
) -> Pin<Box<dyn Future<'a, DynamicValue> + 'a>> {
    let key = key.downcast_ref::<K>().expect("Key type mismatch");
    let executor =
        executor.downcast_ref::<E>().expect("Executor type mismatch");

    Box::pin(async {
        executor.execute(engine, key).await.map(|x| {
            let smallbox: DynamicValue = smallbox::smallbox!(x);

            smallbox
        })
    })
}

type InvokeExecutorFn =
    for<'a> fn(
        key: &'a dyn Any,
        executor: &'a dyn Any,
        engine: &'a mut TrackedEngine,
    ) -> Pin<Box<dyn Future<'a, DynamicValue> + 'a>>;

type ReVerifyQueryFn = for<'a> fn(
    engine: &'a Arc<Engine>,
    key: &'a dyn Any,
    current_version: u64,
    called_from: &'a dyn Dynamic,
) -> Pin<Box<dyn Future<'a, ()> + 'a>>;

/// Contains the [`Executor`] objects for each key type. This struct allows
/// registering and retrieving executors for different query key types.
#[derive(Debug, Default)]
pub struct Registry {
    executors_by_key_type_id: HashMap<TypeId, Entry>,
}

impl Registry {
    /// Registers a new executor for the given [`K`] key type. If an executor
    /// for the given key type already exists, it will be replaced with the new
    /// one and the old one will be returned.
    pub fn register<K: Key, E: Executor<K>>(
        &mut self,
        executor: Arc<E>,
    ) -> Option<Arc<dyn Any + Send + Sync>> {
        self.executors_by_key_type_id
            .insert(TypeId::of::<K>(), Entry::new(executor))
            .map(|entry| entry.executor)
    }

    pub(crate) fn executor_always_recompute<K: Key>(&self) -> Option<bool> {
        self.executors_by_key_type_id
            .get(&TypeId::of::<K>())
            .map(|entry| entry.always_recompute)
    }

    /// Retrieves the executor for the given key type [`K`]. If no executor
    /// is registered for the key type, it returns `None`.
    pub(crate) fn get_entry<K: Key>(&self) -> Option<&Entry> {
        self.executors_by_key_type_id.get(&TypeId::of::<K>())
    }

    /// Retrieves the executor for the given key type by its [`TypeId`].
    pub(crate) fn get_entry_with_id(&self, type_id: TypeId) -> Option<&Entry> {
        self.executors_by_key_type_id.get(&type_id)
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Entry {
    executor: Arc<dyn Any + Send + Sync>,
    invoke_executor: InvokeExecutorFn,
    re_verify_query: ReVerifyQueryFn,
    always_recompute: bool,
}

impl Entry {
    fn new<K: Key, E: Executor<K>>(executor: Arc<E>) -> Self {
        Self {
            executor: executor as Arc<dyn Any + Send + Sync>,
            invoke_executor: invoke_executor::<E, K>,
            re_verify_query: database::re_verify_query::<K>,
            always_recompute: E::ALWAYS_RECOMPUTE,
        }
    }

    /// Returns the executor for the given key type.
    pub fn get_any_executor(&self) -> Arc<dyn Any + Send + Sync> {
        self.executor.clone()
    }

    /// Returns the executor function for the given key type.
    pub fn get_invoke_executor(&self) -> InvokeExecutorFn {
        self.invoke_executor
    }

    /// Returns the re-verify query function for the given key type.
    pub fn get_re_verify_query(&self) -> ReVerifyQueryFn {
        self.re_verify_query
    }
}
