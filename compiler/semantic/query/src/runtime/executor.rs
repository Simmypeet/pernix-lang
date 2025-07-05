//! Contains the [`Executor`] trait and methods to register and manage executors

use std::{
    any::{Any, TypeId},
    sync::Arc,
};

use dashmap::mapref::one::Ref;
use pernixc_hash::DashMap;

use crate::{
    database::{self, TrackedEngine},
    Engine, Key,
};

/// A unit struct for signaling cyclic dependencies in query execution.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    fn execute(
        &self,
        engine: &mut TrackedEngine,
        key: &K,
    ) -> Result<Arc<K::Value>, CyclicError>;
}

fn invoke_executor<'db, E: Executor<K> + 'static, K: Key + 'static>(
    key: &'db dyn Any,
    executor: &'db dyn Any,
    engine: &'db mut TrackedEngine,
) -> Result<Arc<dyn database::Value>, CyclicError> {
    let key = key.downcast_ref::<K>().expect("Key type mismatch");
    let executor =
        executor.downcast_ref::<E>().expect("Executor type mismatch");

    executor.execute(engine, key).map(|x| x as Arc<dyn database::Value>)
}

type InvokeExecutorFn =
    for<'key, 'ex, 'eng> fn(
        key: &'key dyn Any,
        executor: &'ex dyn Any,
        engine: &'eng mut TrackedEngine,
    )
        -> Result<Arc<dyn database::Value>, CyclicError>;

type ReVerifyQueryFn = for<'db, 'call> fn(
    engine: &'db Engine,
    key: &'db dyn Any,
    current_version: u64,
    call_stack: &'call mut Vec<database::DynamicKey>,
) -> Result<(), CyclicError>;

/// Contains the [`Executor`] objects for each key type. This struct allows
/// registering and retrieving executors for different query key types.
#[derive(Debug, Default)]
pub struct Registry {
    executors_by_key_type_id: DashMap<TypeId, Entry>,
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

    /// Retrieves the executor for the given key type [`K`]. If no executor
    /// is registered for the key type, it returns `None`.
    pub(crate) fn get_entry<K: Key>(&self) -> Option<Ref<TypeId, Entry>> {
        self.executors_by_key_type_id.get(&TypeId::of::<K>())
    }

    /// Retrieves the executor for the given key type by its [`TypeId`].
    pub(crate) fn get_entry_with_id(
        &self,
        type_id: TypeId,
    ) -> Option<Ref<TypeId, Entry>> {
        self.executors_by_key_type_id.get(&type_id)
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Entry {
    executor: Arc<dyn Any + Send + Sync>,
    invoke_executor: InvokeExecutorFn,
    re_verify_query: ReVerifyQueryFn,
}

impl Entry {
    fn new<K: Key, E: Executor<K>>(executor: Arc<E>) -> Self {
        Self {
            executor: executor as Arc<dyn Any + Send + Sync>,
            invoke_executor: invoke_executor::<E, K>,
            re_verify_query: database::re_verify_query::<K>,
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
