//! Contains the [`Executor`] trait and methods to register and manage executors

use std::{
    any::{Any, TypeId},
    collections::HashMap,
    sync::Arc,
};

use parking_lot::MutexGuard;

use crate::{
    database::call_graph::CallGraph,
    key::{Dynamic, Key},
    Engine,
};

/// A unit struct for signaling cyclic dependencies in query execution.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CyclicError;

/// Implements by the query executors to compute the value of the given key.
pub trait Executor<K: Key>: Any + Send + Sync {
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
    /// Moreover, the executor **MUST NOT** spawn any threads since the query
    /// system internally uses a thread id to keep track of the query graph
    /// and the dependencies. Spawning threads will break the query dependency
    /// tracking and will lead to undefined behavior. It worths mentioning that
    /// the query system does distribute the work across multiple threads at
    /// very higher-level. So the executor should not worry about that.
    ///
    /// # Returns
    ///
    /// Returns `Ok(value)` on successful computation, or `Err(CyclicError)`
    /// when the query is part of a strongly connected component (SCC) with
    /// cyclic dependencies.
    fn execute(&self, engine: &Engine, key: K)
        -> Result<K::Value, CyclicError>;
}

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
    ) -> Option<Arc<dyn Executor<K>>> {
        self.executors_by_key_type_id
            .insert(TypeId::of::<K>(), Entry::new(executor))
            .map(Entry::downcast::<K>)
    }

    /// Retrieves the executor for the given [`K`] key type. If no executor
    /// exists for the given key type, it will return `None`.
    #[must_use]
    pub fn get<K: Key>(&self) -> Option<Arc<dyn Executor<K>>> {
        self.executors_by_key_type_id
            .get(&TypeId::of::<K>())
            .map(|entry| entry.clone().downcast::<K>())
    }

    pub(crate) fn get_invoke_query(
        &self,
        type_id: &TypeId,
    ) -> Option<InvokeQuery> {
        self.executors_by_key_type_id.get(type_id).map(|entry| entry.invoke)
    }
}

type ExecutorArcDowncast = fn(Arc<dyn Any + Send + Sync>, &mut dyn Any);
type InvokeQuery = for<'db, 'k> fn(
    &'db Engine,
    &'k dyn Dynamic,
    MutexGuard<'db, CallGraph>,
) -> MutexGuard<'db, CallGraph>;

#[derive(Clone)]
struct Entry {
    executor: Arc<dyn Any + Send + Sync>,
    downcast: ExecutorArcDowncast,
    invoke: InvokeQuery,
}

impl std::fmt::Debug for Entry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Entry")
            .field("executor", &self.executor)
            .finish_non_exhaustive()
    }
}

impl Entry {
    fn new<K: Key, E: Executor<K>>(executor: Arc<E>) -> Self {
        fn invoke<'db, K: Key>(
            engine: &'db Engine,
            key: &dyn Dynamic,
            call_graph: MutexGuard<'db, CallGraph>,
        ) -> MutexGuard<'db, CallGraph> {
            let key = key.any().downcast_ref::<K>().unwrap();

            engine.query_internal(key, call_graph).1
        }

        let downcast = |executor: Arc<dyn Any + Send + Sync>,
                        target: &mut dyn Any| {
            let target =
                target.downcast_mut::<Option<Arc<dyn Executor<K>>>>().unwrap();

            let executor = Arc::downcast::<E>(executor).unwrap();
            *target = Some(executor);
        };

        // skipcq: RS-W1026 false positive
        Self { executor, downcast, invoke: invoke::<K> }
    }

    fn downcast<K: Key>(self) -> Arc<dyn Executor<K>> {
        let mut target: Option<Arc<dyn Executor<K>>> = None;
        (self.downcast)(self.executor, &mut target);
        target.unwrap()
    }
}

// #[cfg(test)]
// mod test;
