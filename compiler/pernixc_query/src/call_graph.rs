//! Implements the [`CallGraph`] struct used to track the dependencies between
//! queries.

use std::{collections::HashMap, sync::Arc, thread::ThreadId};

use parking_lot::Condvar;
use pernixc_abort::Abort;

use crate::{
    key::{Dynamic, Key, SmallBox},
    Database,
};

/// Tracks the dependencies between queries and their execution order.
#[derive(Default)]
pub struct CallGraph {
    // emulating a call stack of particular thread
    record_stacks_by_thread_id: HashMap<ThreadId, Vec<SmallBox<dyn Dynamic>>>,

    // the condition variables used to notify the threads that are waiting for
    // the completion of a particular record
    condvars_by_record: HashMap<SmallBox<dyn Dynamic>, Arc<Condvar>>,

    current_dependencies_by_dependant:
        HashMap<SmallBox<dyn Dynamic>, SmallBox<dyn Dynamic>>,

    dependency_graph:
        HashMap<SmallBox<dyn Dynamic>, Vec<SmallBox<dyn Dynamic>>>,

    cyclic_dependencies: Vec<CyclicDependency>,
}

/// Stores the error information about a cyclic dependency in the call graph.
#[derive(Debug)]
pub struct CyclicDependency {
    /// The stack of records that caused the cyclic dependency.
    pub records_stack: Vec<SmallBox<dyn Dynamic>>,
}

impl std::fmt::Debug for CallGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CallGraph").finish_non_exhaustive()
    }
}

impl Database {
    /// Queries the value associated with the given key.
    ///
    /// # Panics
    ///
    /// If the key doesn't have a corresponding [`Executor`] registered in the
    /// database.
    pub fn query<T: Dynamic + Key>(self, key: &T) -> Result<T::Value, Abort> {
        let key_smallbox = key.smallbox_clone();
        let mut call_graph = self.call_graph.lock();

        // query has already been computed return the result
        if let Some(result) = self.map.get(key) {
            return Ok(result);
        }

        let executor = self.executor_registry.get::<T>().unwrap_or_else(|| {
            panic!(
                "no executor registered for key type {}",
                std::any::type_name::<T>()
            )
        });

        let current_thread_id = std::thread::current().id();
        let called_from = call_graph
            .record_stacks_by_thread_id
            .get(&current_thread_id)
            .and_then(|x| x.last().map(|x| x.smallbox_clone()));

        // check for cyclic dependencies
        if let Some(called_from) = &called_from {
            // check if `target_record` can go to `called_from`
            let mut stack = vec![key.smallbox_clone()];

            loop {
                if stack.last().unwrap() == called_from {
                    call_graph
                        .cyclic_dependencies
                        .push(CyclicDependency { records_stack: stack });

                    return Err(Abort);
                }

                // follow the dependency chain
                if let Some(next) = call_graph
                    .current_dependencies_by_dependant
                    .get(stack.last().unwrap())
                {
                    stack.push(next.smallbox_clone());
                } else {
                    break;
                }
            }

            assert!(call_graph
                .current_dependencies_by_dependant
                .insert(called_from.smallbox_clone(), key.smallbox_clone())
                .is_none());
        }

        // add the current record to the call stack
        call_graph
            .record_stacks_by_thread_id
            .entry(current_thread_id)
            .or_default()
            .push(key.smallbox_clone());

        let sync = call_graph.condvars_by_record.get(&key_smallbox).cloned();

        // there's an another thread that is computing the same record
        if let Some(sync) = sync {
            sync.wait(&mut call_graph);
        } else {
            // compute the component
            let sync = Arc::new(Condvar::new());
            assert!(call_graph
                .condvars_by_record
                .insert(key_smallbox.smallbox_clone(), sync.clone())
                .is_none());

            drop(call_graph); // release the context lock

            let value = executor.execute(key.clone());

            // re-acquire the context lock
            call_graph = self.call_graph.lock();

            assert!(self.map.insert(key.clone(), value).is_none());

            assert!(call_graph
                .condvars_by_record
                .remove(&key_smallbox)
                .is_some());

            // notify the other threads that the component is computed
            sync.notify_all();
        }

        assert!(
            call_graph
                .record_stacks_by_thread_id
                .get_mut(&current_thread_id)
                .unwrap()
                .pop()
                .unwrap()
                == key_smallbox,
        );

        if let Some(called_from) = called_from {
            assert!(call_graph
                .current_dependencies_by_dependant
                .remove(&called_from)
                .is_some());
        }

        Ok(self.map.get(key).unwrap())
    }
}
