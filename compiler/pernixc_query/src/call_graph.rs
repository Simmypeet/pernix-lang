//! Implements the [`CallGraph`] struct used to track the dependencies between
//! queries.

use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    sync::Arc,
    thread::ThreadId,
};

use enum_as_inner::EnumAsInner;
use parking_lot::{Condvar, MutexGuard};

use crate::{
    executor::Executor,
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
        HashMap<SmallBox<dyn Dynamic>, HashSet<SmallBox<dyn Dynamic>>>,

    version_info_by_keys: HashMap<SmallBox<dyn Dynamic>, VersionInfo>,

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

/// Stores the information about the version of a query result used to track
/// the validity of the result.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VersionInfo {
    /// The version when the value was computed.
    updated_at_version: usize,

    /// The latest version that the value was verified against its result from
    /// the latest computation.
    verfied_at_version: usize,

    kind: Kind,
}

/// An enumeration storing the information about the value of a query.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
pub enum Kind {
    /// The value is an `input` value, explicitly set by the user.
    Input,

    /// The value is a `derived` value, computed from other values.
    Derived {
        /// Whether the value was choosen by `Default` because of the cyclic
        /// dependency error; this will mark the value as invalid and there
        /// will always be an attempt to recompute the value
        defaulted_by_cyclic_dependency: bool,
    },
}

impl Database {
    /// Sets the input value for the given key.
    ///
    /// If this call happens after one of the derived values has been computed,
    /// the version of the database will be bumped up by one. This is to
    /// indicate that the input value has changed and all the derived values
    /// need to reflect this change.
    pub fn set_input<K: Key + Dynamic>(&mut self, key: &K, value: K::Value) {
        // bump the version for the new input setting
        if *self.last_was_query.get_mut() {
            self.version += 1;
        }

        *self.last_was_query.get_mut() = false;

        let mut invalidate = false;
        if let Some(old_value) = self.map.get(key) {
            invalidate = old_value != value;
        }

        // insert the value into the map
        self.map.insert(key.clone(), value);

        // set the input value
        match self
            .call_graph
            .get_mut()
            .version_info_by_keys
            .entry(key.smallbox_clone())
        {
            Entry::Occupied(mut occupied_entry) => {
                let value = occupied_entry.get_mut();
                value.verfied_at_version = self.version;

                // update the version info if invalidated
                if invalidate {
                    value.updated_at_version = self.version;
                }
            }
            Entry::Vacant(entry) => {
                entry.insert(VersionInfo {
                    updated_at_version: self.version,
                    verfied_at_version: self.version,
                    kind: Kind::Input,
                });
            }
        }
    }

    /// Queries the value associated with the given key.
    ///
    /// # Panics
    ///
    /// If the key doesn't have a corresponding [`Executor`] registered in the
    /// database.
    pub fn query<T: Dynamic + Key>(&self, key: &T) -> T::Value {
        self.query_internal(key, self.call_graph.lock()).0
    }

    pub(super) fn query_internal<'a, T: Dynamic + Key>(
        &'a self,
        key: &T,
        mut call_graph: MutexGuard<'a, CallGraph>,
    ) -> (T::Value, MutexGuard<'a, CallGraph>) {
        let key_smallbox = key.smallbox_clone();

        // query has already been computed return the result
        let Some(mut result) = self.map.get(key) else {
            self.last_was_query
                .store(true, std::sync::atomic::Ordering::SeqCst);

            // the value hasn't been computed yet, so we need to compute it
            let (_, call_graph) =
                self.fresh_query(key, &key_smallbox, call_graph);

            return (self.map.get(key).unwrap(), call_graph);
        };

        // the result is already up to date
        let version_info =
            *call_graph.version_info_by_keys.get(&key_smallbox).unwrap();

        if version_info.kind == Kind::Input {
            let current_thread_id = std::thread::current().id();
            let called_from = call_graph
                .record_stacks_by_thread_id
                .get(&current_thread_id)
                .and_then(|x| x.last().map(|x| x.smallbox_clone()));

            // add the dependency of the input
            if let Some(called_from) = &called_from {
                call_graph
                    .dependency_graph
                    .get_mut(called_from)
                    .unwrap()
                    .insert(key_smallbox);
            }

            // the value is an `input` value, always returns as it.
            return (result, call_graph);
        }

        self.last_was_query.store(true, std::sync::atomic::Ordering::SeqCst);

        if version_info.verfied_at_version == self.version {
            return (result, call_graph);
        }

        let inputs = call_graph
            .dependency_graph
            .get(&key_smallbox)
            .unwrap()
            .iter()
            .map(|x| x.smallbox_clone())
            .collect::<Vec<_>>();

        let mut recompute = false;
        for dep in inputs {
            // run inputs verification for the input as well
            let is_input =
                call_graph.version_info_by_keys.get(&dep).unwrap().kind
                    == Kind::Input;

            if !is_input {
                let invoke_fn = self
                    .get_invoke_query(&dep.any().type_id())
                    .unwrap_or_else(|| {
                        panic!(
                            "no executor registered for key type `{}`",
                            dep.unique_type_name()
                        )
                    });

                call_graph = invoke_fn(self, &*dep, call_graph);
            }

            // check if there's need to recompute the value
            if !recompute {
                let input_version_info =
                    call_graph.version_info_by_keys.get(&dep).unwrap();

                recompute |= (input_version_info.updated_at_version
                    > version_info.verfied_at_version)
                    || input_version_info.kind.as_derived().is_some_and(|x| *x);
            }
        }

        if recompute {
            // recompute the value
            let (_, returned_call_graph) =
                self.fresh_query(key, &key_smallbox, call_graph);

            call_graph = returned_call_graph;

            // update the result, as it might have been changed
            result = self.map.get(key).unwrap();
        } else {
            call_graph
                .version_info_by_keys
                .get_mut(&key_smallbox)
                .unwrap()
                .verfied_at_version = self.version;
        }

        (result, call_graph)
    }

    fn fresh_query<'a, T: Dynamic + Key>(
        &'a self,
        key: &T,
        key_smallbox: &SmallBox<dyn Dynamic>,
        mut call_graph: MutexGuard<'a, CallGraph>,
    ) -> (bool, MutexGuard<'a, CallGraph>) {
        call_graph
            .dependency_graph
            .insert(key.smallbox_clone(), HashSet::new());
        let executor = self.get_executor::<T>().unwrap_or_else(|| {
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
            call_graph
                .dependency_graph
                .get_mut(called_from)
                .unwrap()
                .insert(key.smallbox_clone());

            // check if `target_record` can go to `called_from`
            let mut stack = vec![key.smallbox_clone()];

            loop {
                if stack.last().unwrap() == called_from {
                    call_graph
                        .cyclic_dependencies
                        .push(CyclicDependency { records_stack: stack });

                    // insert a default value for the cyclic dependency
                    self.map
                        .insert(key.clone(), <T::Value as Default>::default());

                    call_graph.version_info_by_keys.insert(
                        key.smallbox_clone(),
                        VersionInfo {
                            updated_at_version: self.version,
                            verfied_at_version: self.version,
                            kind: Kind::Derived {
                                defaulted_by_cyclic_dependency: true,
                            },
                        },
                    );

                    // signifying that the value was defaulted
                    return (false, call_graph);
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

        let sync = call_graph.condvars_by_record.get(key_smallbox).cloned();

        // there's an another thread that is computing the same record
        if let Some(sync) = sync {
            sync.wait(&mut call_graph);
        } else {
            call_graph =
                self.compute(key, key_smallbox, call_graph, &*executor);
        }

        assert!(
            call_graph
                .record_stacks_by_thread_id
                .get_mut(&current_thread_id)
                .unwrap()
                .pop()
                .unwrap()
                == *key_smallbox,
        );

        if let Some(called_from) = called_from {
            assert!(call_graph
                .current_dependencies_by_dependant
                .remove(&called_from)
                .is_some());
        }

        (true, call_graph)
    }

    fn compute<'a, K: Key + Dynamic>(
        &'a self,
        key: &K,
        key_smallbox: &SmallBox<dyn Dynamic>,
        mut call_graph: MutexGuard<'a, CallGraph>,
        executor: &dyn Executor<K>,
    ) -> MutexGuard<'a, CallGraph> {
        // compute the component
        let sync = Arc::new(Condvar::new());
        assert!(call_graph
            .condvars_by_record
            .insert(key_smallbox.smallbox_clone(), sync.clone())
            .is_none());

        // skipcq: RS-E1021 false positive
        drop(call_graph); // release the context lock

        let value = executor.execute(self, key.clone());

        // re-acquire the context lock
        call_graph = self.call_graph.lock();

        self.map.insert(key.clone(), value);
        call_graph.version_info_by_keys.insert(
            key_smallbox.smallbox_clone(),
            VersionInfo {
                updated_at_version: self.version,
                verfied_at_version: self.version,
                kind: Kind::Derived { defaulted_by_cyclic_dependency: false },
            },
        );

        assert!(call_graph.condvars_by_record.remove(key_smallbox).is_some());

        // notify the other threads that the component is computed
        sync.notify_all();

        call_graph
    }
}

#[cfg(test)]
mod test;
