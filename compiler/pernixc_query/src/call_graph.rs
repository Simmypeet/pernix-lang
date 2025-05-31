#![allow(clippy::mutable_key_type)]

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
    key::{Dynamic, DynamicBox, Key},
    runtime::executor::Executor,
    Database,
};

/// Tracks the dependencies between queries and their execution order.
#[derive(Default, serde::Serialize, serde::Deserialize)]
#[allow(clippy::mutable_key_type)]
pub struct CallGraph {
    // emulating a call stack of particular thread
    #[serde(skip)] // ThreadId is not serializable
    record_stacks_by_thread_id: HashMap<ThreadId, Vec<DynamicBox>>,

    // the condition variables used to notify the threads that are waiting for
    // the completion of a particular record
    #[serde(skip)] // Arc<Condvar> is not serializable
    condvars_by_record: HashMap<DynamicBox, Arc<Condvar>>,

    #[serde(skip)]
    current_dependencies_by_dependant: HashMap<DynamicBox, DynamicBox>,

    dependency_graph: HashMap<DynamicBox, HashSet<DynamicBox>>,

    version_info_by_keys: HashMap<DynamicBox, VersionInfo>,

    cyclic_dependencies: Vec<CyclicDependency>,
}

impl CallGraph {
    fn called_from(&self) -> Option<DynamicBox> {
        let current_thread_id = std::thread::current().id();
        self.record_stacks_by_thread_id
            .get(&current_thread_id)
            .and_then(|x| x.last().cloned())
    }
}

/// Stores the error information about a cyclic dependency in the call graph.
#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct CyclicDependency {
    /// The stack of records that caused the cyclic dependency.
    pub records_stack: Vec<DynamicBox>,
}

impl std::fmt::Debug for CallGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CallGraph").finish_non_exhaustive()
    }
}

/// Stores the information about the version of a query result used to track
/// the validity of the result.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize,
)]
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
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    serde::Serialize,
    serde::Deserialize,
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
            .entry(DynamicBox(key.smallbox_clone()))
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
    ///
    /// # Returns
    ///
    /// Returns `Ok(value)` for successful queries, or `Err(CyclicError)` for
    /// queries that are part of a strongly connected component (SCC). Queries
    /// outside the SCC that depend on cyclic queries will receive default
    /// values.
    pub fn query<T: Dynamic + Key>(
        &self,
        key: &T,
    ) -> Result<T::Value, crate::runtime::executor::CyclicError> {
        self.query_internal(key, self.call_graph.lock()).0
    }

    fn check_cyclic(
        &self,
        computed_successfully: bool,
        called_from: Option<&DynamicBox>,
        call_graph: &mut MutexGuard<CallGraph>,
    ) -> Result<(), crate::runtime::executor::CyclicError> {
        let (Some(called_from), true) = (called_from, !computed_successfully)
        else {
            return Ok(());
        };

        let Some(version_info) =
            call_graph.version_info_by_keys.get(called_from)
        else {
            return Ok(());
        };

        if matches!(version_info.kind, Kind::Derived {
            defaulted_by_cyclic_dependency: true
        }) && version_info.verfied_at_version == self.version
        {
            return Err(crate::runtime::executor::CyclicError);
        }

        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    pub(super) fn query_internal<'a, T: Dynamic + Key>(
        &'a self,
        key: &T,
        mut call_graph: MutexGuard<'a, CallGraph>,
    ) -> (
        Result<T::Value, crate::runtime::executor::CyclicError>,
        MutexGuard<'a, CallGraph>,
    ) {
        let key_smallbox = DynamicBox(key.smallbox_clone());

        let called_from = call_graph.called_from();

        // query has already been computed return the result
        let Some(mut result) = self.map.get(key) else {
            self.last_was_query
                .store(true, std::sync::atomic::Ordering::SeqCst);

            // the value hasn't been computed yet, so we need to compute it
            let (computed_successfully, mut call_graph) = self.fresh_query(
                key,
                &key_smallbox,
                called_from.as_ref(),
                call_graph,
            );

            if self.check_cyclic(
                computed_successfully,
                called_from.as_ref(),
                &mut call_graph,
            ) != Ok(())
            {
                return (
                    Err(crate::runtime::executor::CyclicError),
                    call_graph,
                );
            }

            return (Ok(self.map.get(key).unwrap()), call_graph);
        };

        // the result is already up to date
        let version_info =
            *call_graph.version_info_by_keys.get(&key_smallbox).unwrap();

        if version_info.kind == Kind::Input {
            // add the dependency of the input
            if let Some(called_from) = &called_from {
                call_graph
                    .dependency_graph
                    .get_mut(called_from)
                    .unwrap()
                    .insert(key_smallbox);
            }

            // the value is an `input` value, always returns as it.
            return (Ok(result), call_graph);
        }

        self.last_was_query.store(true, std::sync::atomic::Ordering::SeqCst);

        if version_info.verfied_at_version == self.version {
            // add the dependency of the input
            if let Some(called_from) = &called_from {
                call_graph
                    .dependency_graph
                    .get_mut(called_from)
                    .unwrap()
                    .insert(key_smallbox);
            }

            return (Ok(result), call_graph);
        }

        let recompute = if matches!(version_info.kind, Kind::Derived {
            defaulted_by_cyclic_dependency: true
        }) {
            true
        } else {
            let inputs = call_graph
                .dependency_graph
                .get(&key_smallbox)
                .unwrap()
                .iter()
                .map(|x| DynamicBox(x.smallbox_clone()))
                .collect::<Vec<_>>();

            let mut recompute = false;
            for dep in &inputs {
                // run inputs verification for the input as well
                let is_input =
                    call_graph.version_info_by_keys.get(dep).unwrap().kind
                        == Kind::Input;

                if !is_input {
                    let invoke_fn = self
                        .runtime
                        .executor
                        .get_invoke_query(&dep.any().type_id())
                        .unwrap_or_else(|| {
                            panic!(
                                "no executor registered for key type `{}`",
                                dep.type_name()
                            )
                        });

                    call_graph = invoke_fn(self, &***dep, call_graph);
                }

                // check if there's need to recompute the value
                if !recompute {
                    let input_version_info =
                        call_graph.version_info_by_keys.get(dep).unwrap();

                    let should_recompute = input_version_info
                        .updated_at_version
                        > version_info.verfied_at_version;

                    recompute |= should_recompute;
                }
            }

            recompute
        };

        if recompute {
            // recompute the value
            let (computed_successfully, mut returned_call_graph) = self
                .fresh_query(
                    key,
                    &key_smallbox,
                    called_from.as_ref(),
                    call_graph,
                );

            if self.check_cyclic(
                computed_successfully,
                called_from.as_ref(),
                &mut returned_call_graph,
            ) != Ok(())
            {
                return (
                    Err(crate::runtime::executor::CyclicError),
                    returned_call_graph,
                );
            }

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

        (Ok(result), call_graph)
    }

    #[allow(clippy::too_many_lines)]
    fn fresh_query<'a, T: Dynamic + Key>(
        &'a self,
        key: &T,
        key_smallbox: &DynamicBox,
        called_from: Option<&DynamicBox>,
        mut call_graph: MutexGuard<'a, CallGraph>,
    ) -> (bool, MutexGuard<'a, CallGraph>) {
        call_graph
            .dependency_graph
            .insert(key_smallbox.clone(), HashSet::new());

        let executor = self.runtime.executor.get::<T>().unwrap_or_else(|| {
            panic!(
                "no executor registered for key type {}",
                std::any::type_name::<T>()
            )
        });

        let current_thread_id = std::thread::current().id();

        // check for cyclic dependencies
        if let Some(called_from) = called_from {
            call_graph
                .dependency_graph
                .get_mut(called_from)
                .unwrap()
                .insert(DynamicBox(key.smallbox_clone()));

            // check if `target_record` can go to `called_from`
            let mut stack = vec![DynamicBox(key.smallbox_clone())];

            loop {
                if stack.last().unwrap() == called_from {
                    for call in &stack {
                        match call_graph
                            .version_info_by_keys
                            .entry(call.clone())
                        {
                            Entry::Occupied(occupied_entry) => {
                                let version_info = occupied_entry.into_mut();
                                version_info.verfied_at_version = self.version;
                                version_info.kind = Kind::Derived {
                                    defaulted_by_cyclic_dependency: true,
                                };
                            }
                            Entry::Vacant(vacant_entry) => {
                                vacant_entry.insert(VersionInfo {
                                    updated_at_version: self.version,
                                    verfied_at_version: self.version,
                                    kind: Kind::Derived {
                                        defaulted_by_cyclic_dependency: true,
                                    },
                                });
                            }
                        }
                    }

                    call_graph
                        .cyclic_dependencies
                        .push(CyclicDependency { records_stack: stack });

                    // insert a default value for the cyclic dependency
                    self.map
                        .insert(key.clone(), <T::Value as Default>::default());

                    call_graph.version_info_by_keys.insert(
                        DynamicBox(key.smallbox_clone()),
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
                    stack.push(next.clone());
                } else {
                    break;
                }
            }

            assert!(call_graph
                .current_dependencies_by_dependant
                .insert(called_from.clone(), DynamicBox(key.smallbox_clone()))
                .is_none());
        }

        // add the current record to the call stack
        call_graph
            .record_stacks_by_thread_id
            .entry(current_thread_id)
            .or_default()
            .push(DynamicBox(key.smallbox_clone()));

        let sync = call_graph.condvars_by_record.get(key_smallbox).cloned();

        // there's an another thread that is computing the same record
        let succeeded = if let Some(sync) = sync {
            sync.wait(&mut call_graph);
            true
        } else {
            let (new_call_graph, ok) =
                self.compute(key, key_smallbox, call_graph, &*executor);

            call_graph = new_call_graph;
            ok
        };

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
                .remove(called_from)
                .is_some());
        }

        // Return whether the computation was successful (not cyclic)
        (succeeded, call_graph)
    }

    #[allow(clippy::too_many_lines)]
    fn compute<'a, K: Key + Dynamic>(
        &'a self,
        key: &K,
        key_smallbox: &DynamicBox,
        mut call_graph: MutexGuard<'a, CallGraph>,
        executor: &dyn Executor<K>,
    ) -> (MutexGuard<'a, CallGraph>, bool) {
        // compute the component
        let sync = Arc::new(Condvar::new());
        assert!(call_graph
            .condvars_by_record
            .insert(key_smallbox.clone(), sync.clone())
            .is_none());

        // skipcq: RS-E1021 false positive
        drop(call_graph); // release the context lock

        let executor_result = executor.execute(self, key.clone());
        let ok = executor_result.is_ok();

        // re-acquire the context lock
        call_graph = self.call_graph.lock();

        // Handle the executor result
        match executor_result {
            Ok(value) => {
                let updated =
                    self.map.entry(key.clone(), |entry| match entry {
                        dashmap::Entry::Occupied(mut occupied_entry) => {
                            let updated = *occupied_entry.get_mut() != value;

                            if updated {
                                occupied_entry.insert(value);
                            }

                            updated
                        }
                        dashmap::Entry::Vacant(vacant_entry) => {
                            vacant_entry.insert(value);

                            false
                        }
                    });

                match call_graph
                    .version_info_by_keys
                    .entry(DynamicBox(key.smallbox_clone()))
                {
                    Entry::Occupied(mut occupied_entry) => {
                        let version_info = occupied_entry.get_mut();
                        version_info.verfied_at_version = self.version;
                        version_info.kind = Kind::Derived {
                            defaulted_by_cyclic_dependency: false,
                        };

                        if updated {
                            version_info.updated_at_version = self.version;
                        }
                    }
                    Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(VersionInfo {
                            updated_at_version: self.version,
                            verfied_at_version: self.version,
                            kind: Kind::Derived {
                                defaulted_by_cyclic_dependency: false,
                            },
                        });
                    }
                }
            }
            Err(_cyclic_error) => {
                call_graph.version_info_by_keys.insert(
                    DynamicBox(key.smallbox_clone()),
                    VersionInfo {
                        updated_at_version: self.version,
                        verfied_at_version: self.version,
                        kind: Kind::Derived {
                            defaulted_by_cyclic_dependency: true,
                        },
                    },
                );

                match call_graph
                    .version_info_by_keys
                    .entry(DynamicBox(key.smallbox_clone()))
                {
                    Entry::Occupied(occupied_entry) => {
                        let version_info = occupied_entry.into_mut();

                        // must've been marked as cyclic before
                        assert!(
                            version_info.verfied_at_version == self.version
                                && matches!(version_info.kind, Kind::Derived {
                                    defaulted_by_cyclic_dependency: true
                                })
                        );

                        version_info.verfied_at_version = self.version;
                        version_info.updated_at_version = self.version;
                        version_info.kind = Kind::Derived {
                            defaulted_by_cyclic_dependency: true,
                        };
                    }
                    Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(VersionInfo {
                            updated_at_version: self.version,
                            verfied_at_version: self.version,
                            kind: Kind::Derived {
                                defaulted_by_cyclic_dependency: true,
                            },
                        });
                    }
                }

                // Cyclic dependency detected - store default value and mark as
                // cyclic
                let default_value = <K::Value as Default>::default();

                self.map.entry(key.clone(), |entry| match entry {
                    dashmap::Entry::Occupied(mut occupied_entry) => {
                        occupied_entry.insert(default_value);
                        false
                    }
                    dashmap::Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(default_value);
                        false
                    }
                });
            }
        }

        assert!(call_graph.condvars_by_record.remove(key_smallbox).is_some());

        // notify the other threads that the component is computed
        sync.notify_all();

        (call_graph, ok)
    }
}

#[cfg(test)]
mod test;
