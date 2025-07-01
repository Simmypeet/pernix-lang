#![allow(clippy::mutable_key_type)]

//! Implements the [`QueryTracker`] struct used to track the dependencies
//! between queries.

use core::panic;
use std::{collections::hash_map::Entry, sync::Arc, thread::ThreadId};

use enum_as_inner::EnumAsInner;
use getset::Getters;
use parking_lot::{Condvar, MutexGuard};
use pernixc_hash::{HashMap, HashSet};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::{StableHash, StableHasher};

use crate::{
    fingerprint,
    key::{Dynamic, DynamicBox, Key},
    runtime::{
        executor::Executor,
        persistence::serde::{DynamicDeserialize, DynamicSerialize},
    },
    Engine,
};

/// Tracks the dependencies between queries and their execution order.
#[derive(Default, Serialize, Deserialize, Getters)]
#[allow(clippy::mutable_key_type, missing_docs)]
#[serde(
    ser_extension(DynamicSerialize<__S>),
    de_extension(DynamicDeserialize<__D>)
)]
pub struct QueryTracker {
    // emulating a call stack of particular thread
    #[serde(skip)]
    record_stacks_by_thread_id: HashMap<ThreadId, Vec<DynamicBox>>,

    // the condition variables used to notify the threads that are waiting for
    // the completion of a particular record
    #[serde(skip)]
    condvars_by_record: HashMap<DynamicBox, Arc<Condvar>>,

    #[serde(skip)]
    current_dependencies_by_dependant: HashMap<DynamicBox, DynamicBox>,

    /// Key representing the query and the value is a set of keys that
    /// the query depends on.
    #[get = "pub"]
    dependency_graph: HashMap<DynamicBox, HashSet<DynamicBox>>,

    /// Representing the version information of when the query was computed
    #[get = "pub"]
    version_info_by_keys: HashMap<DynamicBox, VersionInfo>,

    /// List of cyclic dependencies that were detected during the query
    /// execution.
    #[get = "pub"]
    cyclic_dependencies: Vec<CyclicDependency>,
}

impl QueryTracker {
    fn called_from(&self) -> Option<DynamicBox> {
        let current_thread_id = std::thread::current().id();
        self.record_stacks_by_thread_id
            .get(&current_thread_id)
            .and_then(|x| x.last().cloned())
    }
}

/// Stores the error information about a cyclic dependency in the query tracker.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(
    ser_extension(DynamicSerialize<__S>),
    de_extension(DynamicDeserialize<__D>)
)]
pub struct CyclicDependency {
    /// The stack of records that caused the cyclic dependency.
    pub records_stack: Vec<DynamicBox>,
}

impl std::fmt::Debug for QueryTracker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("QueryTracker").finish_non_exhaustive()
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
    Serialize,
    Deserialize,
)]
pub struct VersionInfo {
    /// The version when the value was computed.
    updated_at_version: usize,

    /// The latest version that the value was verified against its result from
    /// the latest computation.
    verfied_at_version: usize,

    fingerprint: Option<u128>,

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
    Serialize,
    Deserialize,
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

fn calculate_fingerprint<T: StableHash>(value: &T) -> u128 {
    let mut sip = pernixc_stable_hash::StableSipHasher::new();
    value.stable_hash(&mut sip);
    sip.finish()
}

impl Engine {
    /// Sets the input value for the given key.
    ///
    /// If this call happens after one of the derived values has been computed,
    /// the version of the database will be bumped up by one. This is to
    /// indicate that the input value has changed and all the derived values
    /// need to reflect this change.
    pub fn set_input<K: Key + Dynamic>(&mut self, key: &K, value: K::Value) {
        // set the input value
        let value_fingerprint = calculate_fingerprint(&value);
        self.database.map.insert(key.clone(), value);

        let mut update_version = |version: &mut VersionInfo| {
            version.kind = Kind::Input;
            version.verfied_at_version = self.database.snapshot.version;

            // update the version info if invalidated
            if Some(value_fingerprint) != version.fingerprint {
                // bump the version for the new input setting
                if *self.database.snapshot.last_was_query.get_mut() {
                    self.database.snapshot.version += 1;
                    *self.database.snapshot.last_was_query.get_mut() = false;
                }

                version.updated_at_version = self.database.snapshot.version;
                version.fingerprint = Some(value_fingerprint);
            }
        };

        match self
            .database
            .query_tracker
            .get_mut()
            .version_info_by_keys
            .entry(DynamicBox(key.smallbox_clone()))
        {
            Entry::Occupied(mut occupied_entry) => {
                let version = occupied_entry.get_mut();
                update_version(version);
            }
            Entry::Vacant(entry) => {
                // try to load from the persistence if available
                let loaded = self.runtime.persistence.as_ref().and_then(|x| {
                    x.try_load_version_info::<K>(fingerprint::fingerprint(key))
                        .ok()
                        .flatten()
                });

                match loaded {
                    Some(mut loaded) => {
                        update_version(&mut loaded);
                        entry.insert(loaded);
                    }
                    None => {
                        entry.insert(VersionInfo {
                            updated_at_version: self.database.snapshot.version,
                            verfied_at_version: self.database.snapshot.version,
                            fingerprint: Some(value_fingerprint),
                            kind: Kind::Input,
                        });
                    }
                }
            }
        }
    }
}

impl Engine {
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
        let (result, query_tracker) =
            self.query_internal(key, self.database.query_tracker.lock());

        result?;

        self.database.map.get(key).map_or_else(
            || {
                let key_smallbox = DynamicBox(key.smallbox_clone());
                let version_info = query_tracker
                    .version_info_by_keys
                    .get(&key_smallbox)
                    .copied()
                    .unwrap();

                // load from the persistence if available
                let value = self
                    .runtime
                    .persistence
                    .as_ref()
                    .and_then(|x| {
                        x.try_load_value::<T>(version_info.fingerprint.unwrap())
                            .ok()
                    })
                    .flatten();

                if let Some(value) = value {
                    // save for the future use
                    self.database.map.insert(key.clone(), value.clone());

                    return Ok(value);
                }

                let called_from = query_tracker.called_from();

                // compute it again
                let (computed_successfully, mut query_tracker) = self
                    .fresh_query(
                        key,
                        &key_smallbox,
                        called_from.as_ref(),
                        query_tracker,
                    );

                if self.check_cyclic(
                    computed_successfully,
                    called_from.as_ref(),
                    &mut query_tracker,
                ) != Ok(())
                {
                    return Err(crate::runtime::executor::CyclicError);
                }

                Ok(self
                    .database
                    .map
                    .get(key)
                    .expect("value should be computed"))
            },
            Ok,
        )
    }

    fn check_cyclic(
        &self,
        computed_successfully: bool,
        called_from: Option<&DynamicBox>,
        query_tracker: &mut MutexGuard<QueryTracker>,
    ) -> Result<(), crate::runtime::executor::CyclicError> {
        let (Some(called_from), true) = (called_from, !computed_successfully)
        else {
            return Ok(());
        };

        let Some(version_info) =
            query_tracker.version_info_by_keys.get(called_from)
        else {
            return Ok(());
        };

        if matches!(version_info.kind, Kind::Derived {
            defaulted_by_cyclic_dependency: true
        }) && version_info.verfied_at_version
            == self.database.snapshot.version
        {
            return Err(crate::runtime::executor::CyclicError);
        }

        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    pub(crate) fn query_internal<'a, T: Dynamic + Key>(
        &'a self,
        key: &T,
        mut query_tracker: MutexGuard<'a, QueryTracker>,
    ) -> (
        Result<(), crate::runtime::executor::CyclicError>,
        MutexGuard<'a, QueryTracker>,
    ) {
        let key_smallbox = DynamicBox(key.smallbox_clone());

        let called_from = query_tracker.called_from();

        // get the version info for the key.
        let Some(version_info) = query_tracker
            .version_info_by_keys
            .get(&key_smallbox)
            .copied()
            .or_else(|| {
                let loaded = self.runtime.persistence.as_ref().and_then(|x| {
                    x.try_load_version_info::<T>(key_smallbox.fingerprint())
                        .ok()
                        .flatten()
                });

                // cache to in-memory
                if let Some(loaded) = loaded {
                    query_tracker
                        .version_info_by_keys
                        .insert(key_smallbox.clone(), loaded);
                }

                loaded
            })
        else {
            self.database
                .snapshot
                .last_was_query
                .store(true, std::sync::atomic::Ordering::SeqCst);

            // the value hasn't been computed yet, so we need to compute it
            let (computed_successfully, mut query_tracker) = self.fresh_query(
                key,
                &key_smallbox,
                called_from.as_ref(),
                query_tracker,
            );

            if self.check_cyclic(
                computed_successfully,
                called_from.as_ref(),
                &mut query_tracker,
            ) != Ok(())
            {
                return (
                    Err(crate::runtime::executor::CyclicError),
                    query_tracker,
                );
            }

            return (Ok(()), query_tracker);
        };

        if version_info.kind == Kind::Input {
            // add the dependency of the input
            if let Some(called_from) = &called_from {
                query_tracker
                    .dependency_graph
                    .get_mut(called_from)
                    .unwrap()
                    .insert(key_smallbox);
            }

            // the value is an `input` value, always returns as it.
            return (Ok(()), query_tracker);
        }

        self.database
            .snapshot
            .last_was_query
            .store(true, std::sync::atomic::Ordering::SeqCst);

        if version_info.verfied_at_version == self.database.snapshot.version {
            // add the dependency of the input
            if let Some(called_from) = &called_from {
                query_tracker
                    .dependency_graph
                    .get_mut(called_from)
                    .unwrap()
                    .insert(key_smallbox);
            }

            return (Ok(()), query_tracker);
        }

        let recompute = if matches!(version_info.kind, Kind::Derived {
            defaulted_by_cyclic_dependency: true
        }) {
            true
        } else {
            'result: {
                let Some(inputs) = query_tracker
                    .dependency_graph
                    .get(&key_smallbox)
                    .map(|x| {
                        x.iter()
                            .map(|x| DynamicBox(x.smallbox_clone()))
                            .collect::<Vec<_>>()
                    })
                    .or_else(|| {
                        let loaded =
                            self.runtime.persistence.as_ref().and_then(|x| {
                                x.try_load_dependencies::<T>(
                                    key_smallbox.fingerprint(),
                                )
                                .ok()
                                .flatten()
                            });

                        let dependencies_vec = loaded.as_ref().map(|x| {
                            x.iter()
                                .map(|x| DynamicBox(x.smallbox_clone()))
                                .collect::<Vec<_>>()
                        });

                        if let Some(dependencies) = loaded {
                            query_tracker
                                .dependency_graph
                                .insert(key_smallbox.clone(), dependencies);
                        }

                        dependencies_vec
                    })
                else {
                    // recompute
                    break 'result true;
                };

                let mut recompute = false;
                for dep in &inputs {
                    // run inputs verification for the input as well
                    let is_input = query_tracker
                        .version_info_by_keys
                        .get(dep)
                        .unwrap()
                        .kind
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

                        query_tracker = invoke_fn(self, &***dep, query_tracker);
                    }

                    // check if there's need to recompute the value
                    if !recompute {
                        let input_version_info = query_tracker
                            .version_info_by_keys
                            .get(dep)
                            .unwrap();

                        let should_recompute = input_version_info
                            .updated_at_version
                            > version_info.verfied_at_version;

                        recompute |= should_recompute;
                    }
                }

                recompute
            }
        };

        if recompute {
            // recompute the value
            let (computed_successfully, mut returned_call_graph) = self
                .fresh_query(
                    key,
                    &key_smallbox,
                    called_from.as_ref(),
                    query_tracker,
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

            query_tracker = returned_call_graph;
        } else {
            query_tracker
                .version_info_by_keys
                .get_mut(&key_smallbox)
                .unwrap()
                .verfied_at_version = self.database.snapshot.version;
        }

        (Ok(()), query_tracker)
    }

    #[allow(clippy::too_many_lines)]
    fn fresh_query<'a, T: Dynamic + Key>(
        &'a self,
        key: &T,
        key_smallbox: &DynamicBox,
        called_from: Option<&DynamicBox>,
        mut query_tracker: MutexGuard<'a, QueryTracker>,
    ) -> (bool, MutexGuard<'a, QueryTracker>) {
        query_tracker
            .dependency_graph
            .insert(key_smallbox.clone(), HashSet::default());

        let executor = self.runtime.executor.get::<T>().unwrap_or_else(|| {
            panic!(
                "no executor registered for key type {}",
                std::any::type_name::<T>()
            )
        });

        let current_thread_id = std::thread::current().id();

        // check for cyclic dependencies
        if let Some(called_from) = called_from {
            query_tracker
                .dependency_graph
                .get_mut(called_from)
                .unwrap()
                .insert(DynamicBox(key.smallbox_clone()));

            // check if `target_record` can go to `called_from`
            let mut stack = vec![DynamicBox(key.smallbox_clone())];

            loop {
                if stack.last().unwrap() == called_from {
                    for call in &stack {
                        match query_tracker
                            .version_info_by_keys
                            .entry(call.clone())
                        {
                            Entry::Occupied(occupied_entry) => {
                                let version_info = occupied_entry.into_mut();
                                version_info.verfied_at_version =
                                    self.database.snapshot.version;
                                version_info.kind = Kind::Derived {
                                    defaulted_by_cyclic_dependency: true,
                                };
                            }
                            Entry::Vacant(vacant_entry) => {
                                vacant_entry.insert(VersionInfo {
                                    updated_at_version: self
                                        .database
                                        .snapshot
                                        .version,
                                    verfied_at_version: self
                                        .database
                                        .snapshot
                                        .version,
                                    kind: Kind::Derived {
                                        defaulted_by_cyclic_dependency: true,
                                    },
                                    fingerprint: None,
                                });
                            }
                        }
                    }

                    query_tracker
                        .cyclic_dependencies
                        .push(CyclicDependency { records_stack: stack });

                    // insert a default value for the cyclic dependency
                    self.database.map.insert(key.clone(), T::scc_value());

                    query_tracker.version_info_by_keys.insert(
                        DynamicBox(key.smallbox_clone()),
                        VersionInfo {
                            updated_at_version: self.database.snapshot.version,
                            verfied_at_version: self.database.snapshot.version,
                            kind: Kind::Derived {
                                defaulted_by_cyclic_dependency: true,
                            },
                            fingerprint: None,
                        },
                    );

                    // signifying that the value was defaulted
                    return (false, query_tracker);
                }

                // follow the dependency chain
                if let Some(next) = query_tracker
                    .current_dependencies_by_dependant
                    .get(stack.last().unwrap())
                {
                    stack.push(next.clone());
                } else {
                    break;
                }
            }

            assert!(query_tracker
                .current_dependencies_by_dependant
                .insert(called_from.clone(), DynamicBox(key.smallbox_clone()))
                .is_none());
        }

        // add the current record to the call stack
        query_tracker
            .record_stacks_by_thread_id
            .entry(current_thread_id)
            .or_default()
            .push(DynamicBox(key.smallbox_clone()));

        let sync = query_tracker.condvars_by_record.get(key_smallbox).cloned();

        // there's an another thread that is computing the same record
        let succeeded = if let Some(sync) = sync {
            sync.wait(&mut query_tracker);
            true
        } else {
            let (new_call_graph, ok) =
                self.compute(key, key_smallbox, query_tracker, &*executor);

            query_tracker = new_call_graph;
            ok
        };

        assert!(
            query_tracker
                .record_stacks_by_thread_id
                .get_mut(&current_thread_id)
                .unwrap()
                .pop()
                .unwrap()
                == *key_smallbox,
        );

        if let Some(called_from) = called_from {
            assert!(query_tracker
                .current_dependencies_by_dependant
                .remove(called_from)
                .is_some());
        }

        // Return whether the computation was successful (not cyclic)
        (succeeded, query_tracker)
    }

    #[allow(clippy::too_many_lines)]
    fn compute<'a, K: Key + Dynamic>(
        &'a self,
        key: &K,
        key_smallbox: &DynamicBox,
        mut query_tracker: MutexGuard<'a, QueryTracker>,
        executor: &dyn Executor<K>,
    ) -> (MutexGuard<'a, QueryTracker>, bool) {
        // compute the component
        let sync = Arc::new(Condvar::new());
        assert!(query_tracker
            .condvars_by_record
            .insert(key_smallbox.clone(), sync.clone())
            .is_none());

        // skipcq: RS-E1021 false positive
        drop(query_tracker); // release the context lock

        let executor_result = executor.execute(self, key.clone());
        let ok = executor_result.is_ok();

        // re-acquire the context lock
        query_tracker = self.database.query_tracker.lock();

        // Handle the executor result
        match executor_result {
            Ok(value) => {
                let value_fingerprint = calculate_fingerprint(&value);
                self.database.map.insert(key.clone(), value);

                match query_tracker
                    .version_info_by_keys
                    .entry(DynamicBox(key.smallbox_clone()))
                {
                    Entry::Occupied(mut occupied_entry) => {
                        let version_info = occupied_entry.get_mut();
                        version_info.verfied_at_version =
                            self.database.snapshot.version;
                        version_info.kind = Kind::Derived {
                            defaulted_by_cyclic_dependency: false,
                        };

                        if Some(value_fingerprint) != version_info.fingerprint {
                            version_info.updated_at_version =
                                self.database.snapshot.version;
                            version_info.fingerprint = Some(value_fingerprint);
                        }
                    }
                    Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(VersionInfo {
                            updated_at_version: self.database.snapshot.version,
                            verfied_at_version: self.database.snapshot.version,
                            kind: Kind::Derived {
                                defaulted_by_cyclic_dependency: false,
                            },
                            fingerprint: Some(value_fingerprint),
                        });
                    }
                }
            }
            Err(_cyclic_error) => {
                query_tracker.version_info_by_keys.insert(
                    DynamicBox(key.smallbox_clone()),
                    VersionInfo {
                        updated_at_version: self.database.snapshot.version,
                        verfied_at_version: self.database.snapshot.version,
                        kind: Kind::Derived {
                            defaulted_by_cyclic_dependency: true,
                        },
                        fingerprint: None,
                    },
                );

                match query_tracker
                    .version_info_by_keys
                    .entry(DynamicBox(key.smallbox_clone()))
                {
                    Entry::Occupied(occupied_entry) => {
                        let version_info = occupied_entry.into_mut();

                        // must've been marked as cyclic before
                        assert!(
                            version_info.verfied_at_version
                                == self.database.snapshot.version
                                && matches!(version_info.kind, Kind::Derived {
                                    defaulted_by_cyclic_dependency: true
                                })
                        );

                        version_info.verfied_at_version =
                            self.database.snapshot.version;
                        version_info.updated_at_version =
                            self.database.snapshot.version;
                        version_info.kind = Kind::Derived {
                            defaulted_by_cyclic_dependency: true,
                        };
                    }
                    Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(VersionInfo {
                            updated_at_version: self.database.snapshot.version,
                            verfied_at_version: self.database.snapshot.version,
                            kind: Kind::Derived {
                                defaulted_by_cyclic_dependency: true,
                            },
                            fingerprint: None,
                        });
                    }
                }

                // Cyclic dependency detected - store default value and mark as
                // cyclic
                let default_value = K::scc_value();

                self.database.map.entry(key.clone(), |entry| match entry {
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

        assert!(query_tracker
            .condvars_by_record
            .remove(key_smallbox)
            .is_some());

        // notify the other threads that the component is computed
        sync.notify_all();

        (query_tracker, ok)
    }
}

#[cfg(test)]
mod test;
