#![allow(clippy::mutable_key_type)]

//! Implements the [`QueryTracker`] struct used to track the dependencies
//! between queries.

use core::panic;
use std::{collections::hash_map::Entry, sync::Arc};

use enum_as_inner::EnumAsInner;
use getset::Getters;
use parking_lot::{Condvar, MutexGuard};
use pernixc_hash::{HashMap, HashSet};
use pernixc_serialize::{Deserialize, Serialize};

use crate::{
    fingerprint,
    key::{Dynamic, DynamicKey, Key},
    runtime::{
        executor::Executor,
        persistence::serde::{DynamicDeserialize, DynamicSerialize},
    },
    Engine,
};

/// A wrapper over the [`Engine`] with the key remembering where the query was
/// called from.
///
/// All the queries must be done through this struct to ensure that dependencies
/// are correctly tracked.
///
/// This struct allows the query engine to track the dependencies between query.
#[derive(Debug, Clone)]
pub struct Tracked<'a> {
    engine: &'a Engine,
    called_from: Option<&'a DynamicKey>,
}

impl Engine {
    /// Creates a new [`Tracked`] instance for the current engine.
    ///
    /// Using [`Tracked`] instance allows you to query the engine while
    /// tracking the dependencies of the queries.
    pub const fn tracked(&self) -> Tracked<'_> {
        Tracked { engine: self, called_from: None }
    }
}

impl Tracked<'_> {
    /// Queries the value associated with the given key.
    ///
    /// # Panics
    ///
    /// If the key doesn't have a corresponding [`Executor`] registered in the
    /// database or the input isn't explicitly set.
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
        self.engine.query(key, self.called_from)
    }
}

/// Stores the version information of the database, including whether the
/// last operation was a query or not.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
)]
pub struct Snapshot {
    /// The current version of the database.
    ///
    /// This value is incremented whenever the database is modified,
    /// such as when a new key-value pair is added or an existing one is
    /// updated.
    pub version: usize,

    /// Indicates whether the last operation was a query.
    pub last_was_query: bool,
}

/// Tracks the dependencies between queries and their execution order.
#[derive(Default, Serialize, Deserialize, Getters)]
#[allow(clippy::mutable_key_type, missing_docs)]
#[serde(
    ser_extension(DynamicSerialize<__S>),
    de_extension(DynamicDeserialize<__D>)
)]
pub struct QueryTracker {
    // the condition variables used to notify the threads that are waiting for
    // the completion of a particular record
    #[serde(skip)]
    condvars_by_record: HashMap<DynamicKey, Arc<Condvar>>,

    #[serde(skip)]
    current_dependencies_by_dependant: HashMap<DynamicKey, DynamicKey>,

    /// Key representing the query and the value is a set of keys that
    /// the query depends on.
    #[get = "pub"]
    dependency_graph: HashMap<DynamicKey, HashSet<DynamicKey>>,

    /// Representing the version information of when the query was computed
    #[get = "pub"]
    version_info_by_keys: HashMap<DynamicKey, VersionInfo>,

    /// List of cyclic dependencies that were detected during the query
    /// execution.
    #[get = "pub"]
    cyclic_dependencies: Vec<CyclicDependency>,

    /// Represents a snapshot of the database version.
    #[get = "pub"]
    snapshot: Snapshot,
}

impl Engine {
    /// Returns the current snapshot of the database.
    pub fn snapshot(&self) -> Snapshot {
        self.database.query_tracker.lock().snapshot
    }
}

impl QueryTracker {
    /// Creates a new instance of [`QueryTracker`] with provided snapshot.
    #[must_use]
    pub fn with_snapshot(snapshot: Snapshot) -> Self {
        Self {
            condvars_by_record: HashMap::default(),
            current_dependencies_by_dependant: HashMap::default(),
            dependency_graph: HashMap::default(),
            version_info_by_keys: HashMap::default(),
            cyclic_dependencies: Vec::new(),
            snapshot,
        }
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
    pub records_stack: Vec<DynamicKey>,
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

impl Engine {
    /// Sets the input value for the given key.
    ///
    /// If this call happens after one of the derived values has been computed,
    /// the version of the database will be bumped up by one. This is to
    /// indicate that the input value has changed and all the derived values
    /// need to reflect this change.
    pub fn set_input<K: Key + Dynamic>(&self, key: &K, value: K::Value) {
        // set the input value
        let value_fingerprint = fingerprint::fingerprint(&value);
        let key_fingerprint = fingerprint::fingerprint(key);

        let update_version = |version: &mut VersionInfo,
                              snapshot: &mut Snapshot|
         -> (bool, bool) {
            let mut save_value = false;
            let mut need_update = version.kind != Kind::Input
                || version.verfied_at_version != snapshot.version;

            version.kind = Kind::Input;
            version.verfied_at_version = snapshot.version;

            // update the version info if invalidated
            if Some(value_fingerprint) != version.fingerprint {
                // bump the version for the new input setting
                if snapshot.last_was_query {
                    snapshot.version += 1;
                    snapshot.last_was_query = false;
                }

                version.updated_at_version = snapshot.version;
                version.fingerprint = Some(value_fingerprint);

                // always need to update the version info
                need_update = true;
                save_value = true;
            }

            (need_update, save_value)
        };

        let mut query_tracker = self.database.query_tracker.lock();
        let query_tracker = &mut *query_tracker;

        let (save_version_info, save_value) = match query_tracker
            .version_info_by_keys
            .entry(DynamicKey(key.smallbox_clone()))
        {
            Entry::Occupied(mut occupied_entry) => {
                let version = occupied_entry.get_mut();
                let (version_need_update, update_value) =
                    update_version(version, &mut query_tracker.snapshot);

                (version_need_update.then_some(*version), update_value)
            }
            Entry::Vacant(entry) => {
                // try to load from the persistence if available
                let loaded = self.runtime.persistence.as_ref().and_then(|x| {
                    x.try_load_version_info::<K>(key_fingerprint).ok().flatten()
                });

                if let Some(mut loaded) = loaded {
                    let (result, update_value) = update_version(
                        &mut loaded,
                        &mut query_tracker.snapshot,
                    );

                    entry.insert(loaded);

                    (result.then_some(loaded), update_value)
                } else {
                    // setting new input could influence the derived values
                    // so we need to bump the version
                    if query_tracker.snapshot.last_was_query {
                        query_tracker.snapshot.version += 1;
                        query_tracker.snapshot.last_was_query = false;
                    }

                    let inserted = VersionInfo {
                        updated_at_version: query_tracker.snapshot.version,
                        verfied_at_version: query_tracker.snapshot.version,
                        fingerprint: Some(value_fingerprint),
                        kind: Kind::Input,
                    };
                    entry.insert(inserted);

                    (Some(inserted), true)
                }
            }
        };

        if let Some(persistence) = self.runtime.persistence.as_ref() {
            if let Some(version_info) = save_version_info {
                if let Err(err) = persistence
                    .save_version_info::<K>(key_fingerprint, &version_info)
                {
                    tracing::trace!(
                        "Failed to save version info for key {}: {}",
                        key.type_name(),
                        err
                    );
                }
            }

            if save_value {
                unsafe {
                    if let Err(err) = persistence
                        .save_with_fingerprint::<K>(&value, value_fingerprint)
                    {
                        tracing::trace!(
                            "Failed to save value for key {}: {}",
                            key.type_name(),
                            err
                        );
                    }
                }
            }
        }

        self.database.map.insert(key.clone(), value);
    }
}

impl Engine {
    fn query<T: Dynamic + Key>(
        &self,
        key: &T,
        called_from: Option<&DynamicKey>,
    ) -> Result<T::Value, crate::runtime::executor::CyclicError> {
        let (result, query_tracker) = self.query_internal(
            key,
            called_from,
            self.database.query_tracker.lock(),
        );

        result?;

        self.database.map.get(key).map_or_else(
            || {
                let key_smallbox = DynamicKey(key.smallbox_clone());
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

                // compute it again
                let (computed_successfully, mut query_tracker) = self
                    .fresh_query(
                        key,
                        &key_smallbox,
                        called_from,
                        query_tracker,
                    );

                if Self::check_cyclic(
                    computed_successfully,
                    called_from,
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
        computed_successfully: bool,
        called_from: Option<&DynamicKey>,
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
            == query_tracker.snapshot.version
        {
            return Err(crate::runtime::executor::CyclicError);
        }

        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    pub(crate) fn query_internal<'a, T: Dynamic + Key>(
        &'a self,
        key: &T,
        called_from: Option<&DynamicKey>,
        mut query_tracker: MutexGuard<'a, QueryTracker>,
    ) -> (
        Result<(), crate::runtime::executor::CyclicError>,
        MutexGuard<'a, QueryTracker>,
    ) {
        let key_smallbox = DynamicKey(key.smallbox_clone());

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
            query_tracker.snapshot.last_was_query = true;

            // the value hasn't been computed yet, so we need to compute it
            let (computed_successfully, mut query_tracker) = self.fresh_query(
                key,
                &key_smallbox,
                called_from,
                query_tracker,
            );

            if Self::check_cyclic(
                computed_successfully,
                called_from,
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

        query_tracker.snapshot.last_was_query = true;

        if version_info.verfied_at_version == query_tracker.snapshot.version {
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
                            .map(|x| DynamicKey(x.smallbox_clone()))
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
                                .map(|x| DynamicKey(x.smallbox_clone()))
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

                        query_tracker = invoke_fn(
                            self,
                            &***dep,
                            called_from,
                            query_tracker,
                        );
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
                .fresh_query(key, &key_smallbox, called_from, query_tracker);

            if Self::check_cyclic(
                computed_successfully,
                called_from,
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
                .verfied_at_version = query_tracker.snapshot.version;
        }

        (Ok(()), query_tracker)
    }

    #[allow(clippy::too_many_lines)]
    fn fresh_query<'a, T: Dynamic + Key>(
        &'a self,
        key: &T,
        key_smallbox: &DynamicKey,
        called_from: Option<&DynamicKey>,
        mut query_tracker: MutexGuard<'a, QueryTracker>,
    ) -> (bool, MutexGuard<'a, QueryTracker>) {
        let executor = self.runtime.executor.get::<T>().unwrap_or_else(|| {
            panic!(
                "no executor registered for key type {}",
                std::any::type_name::<T>()
            )
        });

        let mut query_tracker_ref = &mut *query_tracker;

        // check for cyclic dependencies
        if let Some(called_from) = called_from {
            query_tracker_ref
                .dependency_graph
                .get_mut(called_from)
                .unwrap()
                .insert(DynamicKey(key.smallbox_clone()));

            // check if `target_record` can go to `called_from`
            let mut stack = vec![DynamicKey(key.smallbox_clone())];

            loop {
                if stack.last().unwrap() == called_from {
                    for call in &stack {
                        match query_tracker_ref
                            .version_info_by_keys
                            .entry(call.clone())
                        {
                            Entry::Occupied(occupied_entry) => {
                                let version_info = occupied_entry.into_mut();
                                version_info.verfied_at_version =
                                    query_tracker_ref.snapshot.version;
                                version_info.kind = Kind::Derived {
                                    defaulted_by_cyclic_dependency: true,
                                };
                            }
                            Entry::Vacant(vacant_entry) => {
                                vacant_entry.insert(VersionInfo {
                                    updated_at_version: query_tracker_ref
                                        .snapshot
                                        .version,
                                    verfied_at_version: query_tracker_ref
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

                    query_tracker_ref
                        .cyclic_dependencies
                        .push(CyclicDependency { records_stack: stack });

                    // insert a default value for the cyclic dependency
                    self.database.map.insert(key.clone(), T::scc_value());

                    query_tracker_ref.version_info_by_keys.insert(
                        DynamicKey(key.smallbox_clone()),
                        VersionInfo {
                            updated_at_version: query_tracker_ref
                                .snapshot
                                .version,
                            verfied_at_version: query_tracker_ref
                                .snapshot
                                .version,
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
                if let Some(next) = query_tracker_ref
                    .current_dependencies_by_dependant
                    .get(stack.last().unwrap())
                {
                    stack.push(next.clone());
                } else {
                    break;
                }
            }

            assert!(query_tracker_ref
                .current_dependencies_by_dependant
                .insert(called_from.clone(), DynamicKey(key.smallbox_clone()))
                .is_none());
        }

        let sync =
            query_tracker_ref.condvars_by_record.get(key_smallbox).cloned();

        // there's an another thread that is computing the same record
        let succeeded = if let Some(sync) = sync {
            sync.wait(&mut query_tracker);
            query_tracker_ref = &mut *query_tracker;
            true
        } else {
            query_tracker_ref
                .dependency_graph
                .insert(key_smallbox.clone(), HashSet::default());

            let (new_call_graph, ok) =
                self.compute(key, key_smallbox, query_tracker, &*executor);

            // save the dependency graph
            if let Some(per) = self.runtime.persistence.as_ref() {
                if let Err(err) = per.save_dependency_graph(
                    T::STABLE_TYPE_ID,
                    key.fingerprint(),
                    new_call_graph.dependency_graph.get(key_smallbox).unwrap(),
                ) {
                    tracing::trace!(
                        "Failed to save dependency graph for key {}: {}",
                        key.type_name(),
                        err
                    );
                }
            }

            query_tracker = new_call_graph;
            query_tracker_ref = &mut *query_tracker;

            ok
        };

        if let Some(called_from) = called_from {
            assert!(query_tracker_ref
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
        key_smallbox: &DynamicKey,
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

        let executor_result = executor.execute(
            &Tracked { engine: self, called_from: Some(key_smallbox) },
            key.clone(),
        );

        let ok = executor_result.is_ok();

        // re-acquire the context lock
        query_tracker = self.database.query_tracker.lock();
        let query_tracker_ref = &mut *query_tracker;

        // Handle the executor result
        match executor_result {
            Ok(value) => {
                let value_fingerprint = fingerprint::fingerprint(&value);
                self.database.map.insert(key.clone(), value.clone());

                let (save_database, save_version_info) = match query_tracker_ref
                    .version_info_by_keys
                    .entry(DynamicKey(key.smallbox_clone()))
                {
                    Entry::Occupied(mut occupied_entry) => {
                        let version_info = occupied_entry.get_mut();
                        let initial_version = *version_info;

                        version_info.verfied_at_version =
                            query_tracker_ref.snapshot.version;
                        version_info.kind = Kind::Derived {
                            defaulted_by_cyclic_dependency: false,
                        };

                        let save_value = if Some(value_fingerprint)
                            == version_info.fingerprint
                        {
                            false
                        } else {
                            version_info.updated_at_version =
                                query_tracker_ref.snapshot.version;
                            version_info.fingerprint = Some(value_fingerprint);

                            true
                        };

                        (
                            save_value,
                            (&initial_version != version_info)
                                .then_some(*version_info),
                        )
                    }
                    Entry::Vacant(vacant_entry) => {
                        let inserted_version_info = VersionInfo {
                            updated_at_version: query_tracker_ref
                                .snapshot
                                .version,
                            verfied_at_version: query_tracker_ref
                                .snapshot
                                .version,
                            kind: Kind::Derived {
                                defaulted_by_cyclic_dependency: false,
                            },
                            fingerprint: Some(value_fingerprint),
                        };
                        vacant_entry.insert(inserted_version_info);

                        (true, Some(inserted_version_info))
                    }
                };

                // save to the database upon update
                if let Some(per) = self.runtime.persistence.as_ref() {
                    if save_database {
                        unsafe {
                            if let Err(err) = per.save_with_fingerprint::<K>(
                                &value,
                                value_fingerprint,
                            ) {
                                tracing::trace!(
                                    "Failed to save value for key {}: {}",
                                    key.type_name(),
                                    err
                                );
                            }
                        }
                    }

                    if let Some(save_version_info) = save_version_info {
                        if let Err(err) = per.save_version_info::<K>(
                            key_smallbox.fingerprint(),
                            &save_version_info,
                        ) {
                            tracing::trace!(
                                "Failed to save version info for key {}: {}",
                                key.type_name(),
                                err
                            );
                        }
                    }
                }
            }
            Err(_cyclic_error) => {
                query_tracker_ref.version_info_by_keys.insert(
                    DynamicKey(key.smallbox_clone()),
                    VersionInfo {
                        updated_at_version: query_tracker_ref.snapshot.version,
                        verfied_at_version: query_tracker_ref.snapshot.version,
                        kind: Kind::Derived {
                            defaulted_by_cyclic_dependency: true,
                        },
                        fingerprint: None,
                    },
                );

                match query_tracker_ref
                    .version_info_by_keys
                    .entry(DynamicKey(key.smallbox_clone()))
                {
                    Entry::Occupied(occupied_entry) => {
                        let version_info = occupied_entry.into_mut();

                        // must've been marked as cyclic before
                        assert!(
                            version_info.verfied_at_version
                                == query_tracker_ref.snapshot.version
                                && matches!(version_info.kind, Kind::Derived {
                                    defaulted_by_cyclic_dependency: true
                                })
                        );

                        version_info.verfied_at_version =
                            query_tracker_ref.snapshot.version;
                        version_info.updated_at_version =
                            query_tracker_ref.snapshot.version;
                        version_info.kind = Kind::Derived {
                            defaulted_by_cyclic_dependency: true,
                        };
                    }
                    Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(VersionInfo {
                            updated_at_version: query_tracker_ref
                                .snapshot
                                .version,
                            verfied_at_version: query_tracker_ref
                                .snapshot
                                .version,
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

        assert!(query_tracker_ref
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
