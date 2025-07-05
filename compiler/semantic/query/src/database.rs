//! Contains the definition of the [`Database`] struct.

use std::{
    any::Any,
    borrow::Borrow,
    fmt::Debug,
    hash::Hash,
    ops::Deref,
    sync::{
        atomic::{AtomicBool, AtomicU64},
        Arc,
    },
};

use dashmap::DashMap;
use enum_as_inner::EnumAsInner;
use parking_lot::{Condvar, Mutex, RwLock};
use pernixc_arena::ID;
use pernixc_hash::HashSet;
use pernixc_stable_type_id::StableTypeID;
use pernixc_target::Global;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

use crate::{fingerprint, runtime::executor::CyclicError, Engine, Key};

mod input;

pub use input::SetInputLock;

#[derive(Debug, Clone)]
struct Notification(Arc<(Mutex<bool>, Condvar)>);

impl Notification {
    fn new() -> Self { Self(Arc::new((Mutex::new(false), Condvar::new()))) }

    fn wait(&self) {
        let (lock, cvar) = &*self.0;
        let mut notified = lock.lock();
        while !*notified {
            cvar.wait(&mut notified);
        }
    }

    fn notify(&self) {
        let (lock, cvar) = &*self.0;
        let mut notified = lock.lock();
        *notified = true;
        cvar.notify_all();
    }
}

#[derive(Debug)]
struct Running {
    notify: Notification,
    is_in_scc: AtomicBool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub(crate) struct DerivedVersionInfo {
    verified_at: u64,
    updated_at: u64,

    // if `None`, the value is involved in a cycle
    fingerprint: Option<u128>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub(crate) struct DerivedMetadata {
    version_info: DerivedVersionInfo,
    dependencies: HashSet<DynamicKey>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct InputMetadata {
    fingerprint: u128,
    updated_at: u64,
}

#[derive(Debug, EnumAsInner)]
pub(crate) enum ValueMetadata {
    Derived(DerivedMetadata),
    Input(InputMetadata),
}

impl ValueMetadata {
    const fn value_fingerprint(&self) -> Option<u128> {
        match self {
            Self::Derived(derived) => derived.version_info.fingerprint,
            Self::Input(input) => Some(input.fingerprint),
        }
    }

    const fn updated_at(&self) -> u64 {
        match self {
            Self::Derived(derived) => derived.version_info.updated_at,
            Self::Input(input) => input.updated_at,
        }
    }
}

#[derive(Debug)]
struct Completion {
    metadata: ValueMetadata,
    store: Option<Arc<dyn Value>>,
}

#[derive(Debug, EnumAsInner)]
enum State {
    Running(Running),
    Completion(Completion),
}

/// A struct that wraps the [`Engine`] and tracks its dependencies and call
/// stack. These trackings are required by the query system to be able to
/// correctly handle cyclic dependencies and to provide a way to access the
/// dependencies of the engine.
///
/// The [`TrackedEngine`] can be cloned and shared across multiple threads,
/// allowing the query system to run multiple queries in parallel using
/// [`tokio::spawn`].
#[derive(Debug, Clone)]
pub struct TrackedEngine<'e> {
    engine: &'e Engine,
    dependencies: Option<&'e RwLock<HashSet<DynamicKey>>>,
    call_stack: Option<Vec<DynamicKey>>,
}

impl Engine {
    /// Creates a new [`TrackedEngine`] allowing queries to the database.
    pub const fn tracked(&self) -> TrackedEngine {
        TrackedEngine { engine: self, dependencies: None, call_stack: None }
    }
}

impl TrackedEngine<'_> {
    /// Queries the value for the given key.
    ///
    /// # Errors
    ///
    /// Returns an error if the query is part of a cyclic dependency, which
    /// prevents deadlocks in the query system.
    pub fn query<K: Key>(
        &mut self,
        key: &K,
    ) -> Result<Arc<K::Value>, CyclicError> {
        let mut new_call_stack =
            if self.call_stack.is_none() { Some(Vec::new()) } else { None };

        let call_stack = new_call_stack
            .as_mut()
            .unwrap_or_else(|| self.call_stack.as_mut().unwrap());

        let current_version = self
            .engine
            .database
            .version
            .load(std::sync::atomic::Ordering::Relaxed);

        self.engine
            .query_internal(
                key,
                self.dependencies,
                call_stack,
                current_version,
                true,
            )
            .map(|x| x.unwrap())
    }
}

enum FastPathDecision<V> {
    TryAgain,
    ToSlowPath,
    Return(Option<Arc<V>>),
}

enum SlowPathDecision<V> {
    TryAgain,
    Return(Option<Arc<V>>),
    Continuation(Continuation, Notification),
}

struct ReVerify {
    derived_metadata: DerivedMetadata,
    value_store: Option<Arc<dyn Value>>,
}

struct ReExecute {
    derived_metadata: DerivedMetadata,
}

enum HandleCompletion<V> {
    Return(Option<Arc<V>>),
    Continuation(Continuation),
}

enum Continuation {
    Fresh,
    ReVerify(ReVerify),
    ReExecute(ReExecute),
}

/// The main database struct that holds the query states and their versions.
#[derive(Debug, Default)]
pub struct Database {
    query_states_by_key: DashMap<DynamicKey, State>,
    version: AtomicU64,
}

pub(super) fn re_verify_query<'a, K: Key + 'static>(
    engine: &'a Engine,
    key: &'a dyn Any,
    current_version: u64,
    call_stack: &mut Vec<DynamicKey>,
) -> Result<(), CyclicError> {
    let key = key.downcast_ref::<K>().expect("Key type mismatch");

    engine.query_internal(key, None, call_stack, current_version, false)?;

    Ok(())
}

impl Engine {
    fn fast_path<K: Key>(
        &self,
        key: &K,
        return_value: bool,
        call_stack: &[DynamicKey],
        current_version: u64,
    ) -> Result<FastPathDecision<K::Value>, CyclicError> {
        if let Some(state) =
            self.database.query_states_by_key.get(key as &dyn Dynamic)
        {
            match state.value() {
                State::Running(running) => {
                    // check if the query is cyclic, preventing deadlocks
                    let index =
                        call_stack.iter().position(|x| x.0.deref().eq(key));

                    // found cyclic dependency, mark all the SCCs
                    if let Some(index) = index {
                        for key in &call_stack[index..] {
                            self.database
                                .query_states_by_key
                                .get(&*key.0 as &dyn Dynamic)
                                .expect("should be present")
                                .as_running()
                                .expect(
                                    "should be running since it appeared in \
                                     the call stack",
                                )
                                .is_in_scc
                                .store(
                                    true,
                                    std::sync::atomic::Ordering::Relaxed,
                                );
                        }

                        return Err(CyclicError);
                    }

                    let notify = running.notify.clone();
                    drop(state);

                    notify.wait();

                    // try again and should see the `State::Completion`
                    return Ok(FastPathDecision::TryAgain);
                }
                State::Completion(completion) => {
                    // check if the value is up-to-date
                    if completion.metadata.as_derived().is_none_or(|x| {
                        x.version_info.verified_at == current_version
                    }) {
                        if return_value {
                            match completion.store.as_ref() {
                                Some(v) => {
                                    let arc_any = v.clone()
                                        as Arc<dyn Any + Send + Sync + 'static>;

                                    return Ok(FastPathDecision::Return(Some(
                                        arc_any
                                            .downcast()
                                            .expect("Failed to downcast value"),
                                    )));
                                }

                                None => {
                                    // let's the value is not stored, might be
                                    // that the value hasn't been loaded from
                                    // the persistent storage or the query might
                                    // need to be re-executed, let's the
                                    // slow-path handle it
                                    return Ok(FastPathDecision::ToSlowPath);
                                }
                            }
                        }

                        // doesn't require the value and the version is
                        // verified, nothing to do left, return now
                        return Ok(FastPathDecision::Return(None));
                    }

                    // the value is not up-to-date, let's the slow-path
                    // re-verify the value and update it if
                    // needed
                    return Ok(FastPathDecision::ToSlowPath);
                }
            }
        }

        // the query hasn't been computed or loaded yet, go to the slow path
        // that obtains the write lock.
        Ok(FastPathDecision::ToSlowPath)
    }

    fn handle_completion<K: Key>(
        &self,
        completion: &mut Completion,
        current_version: u64,
        return_value: bool,
    ) -> HandleCompletion<K::Value> {
        if completion
            .metadata
            .as_derived()
            .is_none_or(|x| x.version_info.verified_at == current_version)
        {
            if return_value {
                if let Some(v) = completion.store.as_ref() {
                    let arc_any =
                        v.clone() as Arc<dyn Any + Send + Sync + 'static>;

                    return HandleCompletion::Return(Some(
                        arc_any.downcast().expect("Failed to downcast value"),
                    ));
                }
                let value =
                    completion.metadata.value_fingerprint().map_or_else(
                        || Some(K::scc_value()),
                        |fingerprint| {
                            self.try_load_value::<K>(fingerprint).map(Arc::new)
                        },
                    );

                // successfully loaded the value, store in the cache,
                // return it
                if let Some(value) = value {
                    completion.store = Some(value.clone());
                    return HandleCompletion::Return(Some(value));
                }

                // the value is not found in the persistent storage and
                // in-memory storage,
                // need to re-execute the query
                return HandleCompletion::Continuation(
                    match &mut completion.metadata {
                        ValueMetadata::Derived(derived_metadata) => {
                            Continuation::ReExecute(ReExecute {
                                derived_metadata: DerivedMetadata {
                                    dependencies: std::mem::take(
                                        &mut derived_metadata.dependencies,
                                    ),
                                    version_info: derived_metadata.version_info,
                                },
                            })
                        }

                        // somehow, the value is an input but not found in the
                        // in-memory and persistent storage, we'll try to obtain
                        // the query a derived value.
                        ValueMetadata::Input(_) => Continuation::Fresh,
                    },
                );
            }

            // the value is up-to-date and doesn't require the value,
            // nothing to do left, return now
            return HandleCompletion::Return(None);
        }

        // need to re-verify the value, check if the dependencies are
        let derived_metadata = std::mem::take(
            completion
                .metadata
                .as_derived_mut()
                .expect("should have been a derived value variant"),
        );

        // re-verify the value with the dependencies
        HandleCompletion::Continuation(Continuation::ReVerify(ReVerify {
            derived_metadata,
            value_store: std::mem::take(&mut completion.store),
        }))
    }

    #[allow(clippy::too_many_lines)]
    fn slow_path<K: Key>(
        &self,
        key: &K,
        current_version: u64,
        return_value: bool,
    ) -> SlowPathDecision<K::Value> {
        match self
            .database
            .query_states_by_key
            .entry(DynamicKey(key.smallbox_clone()))
        {
            dashmap::Entry::Occupied(mut occupied_entry) => {
                match occupied_entry.get_mut() {
                    // go back to the fast path if the state is running
                    // and wait for the notification to be sent
                    State::Running(_) => SlowPathDecision::TryAgain,

                    State::Completion(completion) => {
                        let continuation = match self.handle_completion::<K>(
                            completion,
                            current_version,
                            return_value,
                        ) {
                            HandleCompletion::Return(value) => {
                                return SlowPathDecision::Return(value)
                            }
                            HandleCompletion::Continuation(continuation) => {
                                continuation
                            }
                        };

                        let notify = Notification::new();

                        *occupied_entry.get_mut() = State::Running(Running {
                            notify: notify.clone(),
                            is_in_scc: AtomicBool::new(false),
                        });

                        SlowPathDecision::Continuation(continuation, notify)
                    }
                }
            }

            dashmap::Entry::Vacant(vacant_entry) => {
                let fingerprint = key.fingerprint();

                // try loading the version from the persistent storage
                let loaded_metadata =
                    self.try_load_value_metadata::<K>(fingerprint);

                if let Some(loaded_metadata) = loaded_metadata {
                    if loaded_metadata.as_derived().is_none_or(|x| {
                        x.version_info.verified_at == current_version
                    }) {
                        if return_value {
                            let value = loaded_metadata
                                .value_fingerprint()
                                .map_or_else(
                                    || Some(K::scc_value()),
                                    |fingerprint| {
                                        self.try_load_value::<K>(fingerprint)
                                            .map(Arc::new)
                                    },
                                );

                            if let Some(value) = value {
                                // store the value in the cache
                                vacant_entry.insert(State::Completion(
                                    Completion {
                                        metadata: loaded_metadata,
                                        store: Some(value.clone()),
                                    },
                                ));

                                return SlowPathDecision::Return(Some(value));
                            }

                            let notify = Notification::new();
                            vacant_entry.insert(State::Running(Running {
                                notify: notify.clone(),
                                is_in_scc: AtomicBool::new(false),
                            }));

                            return SlowPathDecision::Continuation(
                                match loaded_metadata {
                                    ValueMetadata::Derived(
                                        derived_metadata,
                                    ) => Continuation::ReExecute(ReExecute {
                                        derived_metadata,
                                    }),

                                    // fallback to a fresh query if the input
                                    // value can't be found
                                    ValueMetadata::Input(_) => {
                                        Continuation::Fresh
                                    }
                                },
                                notify,
                            );
                        }

                        vacant_entry.insert(State::Completion(Completion {
                            metadata: loaded_metadata,
                            store: None,
                        }));

                        // doesn't need the value, just return
                        return SlowPathDecision::Return(None);
                    }

                    let notify = Notification::new();
                    vacant_entry.insert(State::Running(Running {
                        notify: notify.clone(),
                        is_in_scc: AtomicBool::new(false),
                    }));

                    // the value is not up-to-date, need to re-verify it
                    return SlowPathDecision::Continuation(
                        Continuation::ReVerify(ReVerify {
                            derived_metadata: loaded_metadata
                                .as_derived()
                                .cloned()
                                .expect("should be a derived value variant"),
                            value_store: None,
                        }),
                        notify,
                    );
                }

                // no version found, need to create a fresh query
                let notify = Notification::new();
                vacant_entry.insert(State::Running(Running {
                    notify: notify.clone(),
                    is_in_scc: AtomicBool::new(false),
                }));

                SlowPathDecision::Continuation(Continuation::Fresh, notify)
            }
        }
    }

    fn compute_query<K: Key>(
        tracked_engine: &mut TrackedEngine,
        key: &K,
    ) -> Result<Arc<dyn Value>, CyclicError> {
        let (executor, invoke) = tracked_engine
            .engine
            .runtime
            .executor
            .get_entry::<K>()
            .map_or_else(
                || {
                    panic!(
                        "No executor registered for key type `{}`",
                        std::any::type_name::<K>()
                    )
                },
                |x| {
                    let value = x.value();
                    (value.get_any_executor(), x.get_invoke_executor())
                },
            );

        (invoke)(key as &dyn Any, executor.as_ref(), tracked_engine)
    }

    fn compute<K: Key>(
        &self,
        key: &K,
        track_dependencies: bool,
        call_stack: &mut Vec<DynamicKey>,
        return_value: bool,
        set_completed: impl FnOnce(
            Result<&K::Value, CyclicError>,
            Option<HashSet<DynamicKey>>,
        ) -> DerivedMetadata,
    ) -> Option<Arc<K::Value>> {
        let original_call_stack_len = call_stack.len();

        // add the key to the call stack
        call_stack.push(DynamicKey(key.smallbox_clone()));

        let tracked_dependencies =
            track_dependencies.then(|| RwLock::new(HashSet::default()));

        let mut tracked_engine = TrackedEngine {
            engine: self,
            dependencies: tracked_dependencies.as_ref(),
            call_stack: Some(std::mem::take(call_stack)),
        };

        let value = Self::compute_query(&mut tracked_engine, key);

        {
            // remove the key from the call stack.
            *call_stack = tracked_engine.call_stack.take().unwrap();
            call_stack.pop();

            assert_eq!(
                call_stack.len(),
                original_call_stack_len,
                "Call stack length mismatch after query computation",
            );
        }

        let is_in_scc = self
            .database
            .query_states_by_key
            .get(key as &dyn Dynamic)
            .expect("should be present with running state")
            .as_running()
            .expect("should be running since it appeared in the call stack")
            .is_in_scc
            .load(std::sync::atomic::Ordering::Relaxed);

        // if `is_in_scc` is `true`, it means that the query is part of a
        // strongly connected component (SCC) and the value should be an error,
        // otherwise, it should be a valid value.
        assert_eq!(
            is_in_scc,
            value.is_err(),
            "Cyclic dependency state mismatch: expected {}, got {}",
            value.is_err(),
            is_in_scc
        );

        self.handle_computed_value(key, value, return_value, |result| {
            set_completed(result, tracked_dependencies.map(RwLock::into_inner))
        })
    }

    #[allow(clippy::needless_pass_by_value)]
    fn handle_computed_value<K: Key>(
        &self,
        key: &K,
        boxed_value: Result<Arc<dyn Value>, CyclicError>,
        return_value: bool,
        set_completed: impl FnOnce(
            Result<&K::Value, CyclicError>,
        ) -> DerivedMetadata,
    ) -> Option<Arc<K::Value>> {
        let metadata = set_completed(
            boxed_value
                .as_ref()
                .ok()
                .map(|x| {
                    let value = x.as_ref();
                    let any = value as &dyn Any;

                    any.downcast_ref::<K::Value>()
                        .expect("Failed to downcast value")
                })
                .ok_or(CyclicError),
        );

        let final_value =
            boxed_value.as_ref().cloned().unwrap_or_else(|_| K::scc_value());

        self.database.query_states_by_key.insert(
            DynamicKey(key.smallbox_clone()),
            State::Completion(Completion {
                metadata: ValueMetadata::Derived(metadata),
                store: Some(final_value.clone()),
            }),
        );

        if return_value {
            let any = final_value as Arc<dyn Any + Send + Sync + 'static>;
            Some(any.downcast().unwrap())
        } else {
            None
        }
    }

    #[allow(clippy::too_many_lines)]
    fn continuation<K: Key>(
        &self,
        key: &K,
        continuation: Continuation,
        call_stack: &mut Vec<DynamicKey>,
        return_value: bool,
        current_version: u64,
    ) -> Option<Arc<K::Value>> {
        match continuation {
            Continuation::Fresh => {
                // fresh query, compute the value from scratch
                self.compute(
                    key,
                    true,
                    call_stack,
                    return_value,
                    |result, tracked_dependencies| DerivedMetadata {
                        version_info: DerivedVersionInfo {
                            verified_at: current_version,
                            updated_at: current_version,
                            fingerprint: result
                                .ok()
                                .map(fingerprint::fingerprint),
                        },
                        dependencies: tracked_dependencies.unwrap(),
                    },
                )
            }
            Continuation::ReVerify(mut re_verify) => {
                let recompute = if re_verify
                    .derived_metadata
                    .version_info
                    .fingerprint
                    .is_none()
                {
                    // if the query is a part of SCC, always recompute
                    true
                } else {
                    re_verify
                        .derived_metadata
                        .dependencies
                        .par_iter()
                        .for_each(|x| {
                            // if the dependency is an input, skip
                            // re-verification
                            if self
                                .database
                                .query_states_by_key
                                .get(x)
                                .is_some_and(|x| {
                                    x.as_completion()
                                        .is_some_and(|x| x.metadata.is_input())
                                })
                            {
                                return;
                            }

                            let mut call_stack_clone = call_stack.clone();

                            let dep_ref = &*x.0;
                            let type_id = dep_ref.any().type_id();

                            let re_verify_query = self
                                .runtime
                                .executor
                                .get_entry_with_id(type_id)
                                .unwrap_or_else(|| {
                                    panic!(
                                        "No executor registered for key type \
                                         `{}`",
                                        dep_ref.type_name()
                                    )
                                })
                                .get_re_verify_query();

                            let _ = (re_verify_query)(
                                self,
                                dep_ref.any(),
                                current_version,
                                &mut call_stack_clone,
                            );
                        });

                    // if any of the dependencies has been updated,
                    // we need to recompute the query
                    re_verify.derived_metadata.dependencies.iter().any(|x| {
                        self.database
                            .query_states_by_key
                            .get(x)
                            .expect("should be present")
                            .as_completion()
                            .expect(
                                "should be completion since it was re-verified",
                            )
                            .metadata
                            .updated_at()
                            > re_verify
                                .derived_metadata
                                .version_info
                                .verified_at
                    })
                };

                if recompute {
                    self.compute(
                        key,
                        true,
                        call_stack,
                        true,
                        |value, tracked_dependencies| {
                            let new_fingerprint =
                                value.ok().map(fingerprint::fingerprint);

                            // if the new fingerprint is different from
                            // the old one, update the value version
                            if re_verify
                                .derived_metadata
                                .version_info
                                .fingerprint
                                .is_none_or(|x| Some(x) != new_fingerprint)
                            {
                                re_verify
                                    .derived_metadata
                                    .version_info
                                    .fingerprint = new_fingerprint;
                                re_verify
                                    .derived_metadata
                                    .version_info
                                    .updated_at = current_version;
                            }

                            re_verify
                                .derived_metadata
                                .version_info
                                .verified_at = current_version;
                            re_verify.derived_metadata.dependencies =
                                tracked_dependencies.unwrap();

                            re_verify.derived_metadata
                        },
                    )
                } else if return_value {
                    let value = match re_verify.value_store {
                        Some(value) => {
                            self.database.query_states_by_key.insert(
                                DynamicKey(key.smallbox_clone()),
                                State::Completion(Completion {
                                    metadata: ValueMetadata::Derived(
                                        re_verify.derived_metadata,
                                    ),
                                    store: Some(value.clone()),
                                }),
                            );

                            return Some({
                                let arc_any = value
                                    as Arc<dyn Any + Send + Sync + 'static>;

                                arc_any
                                    .downcast()
                                    .expect("Failed to downcast value")
                            });
                        }
                        None => re_verify
                            .derived_metadata
                            .version_info
                            .fingerprint
                            .map_or_else(
                                || Some(Err(CyclicError)),
                                |fingerprint| {
                                    self.try_load_value::<K>(fingerprint).map(
                                        |x| Ok(Arc::new(x) as Arc<dyn Value>),
                                    )
                                },
                            ),
                    };

                    match value {
                        Some(value) => self.handle_computed_value(
                            key,
                            value,
                            return_value,
                            |result| {
                                debug_assert_eq!(
                                    result.ok().map(|x| {
                                        fingerprint::fingerprint(x)
                                    }),
                                    re_verify
                                        .derived_metadata
                                        .version_info
                                        .fingerprint,
                                    "The fingerprint of the re-executed value \
                                     should match the old one"
                                );

                                re_verify.derived_metadata
                            },
                        ),
                        None => self.compute(
                            key,
                            false,
                            call_stack,
                            true,
                            |value, _| {
                                debug_assert_eq!(
                                    value.ok().map(|x| {
                                        fingerprint::fingerprint(x)
                                    }),
                                    re_verify
                                        .derived_metadata
                                        .version_info
                                        .fingerprint,
                                    "The fingerprint of the re-executed value \
                                     should match the old one"
                                );

                                re_verify.derived_metadata
                            },
                        ),
                    }
                } else {
                    *self
                        .database
                        .query_states_by_key
                        .get_mut(key as &dyn Dynamic)
                        .expect("should be present") =
                        State::Completion(Completion {
                            metadata: ValueMetadata::Derived(
                                re_verify.derived_metadata,
                            ),
                            store: re_verify.value_store,
                        });

                    None
                }
            }
            Continuation::ReExecute(re_execute) => self.compute(
                key,
                false,
                call_stack,
                return_value,
                |result, _| {
                    debug_assert_eq!(
                        result.ok().map(|x| { fingerprint::fingerprint(x) }),
                        re_execute.derived_metadata.version_info.fingerprint,
                        "The fingerprint of the re-executed value should \
                         match the old one"
                    );

                    re_execute.derived_metadata
                },
            ),
        }
    }

    pub(super) fn query_internal<K: Key>(
        &self,
        key: &K,
        dependencies: Option<&RwLock<HashSet<DynamicKey>>>,
        call_stack: &mut Vec<DynamicKey>,
        current_version: u64,
        return_value: bool,
    ) -> Result<Option<Arc<K::Value>>, CyclicError> {
        // insert to the dependencies list if required
        if let Some(dependencies) = dependencies {
            let mut write = dependencies.write();

            if !write.contains(key as &dyn Dynamic) {
                write.insert(DynamicKey(key.smallbox_clone()));
            }
        }

        loop {
            // Fast path: mostly used read lock, lower lock contention
            match self.fast_path(
                key,
                return_value,
                call_stack,
                current_version,
            )? {
                FastPathDecision::TryAgain => continue,
                FastPathDecision::ToSlowPath => {}
                FastPathDecision::Return(value) => return Ok(value),
            }

            // Slow Path: use `entry` obtaining a write lock for state mutation
            let (continuation, notify) =
                match self.slow_path(key, current_version, return_value) {
                    SlowPathDecision::TryAgain => continue,
                    SlowPathDecision::Return(value) => return Ok(value),
                    SlowPathDecision::Continuation(continuation, notify) => {
                        (continuation, notify)
                    }
                };

            let value = self.continuation(
                key,
                continuation,
                call_stack,
                return_value,
                current_version,
            );

            // notify the waiting tasks that the query has been completed
            notify.notify();

            if let Some(called_from) = call_stack.last() {
                let is_in_scc = self
                    .database
                    .query_states_by_key
                    .get(&*called_from.0 as &dyn Dynamic)
                    .expect("should be present with running state")
                    .as_running()
                    .expect(
                        "should be running since it appeared in the call stack",
                    )
                    .is_in_scc
                    .load(std::sync::atomic::Ordering::Relaxed);

                if is_in_scc {
                    return Err(CyclicError);
                }
            }

            return Ok(value);
        }
    }

    /// Returns the current version of the query database.
    pub fn version(&self) -> u64 {
        self.database.version.load(std::sync::atomic::Ordering::Relaxed)
    }
}

/// A type alias for a [`smallbox::SmallBox`] with a [`Global<ID<()>>`] as its
/// size for the local storage.
///
/// This smallbox is used mainly for performance reasons to avoid heap
/// allocation (premature optimization?). Since most of the queries are just
/// global IDs, the [`Global<ID<()>>`] should be enough to store the data
/// without allocating a heap object.
type KeySmallBox<T> = smallbox::SmallBox<T, Global<ID<()>>>;

/// A trait allowing store multiple types of as a key in a hashmap. This is
/// automatically implemented for all types that implement the [`Key`] trait.
#[doc(hidden)]
trait Dynamic: 'static + Send + Sync + Debug {
    #[doc(hidden)]
    fn any(&self) -> &dyn std::any::Any;
    #[doc(hidden)]
    fn eq(&self, other: &dyn Dynamic) -> bool;
    #[doc(hidden)]
    fn hash(&self, state: &mut dyn std::hash::Hasher);
    #[doc(hidden)]
    fn fingerprint(&self) -> u128;
    #[doc(hidden)]
    fn smallbox_clone(&self) -> KeySmallBox<dyn Dynamic>;
    #[allow(unused)]
    #[doc(hidden)]
    fn stable_type_id(&self) -> StableTypeID;
    #[doc(hidden)]
    fn type_name(&self) -> &'static str;
}

/// A new type wrapper around [`KeySmallBox<dyn Dynamic>`] that allows it to be
/// serializable and deserializable.
#[derive(Debug, PartialEq, Eq, Hash)]
pub(super) struct DynamicKey(KeySmallBox<dyn Dynamic>);

impl Borrow<dyn Dynamic> for DynamicKey {
    fn borrow(&self) -> &dyn Dynamic { &*self.0 }
}

impl Borrow<KeySmallBox<dyn Dynamic>> for DynamicKey {
    fn borrow(&self) -> &KeySmallBox<dyn Dynamic> { &self.0 }
}

impl Clone for DynamicKey {
    fn clone(&self) -> Self { Self(self.0.smallbox_clone()) }
}

impl<K: Key> Dynamic for K {
    fn any(&self) -> &dyn std::any::Any { self as &dyn std::any::Any }

    fn eq(&self, other: &dyn Dynamic) -> bool {
        other.any().downcast_ref::<Self>().is_some_and(|other| self.eq(other))
    }

    fn hash(&self, mut state: &mut dyn std::hash::Hasher) {
        let id = std::any::TypeId::of::<Self>();

        id.hash(&mut state);
        Hash::hash(self, &mut state);
    }

    fn smallbox_clone(&self) -> KeySmallBox<dyn Dynamic> {
        smallbox::smallbox!(self.clone())
    }

    fn stable_type_id(&self) -> StableTypeID { Self::STABLE_TYPE_ID }

    fn type_name(&self) -> &'static str { std::any::type_name::<Self>() }

    fn fingerprint(&self) -> u128 { fingerprint::fingerprint(self) }
}

impl PartialEq for dyn Dynamic + '_ {
    fn eq(&self, other: &Self) -> bool { Dynamic::eq(self, other) }
}

impl Eq for dyn Dynamic + '_ {}

impl Hash for dyn Dynamic + '_ {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Dynamic::hash(self, state);
    }
}

// A trait allowing to store values in the query system.
pub(super) trait Value:
    Any + Send + Sync + std::fmt::Debug + 'static
{
}

impl<T: Any + Send + Sync + std::fmt::Debug + 'static> Value for T {}

#[cfg(test)]
mod test;
