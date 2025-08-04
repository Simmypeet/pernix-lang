//! Contains the definition of the [`Database`] struct,

use std::{
    any::Any,
    borrow::Borrow,
    fmt::Debug,
    hash::Hash,
    pin::Pin,
    sync::{
        atomic::{AtomicBool, AtomicU64},
        Arc,
    },
};

use dashmap::{DashMap, DashSet};
use enum_as_inner::EnumAsInner;
use getset::CopyGetters;
use parking_lot::RwLock;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_type_id::StableTypeID;
use rand::Rng;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use tokio::sync::Notify;
use tracing::instrument;

use crate::{
    fingerprint,
    runtime::{
        executor::{self, CyclicError},
        persistence::serde::{DynamicDeserialize, DynamicSerialize},
    },
    Engine, Key,
};

mod input;

pub use input::{SetInputLock, SetInputResult};

#[derive(Debug)]
struct Running {
    notify: Arc<Notify>,
    dependencies_order: RwLock<Vec<DynamicKey>>,
    dependencies_set: DashSet<DynamicKey>,
    is_in_scc: AtomicBool,
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize,
)]
pub(crate) struct DerivedVersionInfo {
    verified_at: u64,
    updated_at: u64,

    // if `None`, the value is involved in a cycle
    fingerprint: Option<u128>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(ser_extension(DynamicSerialize<__S>), de_extension(DynamicDeserialize<__D>))]
pub(crate) struct DerivedMetadata {
    version_info: DerivedVersionInfo,
    dependencies: Arc<[DynamicKey]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub(crate) struct InputMetadata {
    fingerprint: u128,
    updated_at: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize, EnumAsInner)]
#[serde(ser_extension(DynamicSerialize<__S>), de_extension(DynamicDeserialize<__D>))]
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

/// A boxed type that can hold dynamic value. This is optimized for storing the
/// arc inline. Since most of the time, the query will return `Arc` values,
/// therefore, if we were to use regular `Box` values, it would introduce
/// unnecessary indirection via `Box<Arc<QueryResult>>`.
pub(crate) type DynamicValue = smallbox::SmallBox<dyn Value, Arc<dyn Value>>;

#[derive(Debug)]
struct Completion {
    metadata: ValueMetadata,
    store: Option<DynamicValue>,
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
pub struct TrackedEngine {
    // CONSIDER: Should we add another cache layer within the `TrackedEngine`
    // avoiding to call the `query_internal` method multiple times for the same
    // key?
    engine: Arc<Engine>,
    called_from: Option<DynamicKey>,

    cache: Option<Arc<DashMap<DynamicKey, DynamicValue>>>,
}

static_assertions::assert_impl_all!(TrackedEngine: Send, Sync);

impl Engine {
    /// Creates a new [`TrackedEngine`] allowing queries to the database.
    pub fn tracked(self: &Arc<Self>) -> TrackedEngine {
        TrackedEngine { engine: self.clone(), called_from: None, cache: None }
    }
}

impl TrackedEngine {
    /// Queries the value for the given key.
    ///
    /// # Errors
    ///
    /// Returns an error if the query is part of a cyclic dependency, which
    /// prevents deadlocks in the query system.
    pub async fn query<K: Key>(
        &self,
        key: &K,
    ) -> Result<K::Value, CyclicError> {
        if let Some(cache) = self.cache.as_ref() {
            if let Some(value) = cache.get(key as &dyn Dynamic) {
                let value = (&**value.value() as &dyn Any)
                    .downcast_ref::<K::Value>()
                    .expect("Failed to downcast value")
                    .clone();

                return Ok(value);
            }
        }

        let current_version = self
            .engine
            .database
            .version
            .load(std::sync::atomic::Ordering::Relaxed);

        let value = self
            .engine
            .query_internal(
                key,
                self.called_from.as_ref().map(|x| &*x.0 as &dyn Dynamic),
                current_version,
                true,
            )
            .await
            .map(|x| x.unwrap())?;

        if let Some(cache) = self.cache.as_ref() {
            cache.insert(
                DynamicKey(smallbox::smallbox!(key.clone())),
                smallbox::smallbox!(value.clone()),
            );
        }

        Ok(value)
    }
}

enum FastPathDecision<V> {
    TryAgain,
    ToSlowPath,
    Return(Option<V>),
}

enum SlowPathDecision<V> {
    TryAgain,
    Return(Option<V>),
    Continuation(Continuation, Arc<Notify>),
}

#[derive(Debug)]
struct ReVerify {
    derived_metadata: DerivedMetadata,
    value_store: Option<DynamicValue>,
}

#[derive(Debug)]
struct ReExecute {
    derived_metadata: DerivedMetadata,
}

enum HandleCompletion<V> {
    Return(Option<V>),
    Continuation(Continuation),
}

#[derive(Debug)]
enum Continuation {
    Fresh,
    ReVerify(ReVerify),
    ReExecute(ReExecute),
}

/// The main database struct that holds the query states and their versions.
#[derive(Debug, CopyGetters)]
pub struct Database {
    query_states_by_key: DashMap<DynamicKey, State>,

    /// The random seed primiarily used for the initial state of any hashing
    /// and fingerprinting operations.
    ///
    /// In incremental compilation setting, the random seed is also saved and
    /// loaded from the persistentst storage, ensuring deterministic behavior
    /// across different runs of the compiler.
    #[get_copy = "pub"]
    random_seed: u64,

    version: AtomicU64,
}

impl Default for Database {
    fn default() -> Self {
        Self {
            query_states_by_key: DashMap::default(),
            random_seed: rand::thread_rng().gen(),
            version: AtomicU64::new(0),
        }
    }
}

impl Database {
    /// Creates a new empty database with the given version.
    #[must_use]
    pub fn with_state(random_seed: u64, version: u64) -> Self {
        Self {
            query_states_by_key: DashMap::default(),
            random_seed,
            version: AtomicU64::new(version),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct SaveConfig {
    save_value: bool,
    save_metadata: bool,
}

pub(super) fn re_verify_query<'a, K: Key + 'static>(
    engine: &'a Arc<Engine>,
    key: &'a dyn Any,
    current_version: u64,
    called_from: &'a dyn Dynamic,
) -> Pin<Box<dyn executor::Future<'a, ()> + 'a>> {
    let key = key.downcast_ref::<K>().expect("Key type mismatch");

    Box::pin(async move {
        engine
            .query_internal(key, Some(called_from), current_version, false)
            .await?;

        Ok(())
    })
}

impl Engine {
    fn check_cyclic(
        &self,
        running_state: &Running,
        target: &dyn Dynamic,
    ) -> bool {
        if running_state.dependencies_set.contains(target) {
            running_state
                .is_in_scc
                .store(true, std::sync::atomic::Ordering::Relaxed);

            return true;
        }

        let mut found = false;

        for dep in running_state.dependencies_set.iter() {
            let Some(state) = self.database.query_states_by_key.get(&*dep.0)
            else {
                continue;
            };

            let Some(running) = state.as_running() else {
                continue;
            };

            found |= self.check_cyclic(running, target);
        }

        if found {
            running_state
                .is_in_scc
                .store(true, std::sync::atomic::Ordering::Relaxed);
        }

        found
    }

    async fn fast_path<K: Key>(
        &self,
        key: &K,
        called_from: Option<&dyn Dynamic>,
        return_value: bool,
        current_version: u64,
    ) -> Result<FastPathDecision<K::Value>, CyclicError> {
        if let Some(state) =
            self.database.query_states_by_key.get(key as &dyn Dynamic)
        {
            match state.value() {
                State::Running(running) => {
                    let notify = running.notify.clone();

                    if let Some(called_from) = called_from {
                        let is_in_scc = self.check_cyclic(running, called_from);

                        if is_in_scc {
                            let called_from_state = self
                                .database
                                .query_states_by_key
                                .get(called_from as &dyn Dynamic)
                                .unwrap();

                            // mark the `called_from` state as being in SCC
                            called_from_state
                                .as_running()
                                .unwrap()
                                .is_in_scc
                                .store(
                                    true,
                                    std::sync::atomic::Ordering::Relaxed,
                                );

                            return Err(CyclicError);
                        }
                    }

                    // IMPORTANT: add the current thread to the waiter list
                    // first before dropping the state read lock to avoid the
                    // notification being sent before the thread is added to the
                    // waiters list.
                    let notified = notify.notified();

                    // drop the read lock to allow the thread that is computing
                    // the query to access the state and notify the waiters.
                    drop(state);

                    tracing::debug!(
                        "Fast path `{}` `{:?}` is waiting for notification \
                         from `{}` `{:?}`",
                        called_from
                            .as_ref()
                            .map_or_else(|| "None", |x| x.type_name()),
                        called_from,
                        key.type_name(),
                        key
                    );

                    // wait for the notification to be sent. this yields the
                    // current thread and allows the thread that is computing
                    // the query to notify the waiters. this current thread is
                    // yielded, allowing the tokio runtime to schedule other
                    // tasks and avoid blocking the thread pool.
                    notified.await;

                    tracing::debug!(
                        "Fast path `{}` `{:?}` received notification from \
                         `{}` `{:?}`",
                        called_from
                            .as_ref()
                            .map_or_else(|| "None", |x| x.type_name()),
                        called_from,
                        key.type_name(),
                        key
                    );

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
                                    let any = &**v as &dyn Any;

                                    return Ok(FastPathDecision::Return(Some(
                                        any.downcast_ref::<K::Value>()
                                            .expect("Failed to downcast value")
                                            .clone(),
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
                    let any = &**v as &dyn Any;

                    return HandleCompletion::Return(Some(
                        any.downcast_ref::<K::Value>()
                            .expect("Failed to downcast value")
                            .clone(),
                    ));
                }
                let value =
                    completion.metadata.value_fingerprint().map_or_else(
                        || Some(K::scc_value()),
                        |fingerprint| self.try_load_value::<K>(fingerprint),
                    );

                // successfully loaded the value, store in the cache,
                // return it
                if let Some(value) = value {
                    completion.store = Some(smallbox::smallbox!(value.clone()));
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

                        let notify = Arc::new(Notify::new());

                        *occupied_entry.get_mut() = State::Running(Running {
                            notify: notify.clone(),
                            dependencies_order: RwLock::default(),
                            dependencies_set: DashSet::default(),
                            is_in_scc: AtomicBool::new(false),
                        });

                        SlowPathDecision::Continuation(continuation, notify)
                    }
                }
            }

            dashmap::Entry::Vacant(vacant_entry) => {
                let fingerprint = key.fingerprint(self.database.random_seed);

                // try loading the version from the persistent storage
                let loaded_metadata =
                    self.try_load_value_metadata::<K>(fingerprint);

                tracing::debug!(
                    "Loaded metadata for `{}`: {:?}",
                    key.type_name(),
                    loaded_metadata
                );

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
                                    },
                                );

                            if let Some(value) = value {
                                // store the value in the cache
                                vacant_entry.insert(State::Completion(
                                    Completion {
                                        metadata: loaded_metadata,
                                        store: Some(smallbox::smallbox!(
                                            value.clone()
                                        )),
                                    },
                                ));

                                return SlowPathDecision::Return(Some(value));
                            }

                            let notify = Arc::new(Notify::new());
                            vacant_entry.insert(State::Running(Running {
                                notify: notify.clone(),
                                dependencies_order: RwLock::default(),
                                dependencies_set: DashSet::default(),
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

                    let notify = Arc::new(Notify::new());
                    vacant_entry.insert(State::Running(Running {
                        notify: notify.clone(),
                        dependencies_order: RwLock::default(),
                        dependencies_set: DashSet::default(),
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
                let notify = Arc::new(Notify::new());
                vacant_entry.insert(State::Running(Running {
                    notify: notify.clone(),
                    dependencies_order: RwLock::default(),
                    dependencies_set: DashSet::default(),
                    is_in_scc: AtomicBool::new(false),
                }));

                tracing::debug!(
                    "Fresh query for `{}` with metadata: {:?}",
                    key.type_name(),
                    loaded_metadata
                );
                SlowPathDecision::Continuation(Continuation::Fresh, notify)
            }
        }
    }

    #[instrument(
        fields(
            key_name = std::any::type_name::<K>(),
            key = ?key
        ),
        level = "info",
        skip_all
    )]
    async fn compute_query<K: Key>(
        tracked_engine: &mut TrackedEngine,
        key: &K,
    ) -> Result<DynamicValue, CyclicError> {
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

        (invoke)(key as &dyn Any, executor.as_ref(), tracked_engine).await
    }

    async fn compute<K: Key>(
        self: &Arc<Self>,
        key: &K,
        return_value: bool,
        set_completed: impl FnOnce(
            Result<&K::Value, CyclicError>,
            Vec<DynamicKey>,
        ) -> (DerivedMetadata, bool),
    ) -> Option<K::Value> {
        let cache = Arc::new(DashMap::default());

        let mut tracked_engine = TrackedEngine {
            engine: self.clone(),
            called_from: Some(DynamicKey(key.smallbox_clone())),
            cache: Some(cache),
        };

        // use the `cache`'s strong count to determine if the tracked engine
        // is still held elsewhere other than the current call stack.
        //
        // if there're still references to the `TrackedEngine`, it means that
        // there's some dangling references to the `TrackedEngine` on some
        // other threads that the implementation of the query is not aware of.
        //
        // in this case, we'll panic to avoid silent bugs in the query
        // implementation.
        assert!(
            Arc::strong_count(tracked_engine.cache.as_ref().unwrap()) == 1,
            "`TrackedEngine` is still held elsewhere, this is a bug in the \
             query implementation which violates the query system's contract. \
             It's possible that the `TrackedEngine` is being sent to a
             different thread and the query implementation hasn't properly
             joined the thread before returning the value. Key: `{}`",
            key.type_name()
        );

        // make sure that the dependencies that are added by re-verification
        // are not added to the current query
        {
            let mut running = self
                .database
                .query_states_by_key
                .get_mut(key as &dyn Dynamic)
                .unwrap();

            let running = running.as_running_mut().expect(
                "should be running since it appeared in the call stack",
            );

            running.dependencies_order.get_mut().clear();
            running.dependencies_set.clear();
        }

        let value = Self::compute_query(&mut tracked_engine, key).await;

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
            let result = set_completed(
                result,
                std::mem::take(
                    &mut self
                        .database
                        .query_states_by_key
                        .get_mut(key as &dyn Dynamic)
                        .unwrap()
                        .as_running_mut()
                        .unwrap()
                        .dependencies_order
                        .write(),
                ),
            );

            (result.0, SaveConfig { save_value: true, save_metadata: result.1 })
        })
    }

    #[allow(clippy::needless_pass_by_value)]
    fn handle_computed_value<K: Key>(
        &self,
        key: &K,
        boxed_value: Result<DynamicValue, CyclicError>,
        return_value: bool,
        set_completed: impl FnOnce(
            Result<&K::Value, CyclicError>,
        ) -> (DerivedMetadata, SaveConfig),
    ) -> Option<K::Value> {
        let (metadata, save_config) = set_completed(
            boxed_value
                .as_ref()
                .ok()
                .map(|x| {
                    let any = &**x as &dyn Any;

                    any.downcast_ref::<K::Value>()
                        .expect("Failed to downcast value")
                })
                .ok_or(CyclicError),
        );

        if let (Ok(value), true) = (&boxed_value, save_config.save_value) {
            let any = &**value as &dyn Any;

            self.save_value::<K>(
                metadata.version_info.fingerprint.unwrap(),
                any.downcast_ref::<K::Value>().unwrap().clone(),
            );
        }

        let final_metadata = ValueMetadata::Derived(metadata);

        if save_config.save_metadata {
            self.save_value_metadata::<K>(
                fingerprint::fingerprint(self.database.random_seed, key),
                final_metadata.clone(),
            );
        }

        let final_value =
            boxed_value.unwrap_or_else(|_| smallbox::smallbox!(K::scc_value()));

        let return_value = return_value.then(|| {
            (&*final_value as &dyn Any)
                .downcast_ref::<K::Value>()
                .expect("Failed to downcast value")
                .clone()
        });

        self.database.query_states_by_key.insert(
            DynamicKey(key.smallbox_clone()),
            State::Completion(Completion {
                metadata: final_metadata,
                store: Some(final_value),
            }),
        );

        return_value
    }

    async fn need_recompute<K: Key>(
        self: &Arc<Self>,
        re_verify: &ReVerify,
        key: &K,
        current_version: u64,
    ) -> bool {
        // Sadly, rayon thread pool is not applicable as it could
        // cause thread pool starvation deadlocks.
        for x in re_verify.derived_metadata.dependencies.as_ref() {
            tracing::debug!(
                "Start re-verifying dependency `{}` `{:?} for `{}` `{key:?}`",
                x.0.type_name(),
                x.0,
                key.type_name(),
            );

            let dep_ref = &*x.0;
            let type_id = dep_ref.any().type_id();

            // if the dependency is an input, skip
            // re-verification
            if !self.database.query_states_by_key.get(x).is_some_and(|x| {
                x.as_completion().is_some_and(|x| x.metadata.is_input())
            }) {
                let re_verify_query = self
                    .runtime
                    .executor
                    .get_entry_with_id(type_id)
                    .unwrap_or_else(|| {
                        panic!(
                            "No executor registered for key type `{}`",
                            dep_ref.type_name()
                        )
                    })
                    .get_re_verify_query();

                let _ = (re_verify_query)(
                    self,
                    dep_ref.any(),
                    current_version,
                    key as &dyn Dynamic,
                )
                .await;
            }

            tracing::debug!(
                "Re-verification completed dependency `{}` `{:?} for `{}` \
                 `{key:?}`",
                dep_ref.type_name(),
                dep_ref,
                key.type_name(),
            );

            if let Some(state) = self.database.query_states_by_key.get(x) {
                let updated_at = state
                    .as_completion()
                    .expect("should be completion since it was re-verified")
                    .metadata
                    .updated_at();
                let key_verified_at =
                    re_verify.derived_metadata.version_info.verified_at;

                if updated_at > key_verified_at {
                    tracing::info!(
                        "Re-verification of `{}` `{key:?}` is required since \
                         dependency `{}` `{:?}` has been updated; dependency \
                         was updated at `{updated_at}` but the key was \
                         verified at `{key_verified_at}`",
                        key.type_name(),
                        x.0.type_name(),
                        x.0,
                    );
                    return true;
                }
            }
        }

        false
    }

    #[allow(clippy::too_many_lines)]
    #[instrument(
        fields(
            key_name = std::any::type_name::<K>(),
            key = ?key,
        ),
        level = "info",
        skip_all
    )]
    async fn handle_re_verify<K: Key>(
        self: &Arc<Self>,
        key: &K,
        mut re_verify: ReVerify,
        current_version: u64,
        return_value: bool,
    ) -> Option<K::Value> {
        let recompute = {
            if K::ALWAYS_REVERIFY
                || re_verify.derived_metadata.version_info.fingerprint.is_none()
            {
                // if the query is a part of SCC, always recompute
                tracing::info!(
                    "Always recomputing `{}` `{key:?}`",
                    key.type_name()
                );
                true
            } else {
                self.need_recompute(&re_verify, key, current_version).await
            }
        };

        // update the version info to the current version
        re_verify.derived_metadata.version_info.verified_at = current_version;

        if recompute {
            self.compute(key, true, |value, tracked_dependencies| {
                let new_fingerprint = value.ok().map(|x| {
                    fingerprint::fingerprint(self.database.random_seed, x)
                });

                // if the new fingerprint is different from
                // the old one, update the value version
                if re_verify
                    .derived_metadata
                    .version_info
                    .fingerprint
                    .is_none_or(|x| Some(x) != new_fingerprint)
                {
                    tracing::debug!(
                        "Value fingerprint updated for `{}` `{key:?}` with a \
                         new fingerprint: {:?} -> {:?}",
                        key.type_name(),
                        re_verify.derived_metadata.version_info.fingerprint,
                        new_fingerprint
                    );

                    re_verify.derived_metadata.version_info.fingerprint =
                        new_fingerprint;
                    re_verify.derived_metadata.version_info.updated_at =
                        current_version;
                }

                re_verify.derived_metadata.version_info.verified_at =
                    current_version;

                // update the dependencies with the tracked one
                tracing::debug!(
                    "Dependencies updated for `{}` `{key:?}`: {:?}",
                    key.type_name(),
                    tracked_dependencies
                );

                re_verify.derived_metadata.dependencies =
                    tracked_dependencies.into();

                tracing::info!(
                    "Re-computed value for `{}` `{key:?}` with metadata: {:?}",
                    key.type_name(),
                    re_verify.derived_metadata
                );

                (re_verify.derived_metadata, true)
            })
            .await
        } else if return_value {
            let value = match re_verify.value_store {
                Some(value) => {
                    let return_value = (&*value as &dyn Any)
                        .downcast_ref::<K::Value>()
                        .expect("Failed to downcast value")
                        .clone();

                    let final_metadata =
                        ValueMetadata::Derived(re_verify.derived_metadata);

                    self.save_value_metadata::<K>(
                        fingerprint::fingerprint(
                            self.database.random_seed,
                            key,
                        ),
                        final_metadata.clone(),
                    );

                    self.database.query_states_by_key.insert(
                        DynamicKey(key.smallbox_clone()),
                        State::Completion(Completion {
                            metadata: final_metadata,
                            store: Some(value),
                        }),
                    );

                    return Some(return_value);
                }
                None => re_verify
                    .derived_metadata
                    .version_info
                    .fingerprint
                    .map_or_else(
                        || Some(Err(CyclicError)),
                        |fingerprint| {
                            self.try_load_value::<K>(fingerprint).map(Ok)
                        },
                    ),
            };

            match value {
                Some(value) => self.handle_computed_value(
                    key,
                    value.map(|x| {
                        let dynamic_value: DynamicValue =
                            smallbox::smallbox!(x);

                        dynamic_value
                    }),
                    return_value,
                    |result| {
                        let new_fingerprint = result.ok().map(|x| {
                            fingerprint::fingerprint(
                                self.database.random_seed,
                                x,
                            )
                        });

                        let save_value = if new_fingerprint
                            == re_verify
                                .derived_metadata
                                .version_info
                                .fingerprint
                        {
                            false
                        } else {
                            tracing::debug!(
                                "Value fingerprint updated for `{}` `{key:?}` \
                                 with a new fingerprint: {:?} -> {:?}",
                                key.type_name(),
                                re_verify
                                    .derived_metadata
                                    .version_info
                                    .fingerprint,
                                new_fingerprint
                            );

                            re_verify
                                .derived_metadata
                                .version_info
                                .fingerprint = new_fingerprint;
                            re_verify
                                .derived_metadata
                                .version_info
                                .updated_at = current_version;

                            true
                        };

                        (re_verify.derived_metadata, SaveConfig {
                            save_value,
                            save_metadata: true,
                        })
                    },
                ),
                None => {
                    self.compute(key, true, |value, dependencies| {
                        let new_fingerprint = value.ok().map(|x| {
                            fingerprint::fingerprint(
                                self.database.random_seed,
                                x,
                            )
                        });

                        if new_fingerprint
                            != re_verify
                                .derived_metadata
                                .version_info
                                .fingerprint
                        {
                            tracing::debug!(
                                "Value fingerprint updated for `{}` `{key:?}` \
                                 with a new fingerprint: {:?} -> {:?}",
                                key.type_name(),
                                re_verify
                                    .derived_metadata
                                    .version_info
                                    .fingerprint,
                                new_fingerprint
                            );

                            re_verify
                                .derived_metadata
                                .version_info
                                .fingerprint = new_fingerprint;
                            re_verify
                                .derived_metadata
                                .version_info
                                .updated_at = current_version;

                            re_verify.derived_metadata.dependencies =
                                dependencies.into();
                        }

                        (re_verify.derived_metadata, true)
                    })
                    .await
                }
            }
        } else {
            let final_metadata =
                ValueMetadata::Derived(re_verify.derived_metadata);

            self.save_value_metadata::<K>(
                fingerprint::fingerprint(self.database.random_seed, key),
                final_metadata.clone(),
            );

            *self
                .database
                .query_states_by_key
                .get_mut(key as &dyn Dynamic)
                .expect("should be present") = State::Completion(Completion {
                metadata: final_metadata,
                store: re_verify.value_store,
            });

            None
        }
    }

    #[allow(clippy::too_many_lines)]
    async fn continuation<K: Key>(
        self: &Arc<Self>,
        key: &K,
        continuation: Continuation,
        return_value: bool,
        current_version: u64,
    ) -> Option<K::Value> {
        match continuation {
            Continuation::Fresh => {
                // fresh query, compute the value from scratch
                self.compute(
                    key,
                    return_value,
                    |result, tracked_dependencies| {
                        (
                            DerivedMetadata {
                                version_info: DerivedVersionInfo {
                                    verified_at: current_version,
                                    updated_at: current_version,
                                    fingerprint: result.ok().map(|x| {
                                        fingerprint::fingerprint(
                                            self.database.random_seed,
                                            x,
                                        )
                                    }),
                                },
                                dependencies: tracked_dependencies.into(),
                            },
                            true,
                        )
                    },
                )
                .await
            }

            Continuation::ReVerify(re_verify) => {
                self.handle_re_verify(
                    key,
                    re_verify,
                    current_version,
                    return_value,
                )
                .await
            }

            Continuation::ReExecute(mut re_execute) => {
                tracing::debug!(
                    "Re-executing query for `{}` `{key:?}` with metadata: {:?}",
                    key.type_name(),
                    re_execute.derived_metadata
                );
                self.compute(key, return_value, |result, dependencies| {
                    let new_fingerprint = result.ok().map(|x| {
                        fingerprint::fingerprint(self.database.random_seed, x)
                    });

                    let update = if new_fingerprint
                        == re_execute.derived_metadata.version_info.fingerprint
                    {
                        false
                    } else {
                        tracing::debug!(
                            "Value fingerprint updated for `{}` `{key:?}` \
                             with a new fingerprint: {:?} -> {:?}",
                            key.type_name(),
                            re_execute
                                .derived_metadata
                                .version_info
                                .fingerprint,
                            new_fingerprint
                        );

                        re_execute.derived_metadata.version_info.fingerprint =
                            new_fingerprint;
                        re_execute.derived_metadata.version_info.updated_at =
                            current_version;
                        re_execute.derived_metadata.dependencies =
                            dependencies.into();

                        true
                    };

                    (re_execute.derived_metadata, update)
                })
                .await
            }
        }
    }

    pub(super) async fn query_internal<K: Key>(
        self: &Arc<Self>,
        key: &K,
        called_from: Option<&dyn Dynamic>,
        current_version: u64,
        return_value: bool,
    ) -> Result<Option<K::Value>, CyclicError> {
        // insert to the dependencies list if required
        if let Some(called_from) = called_from {
            let state = self
                .database
                .query_states_by_key
                .get(called_from as &dyn Dynamic)
                .expect("should be present");

            let running = state.as_running().expect(
                "should be running since it appeared in the call stack",
            );

            if !running.dependencies_set.contains(called_from) {
                tracing::debug!(
                    "Inserting `{}` `{key:?}` to the dependencies of `{}`",
                    key.type_name(),
                    called_from.type_name(),
                );
                running
                    .dependencies_order
                    .write()
                    .push(DynamicKey(key.smallbox_clone()));
                running
                    .dependencies_set
                    .insert(DynamicKey(key.smallbox_clone()));
            }
        }

        let value = loop {
            // Fast path: mostly used read lock, lower lock contention
            match self
                .fast_path(key, called_from, return_value, current_version)
                .await?
            {
                FastPathDecision::TryAgain => continue,
                FastPathDecision::ToSlowPath => {}
                FastPathDecision::Return(value) => {
                    tracing::debug!(
                        "`{}` `{key:?}` returned `FastPathDecision::Return`",
                        key.type_name()
                    );
                    break value;
                }
            }

            // Slow Path: use `entry` obtaining a write lock for state mutation
            let (continuation, notify) =
                match self.slow_path(key, current_version, return_value) {
                    SlowPathDecision::TryAgain => continue,
                    SlowPathDecision::Return(value) => break value,
                    SlowPathDecision::Continuation(continuation, notify) => {
                        (continuation, notify)
                    }
                };

            tracing::debug!(
                "Executing query for `{}` with continuation: {:?}",
                key.type_name(),
                continuation
            );

            let value = self
                .continuation(key, continuation, return_value, current_version)
                .await;

            tracing::debug!("`{}` `{key:?}` completed", key.type_name(),);

            debug_assert!(
                self.database
                    .query_states_by_key
                    .get(key as &dyn Dynamic)
                    .is_some_and(|x| x.as_completion().is_some()),
                "Query state for `{}` `{key:?}` should be in completion state",
                key.type_name()
            );

            // notify all the waiters that is waiting for the query to complete
            notify.notify_waiters();

            break value;
        };

        if let Some(called_from) = called_from {
            let is_in_scc = self
                .database
                .query_states_by_key
                .get(called_from)
                .expect("should be present with running state")
                .as_running()
                .expect("should be running since it appeared in the call stack")
                .is_in_scc
                .load(std::sync::atomic::Ordering::Relaxed);

            if is_in_scc {
                return Err(CyclicError);
            }
        }

        Ok(value)
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
type KeySmallBox<T> = smallbox::SmallBox<T, (u64, u64, u64)>;

/// A trait allowing store multiple types of as a key in a hashmap. This is
/// automatically implemented for all types that implement the [`Key`] trait.
#[doc(hidden)]
pub(super) trait Dynamic: 'static + Send + Sync + Debug {
    #[doc(hidden)]
    fn any(&self) -> &dyn std::any::Any;
    #[doc(hidden)]
    fn eq(&self, other: &dyn Dynamic) -> bool;
    #[doc(hidden)]
    fn hash(&self, state: &mut dyn std::hash::Hasher);
    #[doc(hidden)]
    fn fingerprint(&self, random_seed: u64) -> u128;
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
pub(super) struct DynamicKey(pub KeySmallBox<dyn Dynamic>);

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

    fn fingerprint(&self, random_seed: u64) -> u128 {
        fingerprint::fingerprint(random_seed, self)
    }
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

impl Drop for Database {
    fn drop(&mut self) {
        let map = std::mem::take(&mut self.query_states_by_key);
        map.into_par_iter().for_each(drop); // parallel drop the entries
    }
}

#[cfg(test)]
mod test;
