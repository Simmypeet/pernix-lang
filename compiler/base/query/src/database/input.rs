use std::{any::Any, sync::atomic::AtomicBool};

use crate::{
    Engine, Key,
    database::{Completion, Dynamic, DynamicKey, InputMetadata, ValueMetadata},
    fingerprint,
};

/// A lock that allows setting input values in the query database.
#[derive(Debug)]
pub struct SetInputLock<'eng> {
    engine: &'eng Engine,
    current_version: u64,
    update_version: AtomicBool,
}

impl Engine {
    /// Creates a lock that allows setting input values in the query database.
    ///
    /// This lock is used to ensure that queries can't be run while the inputs
    /// are being set.
    pub fn input_lock(&mut self) -> SetInputLock<'_> {
        SetInputLock {
            engine: self,
            current_version: self
                .database
                .version
                .load(std::sync::atomic::Ordering::Relaxed),
            update_version: AtomicBool::new(false),
        }
    }

    /// A helper method allowing setting the input queries via [`SetInputLock`]
    pub async fn input_session<'x, R>(
        &'x mut self,
        f: impl AsyncFnOnce(&SetInputLock) -> R,
    ) -> R {
        let input_lock = self.input_lock();
        let result = f(&input_lock).await;
        drop(input_lock);

        result
    }

    /// Forcefully increments the version of the query database, making all the
    /// queries re-verify their values. This works together with the
    /// [`Executor::ALWAYS_RECOMPUTE`] constant, which allows the queries to
    /// work with impure inputs.
    pub fn increment_version(&mut self) {
        *self.database.version.get_mut() += 1;
    }
}

impl Drop for SetInputLock<'_> {
    fn drop(&mut self) {
        if self.update_version.load(std::sync::atomic::Ordering::Relaxed) {
            self.engine
                .database
                .version
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        }
    }
}

struct UpdateSave {
    update_value: bool,
    update_metadata: bool,
}

/// Specifying whether there's a change in the input value or not
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SetInputResult {
    /// A new input value has been set
    Fresh,

    /// The input value has been updated; boolean indicates whether the
    /// value has been updated since the last time it was set or not.
    Updated(bool),
}

impl SetInputLock<'_> {
    /// Mutably accesses the value of the given key in the query database
    /// memory. After the mutation, the value is verified to see if it has
    /// changed. This is similar to calling `set_input` for overriding the
    /// value but more efficient as it avoids cloning the value.
    ///
    /// # Returns
    ///
    /// `true` if the value of the key exists in the **memory** (will not try to
    /// retrieve from the persistent storage), `false` otherwise.
    pub fn inplace_mutate<K: Key>(
        &self,
        key: &K,
        f: impl FnOnce(&mut K::Value),
    ) -> bool {
        let Some(mut value) = self
            .engine
            .database
            .query_states_by_key
            .get_mut(key as &dyn Dynamic)
        else {
            return false;
        };

        let super::State::Completion(completion) = value.value_mut() else {
            unreachable!(
                "should not be able to access SetInputLock while running a \
                 query"
            )
        };

        // try to get the value in the memory
        let Some(store) = completion.store.as_mut() else {
            return false;
        };

        let value = &mut **store as &mut dyn Any;
        let value = value.downcast_mut::<K::Value>().unwrap();

        f(value);

        // re-calculate the fingerprint of the value
        let value_fingerprint =
            fingerprint::fingerprint(self.engine.database.random_seed, value);

        let update =
            self.update_metadata(&mut completion.metadata, value_fingerprint);

        let key_fingerprint =
            fingerprint::fingerprint(self.engine.database.random_seed, key);

        if update.update_metadata {
            self.engine.save_value_metadata::<K>(
                key_fingerprint,
                completion.metadata.clone(),
            );
        }

        if update.update_value {
            self.engine.save_value::<K>(value_fingerprint, value.clone());
        }

        true
    }

    /// Sets the predefined input value for the given key in the query
    /// database.
    ///
    /// Is it also possible to override the value of an existing key using this
    /// method.
    ///
    /// When setting the input, the dependencies of the key are cleared.
    pub async fn set_input<K: Key>(
        &self,
        key: K,
        value: K::Value,
    ) -> SetInputResult {
        let key_fingerprint =
            fingerprint::fingerprint(self.engine.database.random_seed, &key);
        let value_fingerprint =
            fingerprint::fingerprint(self.engine.database.random_seed, &value);

        match self
            .engine
            .database
            .query_states_by_key
            .entry(DynamicKey(smallbox::smallbox!(key)))
        {
            dashmap::Entry::Occupied(mut occupied_entry) => {
                match occupied_entry.get_mut() {
                    super::State::Running(_) => {
                        unreachable!(
                            "should not be able to access SetInputLock while \
                             running a query"
                        )
                    }
                    super::State::Completion(completion) => {
                        let update = self.update_metadata(
                            &mut completion.metadata,
                            value_fingerprint,
                        );

                        if update.update_metadata {
                            self.engine.save_value_metadata::<K>(
                                key_fingerprint,
                                completion.metadata.clone(),
                            );
                        }

                        if update.update_value {
                            self.engine.save_value::<K>(
                                value_fingerprint,
                                value.clone(),
                            );
                        }

                        completion.store = Some(smallbox::smallbox!(value));

                        SetInputResult::Updated(update.update_value)
                    }
                }
            }

            dashmap::Entry::Vacant(vacant_entry) => {
                let version_info = self
                    .engine
                    .try_load_value_metadata::<K>(key_fingerprint)
                    .await;

                if let Some(mut version_info) = version_info {
                    let update = self
                        .update_metadata(&mut version_info, value_fingerprint);

                    if update.update_metadata {
                        self.engine.save_value_metadata::<K>(
                            key_fingerprint,
                            version_info.clone(),
                        );
                    }
                    if update.update_value {
                        self.engine
                            .save_value::<K>(value_fingerprint, value.clone());
                    }

                    vacant_entry.insert(super::State::Completion(Completion {
                        metadata: version_info,
                        store: Some(smallbox::smallbox!(value)),
                    }));

                    SetInputResult::Updated(update.update_value)
                } else {
                    self.update_version
                        .store(true, std::sync::atomic::Ordering::Relaxed);

                    let version_no = self.get_version();
                    let metadata = ValueMetadata::Input(InputMetadata {
                        fingerprint: value_fingerprint,
                        updated_at: version_no,
                    });

                    self.engine.save_value_metadata::<K>(
                        key_fingerprint,
                        metadata.clone(),
                    );

                    self.engine
                        .save_value::<K>(value_fingerprint, value.clone());

                    vacant_entry.insert(super::State::Completion(Completion {
                        metadata,
                        store: Some(smallbox::smallbox!(value)),
                    }));

                    SetInputResult::Fresh
                }
            }
        }
    }

    /// After the input session is done, always increment the database version
    /// regardless whether or not there're any changes in the input
    pub fn always_reverify(&self) {
        self.update_version.store(true, std::sync::atomic::Ordering::Relaxed);
    }

    fn update_metadata(
        &self,
        metadata: &mut ValueMetadata,
        value_fingerprint: u128,
    ) -> UpdateSave {
        match metadata {
            ValueMetadata::Derived(derived_metadata) => {
                let fingerprint_mismathced = Some(value_fingerprint)
                    != derived_metadata.version_info.fingerprint;

                let updated_at = if fingerprint_mismathced {
                    self.update_version
                        .store(true, std::sync::atomic::Ordering::Relaxed);
                    self.get_version()
                } else {
                    derived_metadata.version_info.updated_at
                };

                *metadata = ValueMetadata::Input(InputMetadata {
                    fingerprint: value_fingerprint,
                    updated_at,
                });

                UpdateSave {
                    update_value: fingerprint_mismathced,
                    update_metadata: true,
                }
            }

            ValueMetadata::Input(input_metadata) => {
                if input_metadata.fingerprint == value_fingerprint {
                    UpdateSave { update_value: false, update_metadata: false }
                } else {
                    self.update_version
                        .store(true, std::sync::atomic::Ordering::Relaxed);

                    input_metadata.fingerprint = value_fingerprint;
                    input_metadata.updated_at = self.get_version();

                    UpdateSave { update_value: true, update_metadata: true }
                }
            }
        }
    }

    fn get_version(&self) -> u64 {
        let updated =
            self.update_version.load(std::sync::atomic::Ordering::Relaxed);

        if updated { self.current_version + 1 } else { self.current_version }
    }
}
