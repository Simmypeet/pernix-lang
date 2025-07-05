use std::sync::{atomic::AtomicBool, Arc};

use crate::{
    database::{Completion, DynamicKey, InputMetadata, ValueMetadata},
    fingerprint, Engine, Key,
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

impl SetInputLock<'_> {
    /// Sets the predefined input value for the given key in the query
    /// database.
    ///
    /// Is it also possible to override the value of an existing key using this
    /// method.
    ///
    /// When setting the input, the dependencies of the key are cleared.
    pub fn set_input<K: Key>(&self, key: K, value: Arc<K::Value>) {
        let key_fingerprint = fingerprint::fingerprint(&key);
        let value_fingerprint = fingerprint::fingerprint(&value);

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
                            let _ = self.engine.save_value_metadata::<K>(
                                key_fingerprint,
                                &completion.metadata,
                            );
                        }

                        if update.update_value {
                            let _ = self
                                .engine
                                .save_value::<K>(key_fingerprint, &*value);
                        }

                        completion.store = Some(value);
                    }
                }
            }

            dashmap::Entry::Vacant(vacant_entry) => {
                let version_info =
                    self.engine.try_load_value_metadata::<K>(key_fingerprint);

                if let Some(mut version_info) = version_info {
                    let update = self
                        .update_metadata(&mut version_info, value_fingerprint);

                    if update.update_metadata {
                        let _ = self.engine.save_value_metadata::<K>(
                            key_fingerprint,
                            &version_info,
                        );
                    }
                    if update.update_value {
                        let _ = self
                            .engine
                            .save_value::<K>(key_fingerprint, &*value);
                    }

                    vacant_entry.insert(super::State::Completion(Completion {
                        metadata: version_info,
                        store: Some(value),
                    }));
                } else {
                    self.update_version
                        .store(true, std::sync::atomic::Ordering::Relaxed);

                    let version_no = self.get_version();
                    let metadata = ValueMetadata::Input(InputMetadata {
                        fingerprint: value_fingerprint,
                        updated_at: version_no,
                    });

                    let _ = self
                        .engine
                        .save_value_metadata::<K>(key_fingerprint, &metadata);
                    let _ =
                        self.engine.save_value::<K>(key_fingerprint, &*value);

                    vacant_entry.insert(super::State::Completion(Completion {
                        metadata,
                        store: Some(value),
                    }));
                }
            }
        }
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

        if updated {
            self.current_version + 1
        } else {
            self.current_version
        }
    }
}
