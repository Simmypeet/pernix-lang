use std::sync::{atomic::AtomicBool, Arc};

use pernixc_hash::HashSet;

use crate::{
    database::{Completion, DynamicKey, ValueMetadata, ValueVersion},
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
            self.engine.database.version.store(
                self.current_version,
                std::sync::atomic::Ordering::Relaxed,
            );
        }
    }
}

impl SetInputLock<'_> {
    /// Sets the predefined input value for the given key in the query
    /// database.
    pub fn set_input<K: Key>(&self, key: K, value: Arc<K::Value>) {
        let key_fingerprint = fingerprint::fingerprint(&key);
        let value_fingerprint = fingerprint::fingerprint(&value);

        let update_version = |version: &mut ValueVersion| {
            let old_version = *version;

            let mut version_no = self.get_version();

            // update the version info if invalidated
            if Some(value_fingerprint) != version.fingerprint {
                self.update_version
                    .store(true, std::sync::atomic::Ordering::Relaxed);

                version_no = self.get_version();

                version.updated_at = version_no;
                version.fingerprint = Some(value_fingerprint);
            }

            version.verified_at = version_no;

            if version != &old_version {
                let _ = self
                    .engine
                    .save_value_version::<K>(key_fingerprint, version);
            }
        };

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
                        update_version(&mut completion.metadata.version);
                        let _ = self.engine.save_value_dependencies::<K>(
                            key_fingerprint,
                            &HashSet::default(),
                        );

                        completion.store = Some(value);
                        completion.metadata.dependencies =
                            Some(HashSet::default());
                    }
                }
            }

            dashmap::Entry::Vacant(vacant_entry) => {
                let version_info =
                    self.engine.try_load_value_version::<K>(key_fingerprint);

                if let Some(mut version_info) = version_info {
                    update_version(&mut version_info);
                    let _ = self.engine.save_value_dependencies::<K>(
                        key_fingerprint,
                        &HashSet::default(),
                    );

                    vacant_entry.insert(super::State::Completion(Completion {
                        metadata: ValueMetadata {
                            version: version_info,
                            dependencies: Some(HashSet::default()),
                        },
                        store: Some(value),
                    }));
                } else {
                    self.update_version
                        .store(true, std::sync::atomic::Ordering::Relaxed);

                    let version_no = self.get_version();
                    let version_info = ValueVersion {
                        updated_at: version_no,
                        verified_at: version_no,
                        fingerprint: Some(value_fingerprint),
                    };

                    let _ = self.engine.save_value_version::<K>(
                        key_fingerprint,
                        &version_info,
                    );
                    let _ = self.engine.save_value_dependencies::<K>(
                        key_fingerprint,
                        &HashSet::default(),
                    );

                    vacant_entry.insert(super::State::Completion(Completion {
                        metadata: ValueMetadata {
                            version: version_info,
                            dependencies: Some(HashSet::default()),
                        },
                        store: Some(value),
                    }));
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
