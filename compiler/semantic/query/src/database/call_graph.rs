#![allow(clippy::mutable_key_type)]

//! Implements the [`CallGraph`] struct used to track the dependencies between
//! queries.

use core::panic;
use std::{
    any::Any,
    collections::hash_map::Entry,
    fs::DirEntry,
    io::{BufReader, BufWriter},
    path::Path,
    sync::Arc,
    thread::ThreadId,
};

use enum_as_inner::EnumAsInner;
use getset::Getters;
use parking_lot::{Condvar, MutexGuard, RwLock};
use pernixc_hash::{HashMap, HashSet};
use pernixc_serialize::{
    binary::{de::BinaryDeserializer, ser::BinarySerializer},
    Deserialize, Serialize,
};
use pernixc_stable_hash::{StableHash, StableHasher};
use pernixc_stable_type_id::StableTypeID;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use super::Database;
use crate::{
    key::{Dynamic, DynamicBox, Key},
    runtime::{
        executor::Executor,
        persistence::{
            serde::{DynamicDeserialize, DynamicSerialize},
            ReadAny, WriteAny,
        },
    },
    Engine,
};

/// Tracks the dependencies between queries and their execution order.
#[derive(Default, Serialize, Deserialize, Getters)]
#[allow(clippy::mutable_key_type)]
#[serde(
    ser_extension(DynamicSerialize<__S>),
    de_extension(DynamicDeserialize<__D>)
)]
pub struct CallGraph {
    // emulating a call stack of particular thread
    #[serde(skip)]
    record_stacks_by_thread_id: HashMap<ThreadId, Vec<DynamicBox>>,

    // the condition variables used to notify the threads that are waiting for
    // the completion of a particular record
    #[serde(skip)]
    condvars_by_record: HashMap<DynamicBox, Arc<Condvar>>,

    #[serde(skip)]
    current_dependencies_by_dependant: HashMap<DynamicBox, DynamicBox>,

    dependency_graph: HashMap<DynamicBox, HashSet<DynamicBox>>,
    version_info_by_keys: HashMap<DynamicBox, VersionInfo>,
    cyclic_dependencies: Vec<CyclicDependency>,
}

const CALL_GRAPH_DIRECTORY: &str = "call_graph";
const DEPENDENCY_GRAPH_DIRECTORY: &str = "dependency_graph";
const VERSION_INFO_BY_KEYS: &str = "version_info_by_keys";

impl CallGraph {
    fn collect_files(path: &Path) -> Result<Vec<DirEntry>, std::io::Error> {
        path.read_dir()?.collect::<Result<Vec<_>, _>>()
    }

    fn collect_for_dependency_graph<
        E: DynamicDeserialize<BinaryDeserializer<Box<dyn ReadAny>>>
            + Send
            + Sync
            + 'static,
    >(
        files: Vec<DirEntry>,
        dependency_graph: &RwLock<HashMap<DynamicBox, HashSet<DynamicBox>>>,
        extension: &E,
    ) -> Result<(), std::io::Error> {
        files
            .into_par_iter()
            .map(|dir_entry| {
                // skip if directory
                if dir_entry.file_type()?.is_dir() {
                    return Ok(());
                }

                // dispatch the stable type ID based on the file name
                let file_name = dir_entry.file_name();
                let file_name_str = file_name.to_string_lossy();
                let stable_type_id_u128 =
                    u128::from_str_radix(&file_name_str[..32], 16).map_err(
                        |_| {
                            std::io::Error::new(
                                std::io::ErrorKind::InvalidData,
                                format!(
                                    "invalid stable type ID in file name: \
                                     {file_name_str}"
                                ),
                            )
                        },
                    )?;

                let hi = (stable_type_id_u128 >> 64) as u64;
                let lo = (stable_type_id_u128 & 0xFFFF_FFFF_FFFF_FFFF) as u64;

                let stable_type_id =
                    unsafe { StableTypeID::from_raw_parts(hi, lo) };

                let file = std::fs::File::open(dir_entry.path())?;
                let mut deserializer =
                    BinaryDeserializer::<Box<dyn ReadAny>>::new(Box::new(
                        BufReader::new(file),
                    ));

                let helper = extension
                    .deserialization_helper_by_type_id()
                    .get(&stable_type_id)
                    .ok_or_else(|| {
                        std::io::Error::new(
                            std::io::ErrorKind::InvalidData,
                            format!(
                                "no deserialization helper registered for \
                                 type ID {stable_type_id_u128:032x}",
                            ),
                        )
                    })?;

                helper.deserialize_dependency_graph(
                    &mut deserializer,
                    extension,
                    dependency_graph,
                )?;

                Ok(())
            })
            .collect::<Result<(), std::io::Error>>()
    }

    fn collect_for_version_infos<
        E: DynamicDeserialize<BinaryDeserializer<Box<dyn ReadAny>>>
            + Send
            + Sync
            + 'static,
    >(
        files: Vec<DirEntry>,
        version_infos: &RwLock<HashMap<DynamicBox, VersionInfo>>,
        extension: &E,
    ) -> Result<(), std::io::Error> {
        files
            .into_par_iter()
            .map(|dir_entry| {
                // skip if directory
                if dir_entry.file_type()?.is_dir() {
                    return Ok(());
                }

                // dispatch the stable type ID based on the file name
                let file_name = dir_entry.file_name();
                let file_name_str = file_name.to_string_lossy();
                let stable_type_id_u128 =
                    u128::from_str_radix(&file_name_str[..32], 16).map_err(
                        |_| {
                            std::io::Error::new(
                                std::io::ErrorKind::InvalidData,
                                format!(
                                    "invalid stable type ID in file name: \
                                     {file_name_str}"
                                ),
                            )
                        },
                    )?;

                let hi = (stable_type_id_u128 >> 64) as u64;
                let lo = (stable_type_id_u128 & 0xFFFF_FFFF_FFFF_FFFF) as u64;

                let stable_type_id =
                    unsafe { StableTypeID::from_raw_parts(hi, lo) };

                let file = std::fs::File::open(dir_entry.path())?;
                let mut deserializer =
                    BinaryDeserializer::<Box<dyn ReadAny>>::new(Box::new(
                        BufReader::new(file),
                    ));

                let helper = extension
                    .deserialization_helper_by_type_id()
                    .get(&stable_type_id)
                    .ok_or_else(|| {
                        std::io::Error::new(
                            std::io::ErrorKind::InvalidData,
                            format!(
                                "no deserialization helper registered for \
                                 type ID {stable_type_id_u128:032x}",
                            ),
                        )
                    })?;

                helper.deserialize_version_infos(
                    &mut deserializer,
                    extension,
                    version_infos,
                )?;

                Ok(())
            })
            .collect::<Result<(), std::io::Error>>()
    }

    #[allow(clippy::too_many_lines)]
    pub(crate) fn deserialize_call_graph<
        E: DynamicSerialize<BinarySerializer<Box<dyn WriteAny>>>
            + DynamicDeserialize<BinaryDeserializer<Box<dyn ReadAny>>>
            + Send
            + Sync
            + 'static,
    >(
        serde_extension: &dyn Any,
        base_path: &std::path::Path,
    ) -> Result<Self, std::io::Error> {
        let extension = serde_extension
            .downcast_ref::<E>()
            .expect("serde_extension must match the expected type");

        let dependency_graph = RwLock::new(HashMap::default());
        let version_info_by_keys = RwLock::new(HashMap::default());

        let mut dependency_graph_result = Ok(());
        let mut version_infos_result = Ok(());

        rayon::scope(|s| {
            // deserialize dependency graph
            s.spawn(|_| {
                let mut dependency_graph_path = base_path.to_path_buf();
                dependency_graph_path.push(CALL_GRAPH_DIRECTORY);
                dependency_graph_path.push(DEPENDENCY_GRAPH_DIRECTORY);

                if !dependency_graph_path.exists() {
                    if let Err(e) =
                        std::fs::create_dir_all(&dependency_graph_path)
                    {
                        dependency_graph_result = Err(e);
                        return;
                    }
                }

                let files = match Self::collect_files(&dependency_graph_path) {
                    Ok(files) => files,
                    Err(error) => {
                        dependency_graph_result = Err(error);
                        return;
                    }
                };

                dependency_graph_result = Self::collect_for_dependency_graph(
                    files,
                    &dependency_graph,
                    extension,
                );
            });

            // deserialize version info
            s.spawn(|_| {
                let mut version_info_path = base_path.to_path_buf();
                version_info_path.push(CALL_GRAPH_DIRECTORY);
                version_info_path.push(VERSION_INFO_BY_KEYS);

                if !version_info_path.exists() {
                    if let Err(e) = std::fs::create_dir_all(&version_info_path)
                    {
                        version_infos_result = Err(e);
                        return;
                    }
                }

                let files = match Self::collect_files(&version_info_path) {
                    Ok(files) => files,
                    Err(error) => {
                        version_infos_result = Err(error);
                        return;
                    }
                };

                version_infos_result = Self::collect_for_version_infos(
                    files,
                    &version_info_by_keys,
                    extension,
                );
            });
        });

        dependency_graph_result?;
        version_infos_result?;

        Ok(Self {
            record_stacks_by_thread_id: HashMap::default(),
            condvars_by_record: HashMap::default(),
            current_dependencies_by_dependant: HashMap::default(),
            dependency_graph: dependency_graph.into_inner(),
            version_info_by_keys: version_info_by_keys.into_inner(),
            cyclic_dependencies: Vec::new(),
        })
    }

    #[allow(clippy::too_many_lines)]
    pub(crate) fn serialize_call_graph<
        E: DynamicSerialize<BinarySerializer<Box<dyn WriteAny>>>
            + DynamicDeserialize<BinaryDeserializer<Box<dyn ReadAny>>>
            + Send
            + Sync
            + 'static,
    >(
        call_graph: &Self,
        serde_extension: &dyn Any,
        base_path: &std::path::Path,
    ) -> Result<(), std::io::Error> {
        let extension = serde_extension
            .downcast_ref::<E>()
            .expect("serde_extension must match the expected type");

        let mut dependency_graph = Ok(());
        let mut version_info = Ok(());

        rayon::scope(|s| {
            s.spawn(|_| {
                let mut dependency_graph_path = base_path.to_path_buf();
                dependency_graph_path.push(CALL_GRAPH_DIRECTORY);
                dependency_graph_path.push(DEPENDENCY_GRAPH_DIRECTORY);

                if !dependency_graph_path.exists() {
                    dependency_graph =
                        std::fs::create_dir_all(&dependency_graph_path);

                    if dependency_graph.is_err() {
                        return;
                    }
                }

                let mut entries_by_stable_type_id = HashMap::<
                    StableTypeID,
                    Vec<(&DynamicBox, &HashSet<DynamicBox>)>,
                >::default(
                );

                for (entry, dependencies) in &call_graph.dependency_graph {
                    let stable_type_id = entry.stable_type_id();
                    entries_by_stable_type_id
                        .entry(stable_type_id)
                        .or_default()
                        .push((entry, dependencies));
                }

                dependency_graph = entries_by_stable_type_id
                    .into_par_iter()
                    .map(|(stable_type_id, entries)| {
                        let stable_type_id_u128 = stable_type_id.as_u128();
                        let file =
                            std::fs::File::create(dependency_graph_path.join(
                                format!("{stable_type_id_u128:032x}.dat",),
                            ))?;

                        let mut serializer =
                            BinarySerializer::<Box<dyn WriteAny>>::new(
                                Box::new(BufWriter::new(file)),
                            );

                        extension
                            .serialization_helper_by_type_id()
                            .get(&stable_type_id)
                            .unwrap_or_else(|| {
                                panic!(
                                    "no serialization helper registered for \
                                     type ID {stable_type_id_u128:032x}",
                                )
                            })
                            .serialize_dependency_graph(
                                &entries,
                                &mut serializer,
                                extension,
                            )?;

                        Ok(())
                    })
                    .collect::<Result<(), std::io::Error>>();
            });

            s.spawn(|_| {
                let mut version_info_path = base_path.to_path_buf();
                version_info_path.push(CALL_GRAPH_DIRECTORY);
                version_info_path.push(VERSION_INFO_BY_KEYS);

                if !version_info_path.exists() {
                    version_info = std::fs::create_dir_all(&version_info_path);

                    if version_info.is_err() {
                        return;
                    }
                }

                let mut entries_by_stable_type_id = HashMap::<
                    StableTypeID,
                    Vec<(&DynamicBox, &VersionInfo)>,
                >::default(
                );

                for (entry, version_info) in &call_graph.version_info_by_keys {
                    let stable_type_id = entry.stable_type_id();
                    entries_by_stable_type_id
                        .entry(stable_type_id)
                        .or_default()
                        .push((entry, version_info));
                }

                version_info = entries_by_stable_type_id
                    .into_par_iter()
                    .map(|(stable_type_id, entries)| {
                        let stable_type_id_u128 = stable_type_id.as_u128();
                        let file =
                            std::fs::File::create(version_info_path.join(
                                format!("{stable_type_id_u128:032x}.dat",),
                            ))?;

                        let mut serializer =
                            BinarySerializer::<Box<dyn WriteAny>>::new(
                                Box::new(BufWriter::new(file)),
                            );

                        extension
                            .serialization_helper_by_type_id()
                            .get(&stable_type_id)
                            .unwrap_or_else(|| {
                                panic!(
                                    "no serialization helper registered for \
                                     type ID {stable_type_id_u128:032x}",
                                )
                            })
                            .serialize_version_info(
                                &entries,
                                &mut serializer,
                                extension,
                            )?;

                        Ok(())
                    })
                    .collect::<Result<(), std::io::Error>>();
            });
        });

        dependency_graph?;
        version_info?;

        Ok(())
    }
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
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(
    ser_extension(DynamicSerialize<__S>),
    de_extension(DynamicDeserialize<__D>)
)]
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

impl Database {
    /// Sets the input value for the given key.
    ///
    /// If this call happens after one of the derived values has been computed,
    /// the version of the database will be bumped up by one. This is to
    /// indicate that the input value has changed and all the derived values
    /// need to reflect this change.
    pub fn set_input<K: Key + Dynamic>(&mut self, key: &K, value: K::Value) {
        // set the input value
        let value_fingerprint = calculate_fingerprint(&value);
        self.map.insert(key.clone(), value);

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
                if Some(value_fingerprint) != value.fingerprint {
                    // bump the version for the new input setting
                    if *self.last_was_query.get_mut() {
                        self.version += 1;
                        *self.last_was_query.get_mut() = false;
                    }

                    value.updated_at_version = self.version;
                    value.fingerprint = Some(value_fingerprint);
                }
            }
            Entry::Vacant(entry) => {
                entry.insert(VersionInfo {
                    updated_at_version: self.version,
                    verfied_at_version: self.version,
                    fingerprint: Some(value_fingerprint),
                    kind: Kind::Input,
                });
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
        let (result, call_graph) =
            self.query_internal(key, self.database.call_graph.lock());

        result?;

        self.database.map.get(key).map_or_else(
            || {
                let key_smallbox = DynamicBox(key.smallbox_clone());
                let version_info = call_graph
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
                        x.try_load::<T>(version_info.fingerprint.unwrap()).ok()
                    })
                    .flatten();

                if let Some(value) = value {
                    // save for the future use
                    self.database.map.insert(key.clone(), value.clone());

                    return Ok(value);
                }

                let called_from = call_graph.called_from();

                // compute it again
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
        }) && version_info.verfied_at_version == self.database.version
        {
            return Err(crate::runtime::executor::CyclicError);
        }

        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    pub(crate) fn query_internal<'a, T: Dynamic + Key>(
        &'a self,
        key: &T,
        mut call_graph: MutexGuard<'a, CallGraph>,
    ) -> (
        Result<(), crate::runtime::executor::CyclicError>,
        MutexGuard<'a, CallGraph>,
    ) {
        let key_smallbox = DynamicBox(key.smallbox_clone());

        let called_from = call_graph.called_from();

        // get the version info for the key.
        let Some(version_info) =
            call_graph.version_info_by_keys.get(&key_smallbox).copied()
        else {
            self.database
                .last_was_query
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

            return (Ok(()), call_graph);
        };

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
            return (Ok(()), call_graph);
        }

        self.database
            .last_was_query
            .store(true, std::sync::atomic::Ordering::SeqCst);

        if version_info.verfied_at_version == self.database.version {
            // add the dependency of the input
            if let Some(called_from) = &called_from {
                call_graph
                    .dependency_graph
                    .get_mut(called_from)
                    .unwrap()
                    .insert(key_smallbox);
            }

            return (Ok(()), call_graph);
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
        } else {
            call_graph
                .version_info_by_keys
                .get_mut(&key_smallbox)
                .unwrap()
                .verfied_at_version = self.database.version;
        }

        (Ok(()), call_graph)
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
                                version_info.verfied_at_version =
                                    self.database.version;
                                version_info.kind = Kind::Derived {
                                    defaulted_by_cyclic_dependency: true,
                                };
                            }
                            Entry::Vacant(vacant_entry) => {
                                vacant_entry.insert(VersionInfo {
                                    updated_at_version: self.database.version,
                                    verfied_at_version: self.database.version,
                                    kind: Kind::Derived {
                                        defaulted_by_cyclic_dependency: true,
                                    },
                                    fingerprint: None,
                                });
                            }
                        }
                    }

                    call_graph
                        .cyclic_dependencies
                        .push(CyclicDependency { records_stack: stack });

                    // insert a default value for the cyclic dependency
                    self.database.map.insert(key.clone(), T::scc_value());

                    call_graph.version_info_by_keys.insert(
                        DynamicBox(key.smallbox_clone()),
                        VersionInfo {
                            updated_at_version: self.database.version,
                            verfied_at_version: self.database.version,
                            kind: Kind::Derived {
                                defaulted_by_cyclic_dependency: true,
                            },
                            fingerprint: None,
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
        call_graph = self.database.call_graph.lock();

        // Handle the executor result
        match executor_result {
            Ok(value) => {
                let value_fingerprint = calculate_fingerprint(&value);
                self.database.map.insert(key.clone(), value);

                match call_graph
                    .version_info_by_keys
                    .entry(DynamicBox(key.smallbox_clone()))
                {
                    Entry::Occupied(mut occupied_entry) => {
                        let version_info = occupied_entry.get_mut();
                        version_info.verfied_at_version = self.database.version;
                        version_info.kind = Kind::Derived {
                            defaulted_by_cyclic_dependency: false,
                        };

                        if Some(value_fingerprint) != version_info.fingerprint {
                            version_info.updated_at_version =
                                self.database.version;
                            version_info.fingerprint = Some(value_fingerprint);
                        }
                    }
                    Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(VersionInfo {
                            updated_at_version: self.database.version,
                            verfied_at_version: self.database.version,
                            kind: Kind::Derived {
                                defaulted_by_cyclic_dependency: false,
                            },
                            fingerprint: Some(value_fingerprint),
                        });
                    }
                }
            }
            Err(_cyclic_error) => {
                call_graph.version_info_by_keys.insert(
                    DynamicBox(key.smallbox_clone()),
                    VersionInfo {
                        updated_at_version: self.database.version,
                        verfied_at_version: self.database.version,
                        kind: Kind::Derived {
                            defaulted_by_cyclic_dependency: true,
                        },
                        fingerprint: None,
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
                            version_info.verfied_at_version
                                == self.database.version
                                && matches!(version_info.kind, Kind::Derived {
                                    defaulted_by_cyclic_dependency: true
                                })
                        );

                        version_info.verfied_at_version = self.database.version;
                        version_info.updated_at_version = self.database.version;
                        version_info.kind = Kind::Derived {
                            defaulted_by_cyclic_dependency: true,
                        };
                    }
                    Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(VersionInfo {
                            updated_at_version: self.database.version,
                            verfied_at_version: self.database.version,
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

        assert!(call_graph.condvars_by_record.remove(key_smallbox).is_some());

        // notify the other threads that the component is computed
        sync.notify_all();

        (call_graph, ok)
    }
}

#[cfg(test)]
mod test;
