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
use serde::{
    de::DeserializeSeed,
    ser::{SerializeMap, SerializeSeq, SerializeStruct},
    Deserialize, Serialize,
};

use super::Database;
use crate::{
    key::{Dynamic, DynamicBox, Key},
    runtime::executor::Executor,
    Engine,
};

/// Tracks the dependencies between queries and their execution order.
#[derive(Default)]
#[allow(clippy::mutable_key_type)]
pub struct CallGraph {
    // emulating a call stack of particular thread
    record_stacks_by_thread_id: HashMap<ThreadId, Vec<DynamicBox>>,

    // the condition variables used to notify the threads that are waiting for
    // the completion of a particular record
    condvars_by_record: HashMap<DynamicBox, Arc<Condvar>>,

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

#[derive(Debug, Clone, Copy)]
pub(super) struct SerializableCallGraph<'a> {
    pub(super) call_graph: &'a CallGraph,
    pub(super) serde: &'a crate::serde::Serde,
}

impl Serialize for SerializableCallGraph<'_> {
    #[allow(clippy::too_many_lines)]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        #[derive(Debug, Clone, Copy)]
        struct SerializableDependencyGraph<'a> {
            dependency_graph: &'a HashMap<DynamicBox, HashSet<DynamicBox>>,
            serde: &'a crate::serde::Serde,
        }

        impl Serialize for SerializableDependencyGraph<'_> {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                let mut map = serializer
                    .serialize_map(Some(self.dependency_graph.len()))?;
                for (key, dependencies) in self.dependency_graph {
                    #[derive(Debug)]
                    struct SerializableDependencies<'a> {
                        dependencies: &'a HashSet<DynamicBox>,
                        serde: &'a crate::serde::Serde,
                    }

                    impl Serialize for SerializableDependencies<'_> {
                        fn serialize<S>(
                            &self,
                            serializer: S,
                        ) -> Result<S::Ok, S::Error>
                        where
                            S: serde::Serializer,
                        {
                            let mut seq = serializer
                                .serialize_seq(Some(self.dependencies.len()))?;
                            for dep in self.dependencies {
                                seq.serialize_element(
                                    &dep.serializable(self.serde),
                                )?;
                            }
                            seq.end()
                        }
                    }

                    let serializable_key = key.serializable(self.serde);

                    map.serialize_entry(
                        &serializable_key,
                        &SerializableDependencies {
                            dependencies,
                            serde: self.serde,
                        },
                    )?;
                }
                map.end()
            }
        }

        #[derive(Debug, Clone, Copy)]
        struct SerializableVersionInfoMap<'a> {
            version_info_by_keys: &'a HashMap<DynamicBox, VersionInfo>,
            serde: &'a crate::serde::Serde,
        }

        impl Serialize for SerializableVersionInfoMap<'_> {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                let mut map = serializer
                    .serialize_map(Some(self.version_info_by_keys.len()))?;
                for (key, version_info) in self.version_info_by_keys {
                    let serializable_key = key.serializable(self.serde);
                    map.serialize_entry(&serializable_key, version_info)?;
                }
                map.end()
            }
        }

        #[derive(Debug, Clone, Copy)]
        struct SerializableCyclicDependencies<'a> {
            cyclic_dependencies: &'a [CyclicDependency],
            serde: &'a crate::serde::Serde,
        }

        impl Serialize for SerializableCyclicDependencies<'_> {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                let mut seq = serializer
                    .serialize_seq(Some(self.cyclic_dependencies.len()))?;
                for cyclic_dependency in self.cyclic_dependencies {
                    seq.serialize_element(&SerializableCyclicDependency {
                        cyclic_dependency,
                        serde: self.serde,
                    })?;
                }
                seq.end()
            }
        }

        let mut state = serializer.serialize_struct("CallGraph", 3)?;

        state.serialize_field(
            "dependency_graph",
            &SerializableDependencyGraph {
                dependency_graph: &self.call_graph.dependency_graph,
                serde: self.serde,
            },
        )?;

        state.serialize_field(
            "version_info_by_keys",
            &SerializableVersionInfoMap {
                version_info_by_keys: &self.call_graph.version_info_by_keys,
                serde: self.serde,
            },
        )?;

        state.serialize_field(
            "cyclic_dependencies",
            &SerializableCyclicDependencies {
                cyclic_dependencies: &self.call_graph.cyclic_dependencies,
                serde: self.serde,
            },
        )?;

        state.end()
    }
}

#[derive(Debug, Clone, Copy)]
pub(super) struct DeserializableCallGraph<'a>(pub &'a crate::serde::Serde);

impl<'de> DeserializeSeed<'de> for DeserializableCallGraph<'_> {
    type Value = CallGraph;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_struct(
            "CallGraph",
            &[
                "dependency_graph",
                "version_info_by_keys",
                "cyclic_dependencies",
            ],
            CallGraphVisitor { serde: self.0 },
        )
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Deserialize,
)]
#[serde(field_identifier)]
enum CallGraphField {
    #[serde(rename = "dependency_graph")]
    DependencyGraph,
    #[serde(rename = "version_info_by_keys")]
    VersionInfoByKeys,
    #[serde(rename = "cyclic_dependencies")]
    CyclicDependencies,
}

struct CallGraphVisitor<'a> {
    serde: &'a crate::serde::Serde,
}

impl<'de> serde::de::Visitor<'de> for CallGraphVisitor<'_> {
    type Value = CallGraph;

    fn expecting(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        write!(formatter, "struct CallGraph")
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::MapAccess<'de>,
    {
        let mut dependency_graph = None;
        let mut version_info_by_keys = None;
        let mut cyclic_dependencies = None;

        while let Some(key) = map.next_key()? {
            match key {
                CallGraphField::DependencyGraph => {
                    if dependency_graph.is_some() {
                        return Err(serde::de::Error::duplicate_field(
                            "dependency_graph",
                        ));
                    }
                    dependency_graph = Some(map.next_value_seed(
                        DeserializableDependencyGraph { serde: self.serde },
                    )?);
                }
                CallGraphField::VersionInfoByKeys => {
                    if version_info_by_keys.is_some() {
                        return Err(serde::de::Error::duplicate_field(
                            "version_info_by_keys",
                        ));
                    }
                    version_info_by_keys = Some(map.next_value_seed(
                        DeserializableVersionInfoMap { serde: self.serde },
                    )?);
                }
                CallGraphField::CyclicDependencies => {
                    if cyclic_dependencies.is_some() {
                        return Err(serde::de::Error::duplicate_field(
                            "cyclic_dependencies",
                        ));
                    }
                    cyclic_dependencies = Some(map.next_value_seed(
                        DeserializableCyclicDependencies { serde: self.serde },
                    )?);
                }
            }
        }

        let dependency_graph = dependency_graph.ok_or_else(|| {
            serde::de::Error::missing_field("dependency_graph")
        })?;
        let version_info_by_keys = version_info_by_keys.ok_or_else(|| {
            serde::de::Error::missing_field("version_info_by_keys")
        })?;
        let cyclic_dependencies = cyclic_dependencies.ok_or_else(|| {
            serde::de::Error::missing_field("cyclic_dependencies")
        })?;

        Ok(CallGraph {
            record_stacks_by_thread_id: HashMap::new(),
            condvars_by_record: HashMap::new(),
            current_dependencies_by_dependant: HashMap::new(),
            dependency_graph,
            version_info_by_keys,
            cyclic_dependencies,
        })
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        let dependency_graph = seq
            .next_element_seed(DeserializableDependencyGraph {
                serde: self.serde,
            })?
            .ok_or_else(|| serde::de::Error::invalid_length(0, &self))?;

        let version_info_by_keys = seq
            .next_element_seed(DeserializableVersionInfoMap {
                serde: self.serde,
            })?
            .ok_or_else(|| serde::de::Error::invalid_length(1, &self))?;

        let cyclic_dependencies = seq
            .next_element_seed(DeserializableCyclicDependencies {
                serde: self.serde,
            })?
            .ok_or_else(|| serde::de::Error::invalid_length(2, &self))?;

        Ok(CallGraph {
            record_stacks_by_thread_id: HashMap::new(),
            condvars_by_record: HashMap::new(),
            current_dependencies_by_dependant: HashMap::new(),
            dependency_graph,
            version_info_by_keys,
            cyclic_dependencies,
        })
    }
}

// Helper struct for deserializing dependency graph
struct DeserializableDependencyGraph<'a> {
    serde: &'a crate::serde::Serde,
}

impl<'de> DeserializeSeed<'de> for DeserializableDependencyGraph<'_> {
    type Value = HashMap<DynamicBox, HashSet<DynamicBox>>;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer
            .deserialize_map(DependencyGraphVisitor { serde: self.serde })
    }
}

struct DependencyGraphVisitor<'a> {
    serde: &'a crate::serde::Serde,
}

impl<'de> serde::de::Visitor<'de> for DependencyGraphVisitor<'_> {
    type Value = HashMap<DynamicBox, HashSet<DynamicBox>>;

    fn expecting(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        write!(formatter, "a map representing dependency graph")
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::MapAccess<'de>,
    {
        let mut dependency_graph = HashMap::new();

        while let Some(key) =
            map.next_key_seed(&self.serde.dynamic_box_deserializer())?
        {
            let dependencies =
                map.next_value_seed(DeserializableDependencies {
                    serde: self.serde,
                })?;
            dependency_graph.insert(key, dependencies);
        }

        Ok(dependency_graph)
    }
}

// Helper struct for deserializing dependency sets
struct DeserializableDependencies<'a> {
    serde: &'a crate::serde::Serde,
}

impl<'de> DeserializeSeed<'de> for DeserializableDependencies<'_> {
    type Value = HashSet<DynamicBox>;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_seq(DependenciesVisitor { serde: self.serde })
    }
}

struct DependenciesVisitor<'a> {
    serde: &'a crate::serde::Serde,
}

impl<'de> serde::de::Visitor<'de> for DependenciesVisitor<'_> {
    type Value = HashSet<DynamicBox>;

    fn expecting(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        write!(formatter, "a sequence of dependencies")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        let mut dependencies = HashSet::new();

        while let Some(dep) =
            seq.next_element_seed(&self.serde.dynamic_box_deserializer())?
        {
            dependencies.insert(dep);
        }

        Ok(dependencies)
    }
}

// Helper struct for deserializing version info map
struct DeserializableVersionInfoMap<'a> {
    serde: &'a crate::serde::Serde,
}

impl<'de> DeserializeSeed<'de> for DeserializableVersionInfoMap<'_> {
    type Value = HashMap<DynamicBox, VersionInfo>;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer
            .deserialize_map(VersionInfoMapVisitor { serde: self.serde })
    }
}

struct VersionInfoMapVisitor<'a> {
    serde: &'a crate::serde::Serde,
}

impl<'de> serde::de::Visitor<'de> for VersionInfoMapVisitor<'_> {
    type Value = HashMap<DynamicBox, VersionInfo>;

    fn expecting(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        write!(formatter, "a map representing version info")
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::MapAccess<'de>,
    {
        let mut version_info_map = HashMap::new();

        while let Some(key) =
            map.next_key_seed(&self.serde.dynamic_box_deserializer())?
        {
            let version_info: VersionInfo = map.next_value()?;
            version_info_map.insert(key, version_info);
        }

        Ok(version_info_map)
    }
}

// Helper struct for deserializing cyclic dependencies
struct DeserializableCyclicDependencies<'a> {
    serde: &'a crate::serde::Serde,
}

impl<'de> DeserializeSeed<'de> for DeserializableCyclicDependencies<'_> {
    type Value = Vec<CyclicDependency>;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer
            .deserialize_seq(CyclicDependenciesVisitor { serde: self.serde })
    }
}

struct CyclicDependenciesVisitor<'a> {
    serde: &'a crate::serde::Serde,
}

impl<'de> serde::de::Visitor<'de> for CyclicDependenciesVisitor<'_> {
    type Value = Vec<CyclicDependency>;

    fn expecting(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        write!(formatter, "a sequence of cyclic dependencies")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        let mut cyclic_dependencies = Vec::new();

        while let Some(cyclic_dep) =
            seq.next_element_seed(DeserializableCyclicDependency {
                serde: self.serde,
            })?
        {
            cyclic_dependencies.push(cyclic_dep);
        }

        Ok(cyclic_dependencies)
    }
}

// Helper struct for deserializing individual cyclic dependency
struct DeserializableCyclicDependency<'a> {
    serde: &'a crate::serde::Serde,
}

impl<'de> DeserializeSeed<'de> for DeserializableCyclicDependency<'_> {
    type Value = CyclicDependency;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_struct(
            "CyclicDependency",
            &["records_stack"],
            CyclicDependencyVisitor { serde: self.serde },
        )
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Deserialize,
)]
#[serde(field_identifier)]
enum CyclicDependencyField {
    #[serde(rename = "records_stack")]
    RecordsStack,
}

struct CyclicDependencyVisitor<'a> {
    serde: &'a crate::serde::Serde,
}

impl<'de> serde::de::Visitor<'de> for CyclicDependencyVisitor<'_> {
    type Value = CyclicDependency;

    fn expecting(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        write!(formatter, "struct CyclicDependency")
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::MapAccess<'de>,
    {
        let mut records_stack = None;

        while let Some(key) = map.next_key::<CyclicDependencyField>()? {
            match key {
                CyclicDependencyField::RecordsStack => {
                    if records_stack.is_some() {
                        return Err(serde::de::Error::duplicate_field(
                            "records_stack",
                        ));
                    }
                    records_stack = Some(map.next_value_seed(
                        DeserializableRecordsStack { serde: self.serde },
                    )?);
                }
            }
        }

        let records_stack = records_stack
            .ok_or_else(|| serde::de::Error::missing_field("records_stack"))?;

        Ok(CyclicDependency { records_stack })
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        let records_stack = seq
            .next_element_seed(DeserializableRecordsStack {
                serde: self.serde,
            })?
            .ok_or_else(|| serde::de::Error::invalid_length(0, &self))?;

        Ok(CyclicDependency { records_stack })
    }
}

// Helper struct for deserializing records stack
struct DeserializableRecordsStack<'a> {
    serde: &'a crate::serde::Serde,
}

impl<'de> DeserializeSeed<'de> for DeserializableRecordsStack<'_> {
    type Value = Vec<DynamicBox>;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_seq(self)
    }
}

impl<'de> serde::de::Visitor<'de> for DeserializableRecordsStack<'_> {
    type Value = Vec<DynamicBox>;

    fn expecting(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        write!(formatter, "a sequence of records")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        let mut records_stack = Vec::new();

        while let Some(record) =
            seq.next_element_seed(&self.serde.dynamic_box_deserializer())?
        {
            records_stack.push(record);
        }

        Ok(records_stack)
    }
}

/// Stores the error information about a cyclic dependency in the call graph.
#[derive(Debug, Clone)]
pub struct CyclicDependency {
    /// The stack of records that caused the cyclic dependency.
    pub records_stack: Vec<DynamicBox>,
}

impl std::fmt::Debug for CallGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CallGraph").finish_non_exhaustive()
    }
}

#[derive(Debug, Clone, Copy)]
pub(super) struct SerializableCyclicDependency<'a> {
    pub(super) cyclic_dependency: &'a CyclicDependency,
    pub(super) serde: &'a crate::serde::Serde,
}

impl Serialize for SerializableCyclicDependency<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        #[derive(Debug, Clone, Copy)]
        pub(super) struct SerializableDynamicBoxSeq<'a> {
            pub(super) seq: &'a [DynamicBox],
            pub(super) serde: &'a crate::serde::Serde,
        }

        impl Serialize for SerializableDynamicBoxSeq<'_> {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                let mut seq = serializer.serialize_seq(Some(self.seq.len()))?;
                for item in self.seq {
                    seq.serialize_element(&item.serializable(self.serde))?;
                }
                seq.end()
            }
        }

        let mut state = serializer.serialize_struct("CyclicDependency", 1)?;
        state.serialize_field("records_stack", &SerializableDynamicBoxSeq {
            seq: &self.cyclic_dependency.records_stack,
            serde: self.serde,
        })?;
        state.end()
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
    pub fn set_input<K: Key + Dynamic>(
        &mut self,
        key: &K,
        value: K::Value,
        overwrite: bool,
    ) -> Result<(), String> {
        let invalidate = self.map.entry(key.clone(), |entry| match entry {
            dashmap::Entry::Occupied(mut occupied_entry) => {
                if overwrite {
                    let old_value = occupied_entry.get();

                    if *old_value == value {
                        Ok(false)
                    } else {
                        occupied_entry.insert(value);
                        Ok(true)
                    }
                } else {
                    K::merge_value(occupied_entry.get_mut(), value).map(|x| !x)
                }
            }

            dashmap::Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(value);
                Ok(false)
            }
        })?;

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
                    // bump the version for the new input setting
                    if *self.last_was_query.get_mut() {
                        self.version += 1;
                        *self.last_was_query.get_mut() = false;
                    }

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

        Ok(())
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
        self.query_internal(key, self.database.call_graph.lock()).0
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
        Result<T::Value, crate::runtime::executor::CyclicError>,
        MutexGuard<'a, CallGraph>,
    ) {
        let key_smallbox = DynamicBox(key.smallbox_clone());

        let called_from = call_graph.called_from();

        // query has already been computed return the result
        let Some(mut result) = self.database.map.get(key) else {
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

            return (Ok(self.database.map.get(key).unwrap()), call_graph);
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
            result = self.database.map.get(key).unwrap();
        } else {
            call_graph
                .version_info_by_keys
                .get_mut(&key_smallbox)
                .unwrap()
                .verfied_at_version = self.database.version;
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
                                });
                            }
                        }
                    }

                    call_graph
                        .cyclic_dependencies
                        .push(CyclicDependency { records_stack: stack });

                    // insert a default value for the cyclic dependency
                    self.database
                        .map
                        .insert(key.clone(), <T::Value as Default>::default());

                    call_graph.version_info_by_keys.insert(
                        DynamicBox(key.smallbox_clone()),
                        VersionInfo {
                            updated_at_version: self.database.version,
                            verfied_at_version: self.database.version,
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
        call_graph = self.database.call_graph.lock();

        // Handle the executor result
        match executor_result {
            Ok(value) => {
                let updated =
                    self.database.map.entry(key.clone(), |entry| match entry {
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
                        version_info.verfied_at_version = self.database.version;
                        version_info.kind = Kind::Derived {
                            defaulted_by_cyclic_dependency: false,
                        };

                        if updated {
                            version_info.updated_at_version =
                                self.database.version;
                        }
                    }
                    Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(VersionInfo {
                            updated_at_version: self.database.version,
                            verfied_at_version: self.database.version,
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
                        updated_at_version: self.database.version,
                        verfied_at_version: self.database.version,
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
                        });
                    }
                }

                // Cyclic dependency detected - store default value and mark as
                // cyclic
                let default_value = <K::Value as Default>::default();

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
