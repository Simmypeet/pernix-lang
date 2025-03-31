//! Contains the code for deserializing the table.

use std::{
    collections::{hash_map::Entry, HashMap},
    fmt::{Debug, Display},
    hash::Hash,
};

use derive_new::new;
use pernixc_storage::{serde::Reflector, ArcTrait};
use serde::{
    de::{DeserializeSeed, Visitor},
    Deserialize,
};

use super::{CompilationMetaData, GlobalID, Target, TargetID};
use crate::Table;

/// A struct used for incrementally deserialize the table and merge them
/// together.
#[derive(new)]
#[allow(missing_debug_implementations)]
pub struct IncrementalLibraryDeserializer<'t, 'r, T, E: Display + 'static> {
    table: &'t mut Table,
    reflector: &'r Reflector<GlobalID, ArcTrait, T, E>,
}

impl Table {
    /// Creates a seeded deserializer that can be used to deserialize the table
    /// incrementally.
    pub fn as_incremental_library_deserializer<
        't,
        'r,
        T,
        E: Display + 'static,
    >(
        &'t mut self,
        reflector: &'r Reflector<GlobalID, ArcTrait, T, E>,
    ) -> IncrementalLibraryDeserializer<'t, 'r, T, E> {
        IncrementalLibraryDeserializer::new(self, reflector)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum LibraryField {
    Representation,
    CompilationMetaData,
    Ignored,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct LibraryFieldVisitor;

impl Visitor<'_> for LibraryFieldVisitor {
    type Value = LibraryField;

    fn expecting(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        write!(formatter, "field identifier")
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        match value {
            "representation" => Ok(LibraryField::Representation),
            "compilation_meta_data" => Ok(LibraryField::CompilationMetaData),
            _ => Ok(LibraryField::Ignored),
        }
    }

    fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        match value {
            0 => Ok(LibraryField::Representation),
            1 => Ok(LibraryField::CompilationMetaData),
            _ => Ok(LibraryField::Ignored),
        }
    }

    fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        match v {
            b"representation" => Ok(LibraryField::Representation),
            b"compilation_meta_data" => Ok(LibraryField::CompilationMetaData),
            _ => Ok(LibraryField::Ignored),
        }
    }
}

impl<'x> Deserialize<'x> for LibraryField {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'x>,
    {
        deserializer.deserialize_identifier(LibraryFieldVisitor)
    }
}

impl<
        'de,
        T: for<'x> Deserialize<'x> + Eq + Hash + Debug,
        E: Display + 'static,
    > DeserializeSeed<'de>
    for &mut IncrementalLibraryDeserializer<'_, '_, T, E>
{
    type Value = CompilationMetaData;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_struct(
            "Library",
            &["representation", "compilation_meta_data"],
            self,
        )
    }
}

impl<
        'de,
        T: for<'x> Deserialize<'x> + Eq + Hash + Debug,
        E: Display + 'static,
    > Visitor<'de> for &mut IncrementalLibraryDeserializer<'_, '_, T, E>
{
    type Value = CompilationMetaData;

    fn expecting(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        write!(formatter, "struct Library")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        seq.next_element_seed(IncrementalRepresentationDeserializer(self))?
            .ok_or_else(|| {
                serde::de::Error::invalid_length(
                    0,
                    &"struct Library with 2 elements",
                )
            })?;

        let metadata =
            seq.next_element::<CompilationMetaData>()?.ok_or_else(|| {
                serde::de::Error::invalid_length(
                    1,
                    &"struct Library with 2 elements",
                )
            })?;

        Ok(metadata)
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::MapAccess<'de>,
    {
        let mut visited_representation = false;
        let mut compilation_meta_data = None;

        while let Some(key) = map.next_key()? {
            match key {
                LibraryField::Representation => {
                    if visited_representation {
                        return Err(serde::de::Error::duplicate_field(
                            "representation",
                        ));
                    }
                    visited_representation = true;
                    map.next_value_seed(
                        IncrementalRepresentationDeserializer(self),
                    )?;
                }
                LibraryField::CompilationMetaData => {
                    if compilation_meta_data.is_some() {
                        return Err(serde::de::Error::duplicate_field(
                            "compilation_meta_data",
                        ));
                    }
                    compilation_meta_data = Some(map.next_value()?);
                }
                LibraryField::Ignored => {
                    map.next_value::<serde::de::IgnoredAny>()?;
                }
            }
        }

        if !visited_representation {
            return Err(serde::de::Error::missing_field("representation"));
        }

        compilation_meta_data.ok_or_else(|| {
            serde::de::Error::missing_field("compilation_meta_data")
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum RepresentationField {
    Storage,
    TargetsByID,
    TargetsByName,
    Ignored,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct RepresentationFieldVisitor;

impl Visitor<'_> for RepresentationFieldVisitor {
    type Value = RepresentationField;

    fn expecting(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        write!(formatter, "field identifier")
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        match value {
            "storage" => Ok(RepresentationField::Storage),
            "targets_by_id" => Ok(RepresentationField::TargetsByID),
            "targets_by_name" => Ok(RepresentationField::TargetsByName),
            _ => Ok(RepresentationField::Ignored),
        }
    }

    fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        match value {
            0 => Ok(RepresentationField::Storage),
            1 => Ok(RepresentationField::TargetsByID),
            2 => Ok(RepresentationField::TargetsByName),
            _ => Ok(RepresentationField::Ignored),
        }
    }

    fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        match v {
            b"storage" => Ok(RepresentationField::Storage),
            b"targets_by_id" => Ok(RepresentationField::TargetsByID),
            b"targets_by_name" => Ok(RepresentationField::TargetsByName),
            _ => Ok(RepresentationField::Ignored),
        }
    }
}

impl<'x> Deserialize<'x> for RepresentationField {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'x>,
    {
        deserializer.deserialize_identifier(RepresentationFieldVisitor)
    }
}

struct IncrementalRepresentationDeserializer<
    'current,
    't,
    'r,
    T,
    E: Display + 'static,
>(&'current mut IncrementalLibraryDeserializer<'t, 'r, T, E>);

impl<
        'de,
        T: for<'x> Deserialize<'x> + Eq + Hash + Debug,
        E: Display + 'static,
    > DeserializeSeed<'de>
    for IncrementalRepresentationDeserializer<'_, '_, '_, T, E>
{
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_struct(
            "Representation",
            &["storage", "targets_by_id", "targets_by_name"],
            self,
        )
    }
}

impl<
        'de,
        T: for<'x> Deserialize<'x> + Eq + Hash + Debug,
        E: Display + 'static,
    > Visitor<'de> for IncrementalRepresentationDeserializer<'_, '_, '_, T, E>
{
    type Value = ();

    fn expecting(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        write!(formatter, "struct Representation")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        let inplace_deserializer =
            self.0.table.storage.as_inplace_deserializer(self.0.reflector);

        seq.next_element_seed(&inplace_deserializer)?.ok_or_else(|| {
            serde::de::Error::invalid_length(
                0,
                &"struct Representation with 3 elements",
            )
        })?;

        let new_targets_by_id: HashMap<TargetID, Target> =
            seq.next_element()?.ok_or_else(|| {
                serde::de::Error::invalid_length(
                    1,
                    &"struct Representation with 3 elements",
                )
            })?;

        for (id, target) in new_targets_by_id {
            match self.0.table.targets_by_id.entry(id) {
                Entry::Occupied(entry) => {
                    if entry.get() != &target {
                        return Err(serde::de::Error::custom(format!(
                            "Target ID {id:?} already exists in the table",
                        )));
                    }
                }
                Entry::Vacant(entry) => {
                    entry.insert(target);
                }
            }
        }

        let new_targets_by_name: HashMap<String, TargetID> =
            seq.next_element()?.ok_or_else(|| {
                serde::de::Error::invalid_length(
                    2,
                    &"struct Representation with 3 elements",
                )
            })?;

        for (name, id) in new_targets_by_name {
            match self.0.table.targets_by_name.entry(name.clone()) {
                Entry::Occupied(entry) => {
                    if entry.get() != &id {
                        return Err(serde::de::Error::custom(format!(
                            "Target name {name:?} already exists in the table",
                        )));
                    }
                }
                Entry::Vacant(entry) => {
                    entry.insert(id);
                }
            }
        }

        Ok(())
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::MapAccess<'de>,
    {
        let mut visited_storage = false;
        let mut visited_targets_by_id = false;
        let mut visited_targets_by_name = false;

        while let Some(key) = map.next_key()? {
            match key {
                RepresentationField::Storage => {
                    if visited_storage {
                        return Err(serde::de::Error::duplicate_field(
                            "storage",
                        ));
                    }
                    visited_storage = true;
                    let inplace_deserializer = self
                        .0
                        .table
                        .storage
                        .as_inplace_deserializer(self.0.reflector);
                    map.next_value_seed(&inplace_deserializer)?;
                }
                RepresentationField::TargetsByID => {
                    if visited_targets_by_id {
                        return Err(serde::de::Error::duplicate_field(
                            "targets_by_id",
                        ));
                    }
                    visited_targets_by_id = true;
                    let new_targets_by_id: HashMap<TargetID, Target> =
                        map.next_value()?;
                    for (id, target) in new_targets_by_id {
                        match self.0.table.targets_by_id.entry(id) {
                            Entry::Occupied(entry) => {
                                if entry.get() != &target {
                                    return Err(serde::de::Error::custom(
                                        format!(
                                            "Target ID {id:?} already exists \
                                             in the table",
                                        ),
                                    ));
                                }
                            }
                            Entry::Vacant(entry) => {
                                entry.insert(target);
                            }
                        }
                    }
                }
                RepresentationField::TargetsByName => {
                    if visited_targets_by_name {
                        return Err(serde::de::Error::duplicate_field(
                            "targets_by_name",
                        ));
                    }
                    visited_targets_by_name = true;
                    let new_targets_by_name: HashMap<String, TargetID> =
                        map.next_value()?;
                    for (name, id) in new_targets_by_name {
                        match self.0.table.targets_by_name.entry(name.clone()) {
                            Entry::Occupied(entry) => {
                                if entry.get() != &id {
                                    return Err(serde::de::Error::custom(
                                        format!(
                                            "Target name {name:?} already \
                                             exists in the table",
                                        ),
                                    ));
                                }
                            }
                            Entry::Vacant(entry) => {
                                entry.insert(id);
                            }
                        }
                    }
                }
                RepresentationField::Ignored => {
                    map.next_value::<serde::de::IgnoredAny>()?;
                }
            }
        }

        if !visited_storage {
            return Err(serde::de::Error::missing_field("storage"));
        }

        if !visited_targets_by_id {
            return Err(serde::de::Error::missing_field("targets_by_id"));
        }

        if !visited_targets_by_name {
            return Err(serde::de::Error::missing_field("targets_by_name"));
        }

        Ok(())
    }
}
