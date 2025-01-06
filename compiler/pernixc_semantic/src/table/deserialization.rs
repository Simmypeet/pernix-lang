//! Contains the code for deserializing the table.

use std::{
    collections::{hash_map::Entry, HashMap},
    fmt::{Debug, Display},
    hash::Hash,
};

use derive_new::new;
use pernixc_component::serde::Reflector;
use serde::{
    de::{DeserializeSeed, Visitor},
    Deserialize,
};

use super::{CompilationMetaData, GlobalID, Target, TargetID};
use crate::table::Table;

/// A struct used for incrementally deserialize the table and merge them
/// together.
#[derive(Debug, new)]
pub struct IncrementalLibraryDeserializer<'t, 'r, T, E: Display + 'static> {
    table: &'t mut Table,
    reflector: &'r Reflector<T, GlobalID, E>,
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
        reflector: &'r Reflector<T, GlobalID, E>,
    ) -> IncrementalLibraryDeserializer<'t, 'r, T, E> {
        IncrementalLibraryDeserializer::new(self, reflector)
    }
}

impl<
        'de,
        'current,
        't,
        'r,
        T: for<'x> Deserialize<'x> + Eq + Hash + Debug,
        E: Display + 'static,
    > DeserializeSeed<'de>
    for &'current mut IncrementalLibraryDeserializer<'t, 'r, T, E>
{
    type Value = CompilationMetaData;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_map(self)
    }
}

impl<
        'de,
        'current,
        't,
        'r,
        T: for<'x> Deserialize<'x> + Eq + Hash + Debug,
        E: Display + 'static,
    > Visitor<'de>
    for &'current mut IncrementalLibraryDeserializer<'t, 'r, T, E>
{
    type Value = CompilationMetaData;

    fn expecting(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        write!(
            formatter,
            "a map with the fields `representation` and \
             `compilation_meta_data`"
        )
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::MapAccess<'de>,
    {
        let mut representation_found = false;
        let mut compoilation_meta_data = None;

        while let Some(key) = map.next_key::<String>()? {
            match key.as_str() {
                "representation" => {
                    if representation_found {
                        return Err(serde::de::Error::custom(
                            "The field `representation` has already been \
                             deserialized",
                        ));
                    }

                    representation_found = true;
                    map.next_value_seed(
                        IncrementalRepresentationDeserializer(self),
                    )?;
                }
                "compilation_meta_data" => {
                    if compoilation_meta_data.is_some() {
                        return Err(serde::de::Error::custom(
                            "The field `compilation_meta_data` has already \
                             been deserialized",
                        ));
                    }

                    compoilation_meta_data = Some(map.next_value()?);
                }
                _ => {
                    return Err(serde::de::Error::unknown_field(&key, &[
                        "representation",
                        "compilation_meta_data",
                    ]));
                }
            }
        }

        if !representation_found {
            return Err(serde::de::Error::missing_field("representation"));
        }

        if let Some(compilation_meta_data) = compoilation_meta_data {
            Ok(compilation_meta_data)
        } else {
            Err(serde::de::Error::missing_field("compilation_meta_data"))
        }
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
        deserializer.deserialize_map(self)
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
        write!(
            formatter,
            "a map with the fields `storage`, `targets_by_id`, and \
             `targets_by_name`"
        )
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::MapAccess<'de>,
    {
        while let Some(key) = map.next_key::<String>()? {
            match key.as_str() {
                "storage" => {
                    let inplace_deserializer = self
                        .0
                        .table
                        .storage
                        .as_inplace_deserializer(self.0.reflector);

                    map.next_value_seed(&inplace_deserializer)?;
                }

                "targets_by_id" => {
                    let new_targets_by_id: HashMap<TargetID, Target> =
                        map.next_value()?;

                    for (id, target) in new_targets_by_id.into_iter() {
                        match self.0.table.targets_by_id.entry(id) {
                            Entry::Occupied(entry) => {
                                if entry.get() != &target {
                                    return Err(serde::de::Error::custom(
                                        format!(
                                            "Target ID {:?} already exists in \
                                             the table",
                                            id
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
                "targets_by_name" => {
                    let new_targets_by_name: HashMap<String, TargetID> =
                        map.next_value()?;

                    for (name, id) in new_targets_by_name.into_iter() {
                        match self.0.table.targets_by_name.entry(name.clone()) {
                            Entry::Occupied(entry) => {
                                if entry.get() != &id {
                                    return Err(serde::de::Error::custom(
                                        format!(
                                            "Target name {:?} already exists \
                                             in the table",
                                            name
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
                _ => {
                    return Err(serde::de::Error::unknown_field(&key, &[
                        "storage",
                        "targets_by_id",
                        "targets_by_name",
                    ]));
                }
            }
        }

        Ok(())
    }
}
