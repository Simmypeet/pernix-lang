//! A module for loading source files and building the initial engine state.

use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use flexstr::SharedStr;
use pernixc_query::Identifiable;
use pernixc_serialize::{
    de::{
        Deserializer, Error as _, FieldAccess as _, Identifier, StructAccess,
    },
    ser::{Serializer, Struct as _},
    Deserialize, Serialize,
};
use pernixc_stable_hash::StableHash;

/// Query for loading source files content from the file system.
///
/// This query is im-pure and should be re-evaluated every time it's loaded from
/// the persistence layer to trigger the re-verification of the query that
/// depends on it.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Identifiable, StableHash,
)]
pub struct Key {
    /// The path to load the source file.
    pub path: Arc<Path>,

    /// The target name that requested the source file.
    pub target_name: SharedStr,
}

impl<S: Serializer<E>, E> Serialize<S, E> for Key {
    fn serialize(
        &self,
        serializer: &mut S,
        extension: &E,
    ) -> Result<(), S::Error> {
        serializer.emit_struct("Key", 2, extension, |mut f, e| {
            f.serialize_field("path", self.path.as_ref(), e)?;
            f.serialize_field("target_name", self.target_name.as_str(), e)?;

            Ok(())
        })
    }
}

impl<D: Deserializer<E>, E> Deserialize<D, E> for Key {
    fn deserialize(
        deserializer: &mut D,
        extension: &E,
    ) -> Result<Self, D::Error> {
        deserializer.expect_struct(
            "Key",
            &["path", "target_name"],
            extension,
            |mut f, e| {
                let mut path: Option<Arc<Path>> = None;
                let mut target_name: Option<SharedStr> = None;

                loop {
                    if !f.next_field(e, |field| {
                        let Some((identifier, access, extension)) = field
                        else {
                            return Ok(false);
                        };

                        match identifier {
                            Identifier::Index(0) | Identifier::Name("path") => {
                                if path.is_some() {
                                    return Err(D::Error::duplicated_field(
                                        identifier,
                                    ));
                                }

                                path = Some(Arc::from(
                                    access.deserialize::<PathBuf>(extension)?,
                                ));
                            }
                            Identifier::Index(1)
                            | Identifier::Name("target_name") => {
                                if target_name.is_some() {
                                    return Err(D::Error::duplicated_field(
                                        identifier,
                                    ));
                                }

                                target_name = Some(
                                    access
                                        .deserialize::<SharedStr>(extension)?,
                                );
                            }

                            _ => {
                                return Err(D::Error::unknown_field(
                                    identifier,
                                ));
                            }
                        }

                        Ok(true)
                    })? {
                        break;
                    }
                }

                let path = path.ok_or_else(|| {
                    D::Error::missing_field(Identifier::Name("path"))
                })?;

                let target_name = target_name.ok_or_else(|| {
                    D::Error::missing_field(Identifier::Name("target_name"))
                })?;

                Ok(Self { path, target_name })
            },
        )
    }
}

impl pernixc_query::Key for Key {
    /// The [`Ok`] value represents the source file content, while the [`Err`]
    /// is the string to report the error.
    type Value = Arc<Result<Arc<String>, String>>;
}

/// The executor used by the [`Key`] to load the source file
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Executor;

fn load_source_file(path: &Path) -> Result<Arc<String>, String> {
    std::fs::read_to_string(path).map_err(|x| x.to_string()).map(Arc::new)
}

impl pernixc_query::runtime::executor::Executor<Key> for Executor {
    fn execute(
        &self,
        _: &pernixc_query::TrackedEngine,
        key: &Key,
    ) -> Result<
        Arc<Result<Arc<String>, String>>,
        pernixc_query::runtime::executor::CyclicError,
    > {
        Ok(Arc::new(load_source_file(&key.path)))
    }
}
