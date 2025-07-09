//! A module that provides a wrapper around [`TrackedEngine`] to implement
//! `codespan_reporting::files::Files` for use with `codespan_reporting`.

use std::{fmt::Display, path::Path, sync::Arc};

use pernixc_query::TrackedEngine;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::{GlobalSourceID, SourceFile};
use pernixc_stable_hash::StableHash;

/// A wrapper around [`TrackedEngine`] to implement
/// `codespan_reporting::files::Files` for use with `codespan_reporting`.
#[derive(Debug, Clone, Copy)]
pub struct SourceMap<'a>(pub &'a TrackedEngine<'a>);

/// A wrapper around [`Arc<Path>`] to implement `Display` for use with
/// `codespan_reporting`.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PathDisplay(Arc<Path>);

impl Display for PathDisplay {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.display().fmt(f)
    }
}

/// A wrapper around [`Arc<SourceFile>`] to implement `AsRef<str>` for use with
/// `codespan_reporting`.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceFileStr(Arc<SourceFile>);

impl AsRef<str> for SourceFileStr {
    fn as_ref(&self) -> &str { self.0.content() }
}

/// Query for retrieving a [`Arc<SourceFile>`] from the given
/// [`GlobalSourceID`].
///
/// The given [`GlobalSourceID`] must be a valid ID and obtained from the module
/// tree.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    pernixc_query::Key,
)]
#[value(Arc<SourceFile>)]
pub struct Key(pub GlobalSourceID);

/// An executor for the [`Key`] query that retrieves the source file from the
/// module tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Executor;

impl pernixc_query::runtime::executor::Executor<Key> for Executor {
    fn execute(
        &self,
        engine: &TrackedEngine,
        key: &Key,
    ) -> Result<Arc<SourceFile>, pernixc_query::runtime::executor::CyclicError>
    {
        // Since the `GlobalSourceID` provided is always a valid ID derived from
        // the `ModuleTree`, we can safely unwrap the query result.
        let map =
            engine.query(&crate::path::Key(key.0.target_id)).unwrap().unwrap();

        let path = map.get(&key.0.id).unwrap();

        let source_file = engine
            .query(&crate::load_source_file::Key {
                path: path.clone(),
                target_id: key.0.target_id,
            })
            .unwrap()
            .unwrap();

        Ok(source_file)
    }
}

impl<'a> codespan_reporting::files::Files<'a> for SourceMap<'a> {
    type FileId = GlobalSourceID;

    type Name = PathDisplay;

    type Source = SourceFileStr;

    fn name(
        &'a self,
        id: Self::FileId,
    ) -> Result<Self::Name, codespan_reporting::files::Error> {
        let module_tree = self
            .0
            .query(&crate::Key(id.target_id))
            .unwrap()
            .map_err(|_| codespan_reporting::files::Error::FileMissing)?;

        let path = module_tree
            .source_file_paths_by_id
            .get(&id.id)
            .ok_or(codespan_reporting::files::Error::FileMissing)?;

        Ok(PathDisplay(path.clone()))
    }

    fn source(
        &'a self,
        id: Self::FileId,
    ) -> Result<Self::Source, codespan_reporting::files::Error> {
        let path = self.name(id)?;
        let source_file = self
            .0
            .query(&crate::load_source_file::Key {
                path: path.0,
                target_id: id.target_id,
            })
            .unwrap()
            .map_err(|_| codespan_reporting::files::Error::FileMissing)?;

        Ok(SourceFileStr(source_file))
    }

    fn line_index(
        &'a self,
        id: Self::FileId,
        byte_index: usize,
    ) -> Result<usize, codespan_reporting::files::Error> {
        let source = self.source(id)?;

        if byte_index == source.0.content().len() {
            return Ok(if source.0.lines().is_empty() {
                0
            } else {
                source.0.lines().len() - 1
            });
        }

        let line = source.0.get_line_of_byte_index(byte_index).ok_or(
            codespan_reporting::files::Error::IndexTooLarge {
                given: byte_index,
                max: source.0.content().len(),
            },
        )?;

        Ok(line)
    }

    fn line_range(
        &'a self,
        id: Self::FileId,
        line_index: usize,
    ) -> Result<std::ops::Range<usize>, codespan_reporting::files::Error> {
        let source = self.source(id)?;

        if line_index == source.0.lines().len() {
            return Ok(source
                .0
                .lines()
                .last()
                .map_or(0..0, |x| x.start..x.end));
        }

        let line_range = source.0.lines().get(line_index).ok_or({
            codespan_reporting::files::Error::IndexTooLarge {
                given: line_index,
                max: source.0.lines().len(),
            }
        })?;

        Ok(line_range.clone())
    }
}
