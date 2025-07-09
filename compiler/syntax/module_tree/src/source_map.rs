use std::{fmt::Display, path::Path, sync::Arc};

use pernixc_query::TrackedEngine;
use pernixc_source_file::{GlobalSourceID, SourceFile};

/// A wrapper around [`TrackedEngine`] to implement
/// `codespan_reporting::files::Files` for use with `codespan_reporting`.
#[derive(Debug, Clone, Copy)]
pub struct SourceMap<'a>(&'a TrackedEngine<'a>);

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
pub struct SourceFileDisplay(Arc<SourceFile>);

impl AsRef<str> for SourceFileDisplay {
    fn as_ref(&self) -> &str { self.0.content() }
}

impl<'a> codespan_reporting::files::Files<'a> for SourceMap<'a> {
    type FileId = GlobalSourceID;

    type Name = PathDisplay;

    type Source = SourceFileDisplay;

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
            .query(&crate::source_file::Key {
                path: path.0,
                target_id: id.target_id,
            })
            .unwrap()
            .map_err(|_| codespan_reporting::files::Error::FileMissing)?;

        Ok(SourceFileDisplay(source_file))
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
