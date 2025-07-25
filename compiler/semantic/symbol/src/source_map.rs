//! Contains the definition of the [`SourceMap`] type, which implements the
//! [`codespan_reporting::files::Files`] trait for use with
//! `codespan_reporting`.
use std::{collections::HashMap, fmt::Display, path::Path, sync::Arc};

use pernixc_extend::extend;
use pernixc_lexical::tree::RelativeLocation;
use pernixc_query::TrackedEngine;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::{ByteIndex, SourceFile, Span};
use pernixc_stable_hash::StableHash;
use pernixc_target::{Global, TargetID};

/// A query for retrieving the a path of the given sourcce file ID.
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
    StableHash,
    pernixc_query::Key,
)]
#[value(Arc<Path>)]
#[extend(method(get_source_file_path), no_cyclic)]
pub struct FilePathKey(pub Global<pernixc_arena::ID<SourceFile>>);

#[pernixc_query::executor(key(FilePathKey), name(FilePathExecutor))]
pub async fn file_path_executor(
    FilePathKey(source_file_id): &FilePathKey,
    engine: &TrackedEngine,
) -> Result<Arc<Path>, pernixc_query::runtime::executor::CyclicError> {
    let table = engine.query(&crate::MapKey(source_file_id.target_id)).await?;

    Ok(table.paths_by_source_id.get(&source_file_id.id).map_or_else(
        || panic!("Source file path not found for ID: {:?}", source_file_id.id),
        |x| x.0.clone(),
    ))
}

/// A wrapper around [`TrackedEngine`] to implement
/// `codespan_reporting::files::Files` for use with `codespan_reporting`.
#[derive(Debug, Clone)]
pub struct SourceMap(
    pub HashMap<Global<pernixc_arena::ID<SourceFile>>, Arc<SourceFile>>,
);

/// Creates a new [`SourceMap`] that will allow a code span reporting to
/// retrieve source files by their IDs.
#[extend]
pub async fn create_source_map(
    self: &TrackedEngine,
    target_id: TargetID,
) -> SourceMap {
    let map: crate::Map = self.query(&crate::MapKey(target_id)).await.unwrap();
    let mut source_files = HashMap::default();

    for (id, source_file) in map.paths_by_source_id.iter() {
        source_files.insert(
            Global::new(target_id, *id),
            self.query(&pernixc_source_file::Key {
                path: source_file.0.clone(),
                target_id,
            })
            .await
            .unwrap()
            .unwrap(),
        );
    }

    SourceMap(source_files)
}

/// A wrapper around [`Arc<Path>`] to implement `Display` for use with
/// `codespan_reporting`.
#[derive(Debug)]
pub struct PathDisplay<'x>(std::path::Display<'x>);

impl Display for PathDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

/// A wrapper around [`Arc<SourceFile>`] to implement `AsRef<str>` for use with
/// `codespan_reporting`.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceFileStr(Arc<SourceFile>);

impl AsRef<str> for SourceFileStr {
    fn as_ref(&self) -> &str { self.0.content() }
}

/*
 * NOTE: Due to the `Files` trait defined in `codespan_reporting` is not
 * async, we need to use `Handle::current().block_on(async move { ... })` to
 * execute the async code, which according to the documentation, it's
 * possible that it could cause deadlocks. However, in  this case, it's
 * mostly safe due to it's only related to loading the source file which
 * is simply a cache lookup operation.
 */

impl<'x> codespan_reporting::files::Files<'x> for SourceMap {
    type FileId = Global<pernixc_arena::ID<SourceFile>>;

    type Name = PathDisplay<'x>;

    type Source = &'x str;

    fn name(
        &'x self,
        id: Self::FileId,
    ) -> Result<Self::Name, codespan_reporting::files::Error> {
        self.0
            .get(&id)
            .map(|source_file| PathDisplay(source_file.path().display()))
            .ok_or(codespan_reporting::files::Error::FileMissing)
    }

    fn source(
        &'x self,
        id: Self::FileId,
    ) -> Result<Self::Source, codespan_reporting::files::Error> {
        self.0
            .get(&id)
            .map(|source_file| source_file.content())
            .ok_or(codespan_reporting::files::Error::FileMissing)
    }

    fn line_index(
        &self,
        id: Self::FileId,
        byte_index: usize,
    ) -> Result<usize, codespan_reporting::files::Error> {
        let source_file = self
            .0
            .get(&id)
            .ok_or(codespan_reporting::files::Error::FileMissing)?;

        if byte_index == source_file.content().len() {
            return Ok(if source_file.lines().is_empty() {
                0
            } else {
                source_file.lines().len() - 1
            });
        }

        let line = source_file.get_line_of_byte_index(byte_index).ok_or(
            codespan_reporting::files::Error::IndexTooLarge {
                given: byte_index,
                max: source_file.content().len(),
            },
        )?;

        Ok(line)
    }

    fn line_range(
        &self,
        id: Self::FileId,
        line_index: usize,
    ) -> Result<std::ops::Range<usize>, codespan_reporting::files::Error> {
        let source_file = self
            .0
            .get(&id)
            .ok_or(codespan_reporting::files::Error::FileMissing)?;

        if line_index == source_file.lines().len() {
            return Ok(source_file
                .lines()
                .last()
                .map_or(0..0, |x| x.start..x.end));
        }

        let line_range = source_file.lines().get(line_index).ok_or({
            codespan_reporting::files::Error::IndexTooLarge {
                given: line_index,
                max: source_file.lines().len(),
            }
        })?;

        Ok(line_range.clone())
    }
}

/// Converts the given relative span to an absolute span.
///
/// This method uses the token tree in order to calculate an absolute position.
#[extend]
pub async fn to_absolute_span(
    self: &TrackedEngine,
    relative_span: &Span<RelativeLocation>,
) -> Span<ByteIndex> {
    let path = self.get_source_file_path(relative_span.source_id).await;
    let file = self
        .query(&pernixc_source_file::Key {
            path: path.clone(),
            target_id: relative_span.source_id.target_id,
        })
        .await
        .unwrap()
        .unwrap();

    let token_tree = self
        .query(&pernixc_lexical::Key {
            path,
            target_id: relative_span.source_id.target_id,
        })
        .await
        .unwrap()
        .unwrap()
        .0;

    relative_span.to_absolute_span(&file, &token_tree)
}
