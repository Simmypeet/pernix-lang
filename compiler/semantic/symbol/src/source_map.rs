//! Contains the definition of the [`SourceMap`] type, which implements the
//! [`codespan_reporting::files::Files`] trait for use with
//! `codespan_reporting`.
use std::{fmt::Display, path::Path, sync::Arc};

use pernixc_extend::extend;
use pernixc_lexical::tree::RelativeLocation;
use pernixc_query::TrackedEngine;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::{ByteIndex, SourceFile, Span};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;
use tokio::runtime::Handle;

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
pub struct SourceMap(pub TrackedEngine);

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

/*
 * NOTE: Due to the `Files` trait defined in `codespan_reporting` is not
 * async, we need to use `Handle::current().block_on(async move { ... })` to
 * execute the async code, which according to the documentation, it's
 * possible that it could cause deadlocks. However, in  this case, it's
 * mostly safe due to it's only related to loading the source file which
 * is simply a cache lookup operation.
 */

impl codespan_reporting::files::Files<'_> for SourceMap {
    type FileId = Global<pernixc_arena::ID<SourceFile>>;

    type Name = PathDisplay;

    type Source = SourceFileStr;

    fn name(
        &self,
        id: Self::FileId,
    ) -> Result<Self::Name, codespan_reporting::files::Error> {
        Ok(Handle::current().block_on(async move {
            PathDisplay(self.0.query(&FilePathKey(id)).await.unwrap())
        }))
    }

    fn source(
        &self,
        id: Self::FileId,
    ) -> Result<Self::Source, codespan_reporting::files::Error> {
        Handle::current().block_on(async move {
            let path = self.0.get_source_file_path(id).await;

            Ok(SourceFileStr(
                self.0
                    .query(&pernixc_source_file::Key {
                        path,
                        target_id: id.target_id,
                    })
                    .await
                    .unwrap()
                    .unwrap(),
            ))
        })
    }

    fn line_index(
        &self,
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
        &self,
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
