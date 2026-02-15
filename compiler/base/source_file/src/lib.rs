//! Contains the code related to the source code input.

use core::str;
use std::{
    fmt::Debug,
    hash::Hash,
    io::Read,
    ops::{Range, RangeBounds},
    path::Path,
    str::CharIndices,
};

use getset::{CopyGetters, Getters};
use pernixc_qbice::TrackedEngine;
use pernixc_target::{Global, TargetID};
use qbice::{
    Decode, Encode, Identifiable, Query, StableHash,
    stable_hash::{StableHasher, Value},
    storage::intern::Interned,
};
use rayon::iter::{ParallelBridge, ParallelIterator};
use ropey::{Rope, RopeSlice, iter::Chunks};

pub mod simple_source_map;

/// Represents an source file input for the compiler.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct SourceFile {
    /// Gets the full path to the source file.
    #[get = "pub"]
    path: Interned<Path>,

    rope: ropey::Rope,
}

impl Encode for SourceFile {
    fn encode<E: qbice::serialize::Encoder + ?Sized>(
        &self,
        encoder: &mut E,
        plugin: &qbice::serialize::Plugin,
        session: &mut qbice::serialize::session::Session,
    ) -> std::io::Result<()> {
        self.path.encode(encoder, plugin, session)?;

        let str = self.rope.slice(..).to_string();
        str.encode(encoder, plugin, session)?;

        Ok(())
    }
}

impl Decode for SourceFile {
    fn decode<D: qbice::serialize::Decoder + ?Sized>(
        decoder: &mut D,
        plugin: &qbice::serialize::Plugin,
        session: &mut qbice::serialize::session::Session,
    ) -> std::io::Result<Self> {
        let path = Interned::decode(decoder, plugin, session)?;
        let content = String::decode(decoder, plugin, session)?;

        Ok(Self { path, rope: ropey::Rope::from_str(&content) })
    }
}

/// Parallel hash function for the content of the source file.
fn file_content_hash<H: StableHasher + ?Sized>(
    rope: &ropey::Rope,
    hasher: &mut H,
) -> H::Hash {
    rope.chunks()
        .par_bridge()
        .map(|chunk| {
            hasher.sub_hash(&mut |x| {
                chunk.stable_hash(x);
            })
        })
        .reduce(H::Hash::default, H::Hash::wrapping_add)
}

impl StableHash for SourceFile {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        let hash = file_content_hash(&self.rope, state);
        hash.stable_hash(state);

        self.path.stable_hash(state);
    }
}

impl SourceFile {
    /// Creates a new inline source file
    #[must_use]
    pub fn from_str(content: &str, full_path: Interned<Path>) -> Self {
        Self { path: full_path, rope: ropey::Rope::from_str(content) }
    }

    /// Creates a new source file from the given reader.
    pub fn from_reader<R: Read>(
        reader: R,
        full_path: Interned<Path>,
    ) -> std::io::Result<Self> {
        Ok(Self { path: full_path, rope: ropey::Rope::from_reader(reader)? })
    }

    /// Creates a new source file with replacing the content in the given byte
    /// range with the new text.
    pub fn replace_range(&mut self, range: Range<ByteIndex>, new_text: &str) {
        self.rope.remove(range.clone());
        self.rope.insert(range.start, new_text);
    }

    /// Gets the content of the source file as a string.
    ///
    /// This is an expensive operation and should be used with caution.
    #[must_use]
    pub fn content(&self) -> String { self.rope.slice(..).to_string() }

    /// Returns the number of lines in the source file.
    ///
    /// This is an O(1) operation according to the ropey documentation.
    #[must_use]
    pub fn line_coount(&self) -> usize { self.rope.len_lines() }

    /// Gets the line at the given index, if it exists.
    ///
    /// This materializes the line as a string, so it can be expensive if the
    /// line is long.
    #[must_use]
    pub fn get_line(&self, line_index: usize) -> Option<String> {
        self.rope.get_line(line_index).map(|x| x.to_string())
    }

    /// Gets the slice of the source file content in the given byte range.
    pub fn slice(&self, range: impl RangeBounds<ByteIndex>) -> String {
        self.rope.byte_slice(range).to_string()
    }

    /// Returns an iterator over the characters of the source file that yields
    /// the global byte index of each character in the source file.
    #[must_use]
    pub fn char_indices(&self) -> SourceFileCharIndices<'_> {
        SourceFileCharIndices::new(&self.rope)
    }

    /// Gets the length of the source file in bytes.
    #[must_use]
    pub fn len_bytes(&self) -> usize { self.rope.len_bytes() }

    /// Gets the byte offset at the start of the given line (0-indexed).
    ///
    /// This is an O(log n) operation using ropey's internal indexing.
    #[must_use]
    pub fn line_to_byte(&self, line_index: usize) -> ByteIndex {
        self.rope.line_to_byte(line_index)
    }

    /// Gets the line number (0-indexed) containing the given byte offset.
    ///
    /// This is an O(log n) operation using ropey's internal indexing.
    #[must_use]
    pub fn byte_to_line(&self, byte_index: ByteIndex) -> usize {
        self.rope.byte_to_line(byte_index)
    }

    /// Gets the characters of the source file in the given byte range.
    pub fn chars_range(
        &self,
        range: impl RangeBounds<ByteIndex>,
    ) -> CharsRange<'_> {
        CharsRange { chars: self.rope.byte_slice(range).chars() }
    }

    /// Gets a hashable view of the source file content in the given byte range.
    #[must_use]
    pub fn hashable_view(
        &self,
        range: impl RangeBounds<ByteIndex>,
    ) -> HashableView<'_> {
        HashableView { slice: self.rope.byte_slice(range) }
    }

    /// Converts the given editor location (line and column) to a byte index in
    /// the source file, if the location is valid.
    ///
    /// This can be a one-past-end location at the end of the file.
    #[must_use]
    pub fn into_byte_index_include_ending(
        &self,
        location: EditorLocation,
    ) -> Option<ByteIndex> {
        if location.line >= self.line_coount() {
            return None;
        }

        let line_start = self.rope.line_to_byte(location.line);
        let line = self.rope.get_line(location.line)?;

        line.try_char_to_byte(location.column)
            .ok()
            .map(|byte_offset| line_start + byte_offset)
    }
}

/// An iterator over the characters of a source file at a particular byte range.
#[derive(Debug, Clone)]
pub struct CharsRange<'a> {
    chars: ropey::iter::Chars<'a>,
}

impl Iterator for CharsRange<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> { self.chars.next() }
}

/// An iterator over the characters of a source file that yields the global byte
/// index of each character in the source file.
#[derive(Debug, Clone)]
pub struct SourceFileCharIndices<'a> {
    // The iterator over the rope's chunks
    chunk_iter: Chunks<'a>,
    // The standard iterator for the specific chunk we are currently processing
    cur_chunk_indices: CharIndices<'a>,
    // The global byte offset where the current chunk started
    chunk_start_byte: usize,
    // The length of the current chunk (needed to update the offset when
    // switching chunks)
    cur_chunk_len: usize,
}

impl<'a> SourceFileCharIndices<'a> {
    fn new(rope: &'a Rope) -> Self {
        let mut chunk_iter = rope.chunks();

        // Initialize with the first chunk.
        // If the rope is empty, we use an empty string to ensure safety.
        let first_chunk = chunk_iter.next().unwrap_or("");

        Self {
            chunk_iter,
            cur_chunk_indices: first_chunk.char_indices(),
            chunk_start_byte: 0,
            cur_chunk_len: first_chunk.len(),
        }
    }
}

impl Iterator for SourceFileCharIndices<'_> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // 1. Try to get the next character from the current chunk
            if let Some((local_byte_idx, ch)) = self.cur_chunk_indices.next() {
                // Return global offset + local offset
                return Some((self.chunk_start_byte + local_byte_idx, ch));
            }

            // 2. If the current chunk is exhausted, move to the next chunk

            // Update the global offset by adding the length of the chunk we
            // just finished
            self.chunk_start_byte += self.cur_chunk_len;

            // Pull the next chunk from the rope
            match self.chunk_iter.next() {
                Some(next_chunk) => {
                    // Set up state for the new chunk and loop back to step 1
                    self.cur_chunk_len = next_chunk.len();
                    self.cur_chunk_indices = next_chunk.char_indices();
                }
                None => {
                    // No more chunks left, we are done
                    return None;
                }
            }
        }
    }
}

/// A wrapper around a rope slice that implements `Hash` by hashing the content
/// of the slice.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HashableView<'a> {
    slice: RopeSlice<'a>,
}

impl std::hash::Hash for HashableView<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for chunk in self.slice.chunks() {
            state.write(chunk.as_bytes());
        }
    }
}

/// Is an unsigned integer that represents a byte index in the source code.
pub type ByteIndex = usize;

/// Represents a range of characters in a source file.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Getters,
    CopyGetters,
    Encode,
    Decode,
    StableHash,
    Identifiable,
)]
pub struct Span<L> {
    /// Gets the start byte index of the span.
    pub start: L,

    /// Gets the end byte index of the span (exclusive).
    pub end: L,

    /// The ID of the source file that this span belongs to.
    pub source_id: GlobalSourceID,
}

impl<L> Span<L> {
    /// Creates an absolute span from the current span.
    ///
    /// This is useful if the type parameter `L` is a relative location and
    /// needs to be converted to an absolute byte index range in the source
    /// file.
    pub fn to_absolute_span<C: Clone>(
        &self,
        source_file: &SourceFile,
        location_context: C,
    ) -> AbsoluteSpan
    where
        L: Location<C>,
    {
        AbsoluteSpan {
            start: self
                .start
                .to_absolute_index(source_file, location_context.clone()),
            end: self.end.to_absolute_index(source_file, location_context),
            source_id: self.source_id,
        }
    }
}

/// A type alias for the [`Span`] type with a [`ByteIndex`] as the source
/// location.
pub type AbsoluteSpan = Span<ByteIndex>;

/// Implemented by the type can be used to represents a particular location in
/// the source file that can be then converted to an absolute byte index in the
/// source file.
///
/// This trait is beneficial for **relative** location types that can be then
/// converted to an absolute byte index in the source file using optional
/// context.
pub trait Location<C> {
    /// Converts the location to an absolute byte index in the source file.
    fn to_absolute_index(
        &self,
        source_file: &SourceFile,
        context: C,
    ) -> ByteIndex;
}

impl Location<()> for ByteIndex {
    fn to_absolute_index(&self, _: &SourceFile, (): ()) -> ByteIndex { *self }
}

/// A trait for types that can be joined together.
pub trait Join {
    /// Joins the starting position of this span with the end position of the
    /// given span.
    #[must_use]
    fn join(&self, end: &Self) -> Self;
}

impl<ID: PartialEq + Clone> Join for Span<ID> {
    fn join(&self, end: &Self) -> Self {
        assert!(self.source_id == end.source_id, "source IDs are not equal");

        Self {
            start: self.start.clone(),
            end: end.end.clone(),
            source_id: self.source_id,
        }
    }
}

/// Represents a location used to locate a particular character in the source
/// file using the line and column numbers (0 indexed).
///
/// This struct is used by text editors or user-facing tools to display the
/// location of a character in the source file.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    StableHash,
)]
pub struct EditorLocation {
    /// The line number of the location (starts at 0).
    pub line: usize,

    /// The column number of the location (starts at 0).
    pub column: usize,
}

impl EditorLocation {
    /// Creates a new location with the given line and column numbers.
    #[must_use]
    pub const fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

impl<L> Span<L> {
    /// Creates a span from the given start and end byte indices in the source
    /// file.
    #[must_use]
    pub const fn new(start: L, end: L, source_id: GlobalSourceID) -> Self {
        Self { start, end, source_id }
    }

    /// Joins the starting position of this span with the end position of the
    /// given span.
    #[must_use]
    pub fn join(&self, end: &Self) -> Self
    where
        L: Clone,
    {
        assert!(self.source_id == end.source_id, "source IDs are not equal");

        Self {
            start: self.start.clone(),
            end: end.end.clone(),
            source_id: self.source_id,
        }
    }

    /// Gets the byte range of the span.
    #[must_use]
    pub fn range(&self) -> Range<L>
    where
        L: Clone,
    {
        self.start.clone()..self.end.clone()
    }

    /// Gets the byte range of the span, with an option to make the end
    #[allow(clippy::range_plus_one)]
    pub fn range_maybe_inclusive(&self, inclusive: bool) -> Range<L>
    where
        L: Clone + std::ops::Add<usize, Output = L>,
    {
        if inclusive {
            self.start.clone()..(self.end.clone() + 1)
        } else {
            self.start.clone()..self.end.clone()
        }
    }
}

/// Represents an element that is located within a source file.
pub trait SourceElement {
    /// The type of the span.
    type Location;

    /// Gets the span location of the element.
    fn span(&self) -> Span<Self::Location>;
}

impl<T: SourceElement> SourceElement for Box<T> {
    type Location = T::Location;

    fn span(&self) -> Span<Self::Location> { self.as_ref().span() }
}

/// A type that uniquely identifies a source file within a single target.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    StableHash,
    Encode,
    Decode,
    Identifiable,
)]
pub struct LocalSourceID {
    lo: u64,
    hi: u64,
}

impl LocalSourceID {
    /// Creates a new [`LocalSourceID`] from the given low and high parts.
    #[must_use]
    pub const fn new(lo: u64, hi: u64) -> Self { Self { lo, hi } }

    /// Returns the low 64 bits of the ID.
    #[must_use]
    pub const fn lo(&self) -> u64 { self.lo }

    /// Returns the high 64 bits of the ID.
    #[must_use]
    pub const fn hi(&self) -> u64 { self.hi }
}

/// A type alias for the [`Global`] type with a [`LocalSourceID`] as the inner
/// type, used for identifying source files across different targets.
pub type GlobalSourceID = Global<LocalSourceID>;

/// Query for loading source files content from the file system.
///
/// This query is im-pure and should be re-evaluated every time it's loaded from
/// the persistence layer to trigger the re-verification of the query that
/// depends on it.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    qbice::Query,
)]
#[value(Result<SourceFile, Error>)]
pub struct Key {
    /// The path to load the source file.
    pub path: Interned<Path>,

    /// The target that requested the source file loading
    pub target_id: TargetID,
}

/// The string formatted error from the [`std::io::Error`] when loading
/// the source file.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    thiserror::Error,
)]
#[error("{}", self.0.as_ref())]
pub struct Error(pub Interned<str>);

/// An error that occurs when calculating the source file path ID.
#[derive(Debug, thiserror::Error)]
pub enum CalculatePathError {
    /// Failed to canonicalize the path.
    #[error(transparent)]
    Io(#[from] std::io::Error),

    /// The given path is not in the target directory.
    #[error("the given path is not in the target directory")]
    NotInTargetDirectory,
}

/// A query for calculating the stable source file ID from the given path.
///
/// This query takes a path (which can be relative or absolute) and produces a
/// stable 128-bit ID that uniquely identifies the file. The ID is computed by:
///
/// 1. Canonicalizing the path to resolve:
///    - Relative path components (`.` and `..`)
///    - Symbolic links
///    - Converting to absolute path
/// 2. Hashing the canonicalized path using `SipHash-2-4` with a fixed seed to
///    produce a 128-bit hash
///
/// This ensures that the same file, whether accessed via relative or absolute
/// path, will always produce the same stable ID. The file content does NOT
/// contribute to the ID—only the path.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Result<LocalSourceID, Interned<str>>)]
#[extend(name = get_stable_path_id, by_val)]
pub struct StablePathIDKey {
    /// The path to calculate the stable ID for.
    ///
    /// This can be either a relative or absolute path. The path will be
    /// canonicalized before computing the stable ID.
    pub path: Interned<Path>,

    /// The target id that is requesting the stable path ID calculation.
    pub target_id: TargetID,
}

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
    Encode,
    Decode,
    StableHash,
    qbice::Query,
)]
#[value(Interned<Path>)]
#[extend(name = get_source_file_path, by_val)]
pub struct FilePathKey {
    /// The ID of the source file.
    pub id: GlobalSourceID,
}

/// Obtains the [`SourceFile`] by its associated ID.
#[pernixc_extend::extend]
pub async fn get_source_file_by_id(
    self: &TrackedEngine,
    id: GlobalSourceID,
) -> SourceFile {
    let source_path = self.get_source_file_path(id).await;
    let target_id = id.target_id;

    self.query(&Key { path: source_path, target_id }).await.unwrap()
}

#[cfg(test)]
mod test;
