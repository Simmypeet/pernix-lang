//! Contains the code related to the source code input.

use core::str;
use std::{
    cmp::Ordering,
    fmt::Debug,
    fs::File,
    hash::Hash,
    io::Read,
    ops::{Not, Range},
    path::{Path, PathBuf},
    sync::Arc,
};

use dashmap::{
    DashMap,
    mapref::one::{MappedRef, Ref, RefMut},
};
use getset::{CopyGetters, Getters};
use pernixc_extend::extend;
use pernixc_qbice::TrackedEngine;
use pernixc_target::{
    Global, TargetID, get_invocation_arguments, get_target_seed,
};
use qbice::{
    Decode, Encode, Identifiable, StableHash,
    stable_hash::{StableHasher, Value},
    storage::intern::Interned,
};
use rayon::{iter::ParallelIterator, slice::ParallelSlice};
use siphasher::{
    sip::SipHasher24,
    sip128::{self, Hasher128},
};

/// Represents an source file input for the compiler.
#[derive(Clone, PartialEq, Eq, Hash, Getters, Encode, Decode)]
pub struct SourceFile {
    content: String,

    /// Gets the full path to the source file.
    #[get = "pub"]
    path: PathBuf,

    /// The byte ranges for each line in the source file (including the
    /// newline)
    lines: Vec<Range<usize>>,
}

/// Parallel hash function for the content of the source file.
fn file_content_hash<H: StableHasher + ?Sized>(
    content: &str,
    hasher: &mut H,
) -> H::Hash {
    content
        .as_bytes()
        .par_chunks(1024)
        .map(|chunk| {
            hasher.sub_hash(&mut |x| {
                chunk.stable_hash(x);
            })
        })
        .reduce(H::Hash::default, H::Hash::wrapping_add)
}

impl StableHash for SourceFile {
    fn stable_hash<H: StableHasher + ?Sized>(&self, state: &mut H) {
        let hash = file_content_hash(self.content(), state);
        hash.stable_hash(state);

        self.path.stable_hash(state);
    }
}

impl AsRef<str> for SourceFile {
    fn as_ref(&self) -> &str { &self.content }
}

impl PartialOrd for SourceFile {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SourceFile {
    fn cmp(&self, other: &Self) -> Ordering {
        self.path.cmp(&other.path).then(self.content.cmp(&other.content))
    }
}

#[allow(clippy::missing_fields_in_debug)]
impl Debug for SourceFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SourceFile")
            .field("full_path", &self.path)
            .field("lines", &self.lines)
            .finish()
    }
}

impl SourceFile {
    /// Creates a new inline source file
    #[must_use]
    pub fn new(content: String, full_path: PathBuf) -> Self {
        let lines = get_line_byte_positions(&content);
        Self { content, path: full_path, lines }
    }

    /// Gets the content of the source file.
    #[must_use]
    #[allow(clippy::missing_const_for_fn)]
    pub fn content(&self) -> &str { &self.content }

    /// Returns ranges of byte indices for each line in the source file.
    #[must_use]
    pub fn lines(&self) -> &[Range<usize>] { &self.lines }

    /// Gets the content of the source file as a string.
    #[must_use]
    pub const fn content_string(&self) -> &String { &self.content }

    /// Translates a location to a byte index.
    #[must_use]
    pub fn into_byte_index(
        &self,
        location: EditorLocation,
    ) -> Option<ByteIndex> {
        let start = self.lines.get(location.line)?.start;
        let line = self.get_line(location.line)?;

        line.char_indices()
            .take(location.column + 1)
            .last()
            .map(|x| x.0 + start)
    }

    /// Translates a location to a byte index. This includes the ending byte.
    #[must_use]
    pub fn into_byte_index_include_ending(
        &self,
        location: EditorLocation,
    ) -> Option<ByteIndex> {
        let range = self.lines.get(location.line)?;
        let line = self.get_line(location.line)?;

        let mut index = 0;

        for (byte_idx, _) in line.char_indices() {
            if location.column == index {
                return Some(byte_idx + range.start);
            }

            index += 1;
        }

        if index == location.column { Some(range.end) } else { None }
    }

    /// Replaces the content of the source file in the given range with the
    /// given string.
    #[allow(
        clippy::cast_possible_wrap,
        clippy::cast_sign_loss,
        clippy::range_plus_one,
        clippy::missing_panics_doc
    )]
    pub fn replace_range(&mut self, range: Range<ByteIndex>, string: &str) {
        assert!(
            range.start <= self.content.len(),
            "out of bound start index {}, content length {}",
            range.start,
            self.content.len()
        );
        assert!(
            range.end <= self.content.len(),
            "out of bound end index {}, content length {}",
            range.end,
            self.content.len()
        );
        assert!(
            range.start <= range.end,
            "start index {} is greater than end index {}",
            range.start,
            range.end
        );

        // just append the string
        if range.start == range.end && range.start == self.content.len() {
            let prior_len = self.content.len();
            self.content.push_str(string);

            if !string.contains('\n') && !string.contains('\r') {
                self.lines.last_mut().unwrap().end += string.len();
            } else {
                let mut new_line_changes = get_line_byte_positions(string);

                for new_range in &mut new_line_changes {
                    new_range.start += prior_len;
                    new_range.end += prior_len;
                }

                let last_line = self.lines.last_mut().unwrap();

                last_line.end = new_line_changes[0].end;

                self.lines.extend(new_line_changes.into_iter().skip(1));
            }

            return;
        }

        assert!(
            self.content.is_char_boundary(range.start),
            "start index {} is not a char boundary",
            range.start
        );

        assert!(
            self.content.is_char_boundary(range.end),
            "end index {} is not a char boundary",
            range.end
        );

        self.content.replace_range(range.clone(), string);

        // update the line ranges
        let start_line = self.get_line_of_byte_index(range.start).unwrap();
        let end_line = self
            .get_line_of_byte_index(range.end)
            .unwrap_or_else(|| self.lines.len() - 1);

        let character_difference =
            string.len() as isize - (range.end - range.start) as isize;

        // no new lines, we can skip the line calculations for new string
        if !string.contains('\n') && !string.contains('\r') {
            let removing_lines = start_line + 1..=end_line;

            self.lines[start_line].end = (self.lines[end_line].end as isize
                + character_difference)
                as usize;

            self.lines.drain(removing_lines);

            for i in start_line + 1..self.lines.len() {
                self.lines[i].start = (self.lines[i].start as isize
                    + character_difference)
                    as usize;
                self.lines[i].end = (self.lines[i].end as isize
                    + character_difference)
                    as usize;
            }
        } else {
            let mut new_line_ranges = get_line_byte_positions(string);
            for new_range in &mut new_line_ranges {
                new_range.start += range.start;
                new_range.end += range.start;
            }
            let end_after_count = self.lines[end_line].end - range.end;
            self.lines[start_line].end = new_line_ranges[0].end;

            let removing_lines = start_line + 1..=end_line;
            self.lines.drain(removing_lines);

            self.lines.splice(
                (start_line + 1)..=start_line,
                (1..(new_line_ranges.len() - 1))
                    .map(|x| new_line_ranges[x].clone()),
            );

            self.lines.insert(
                start_line + new_line_ranges.len() - 1,
                new_line_ranges.last().unwrap().start
                    ..new_line_ranges.last().unwrap().end + end_after_count,
            );

            for i in (start_line + new_line_ranges.len())..self.lines.len() {
                self.lines[i].start = (self.lines[i].start as isize
                    + character_difference)
                    as usize;
                self.lines[i].end = (self.lines[i].end as isize
                    + character_difference)
                    as usize;
            }
        }
    }

    /// Determines in which line number the given byte index is located (0
    /// indexed).
    #[must_use]
    pub fn get_line_of_byte_index(
        &self,
        byte_index: ByteIndex,
    ) -> Option<usize> {
        // gets the line number by binary searching the line ranges
        self.lines
            .binary_search_by(|range| {
                if range.contains(&byte_index) {
                    Ordering::Equal
                } else if byte_index < range.start {
                    Ordering::Greater
                } else {
                    Ordering::Less
                }
            })
            .ok()
    }

    /// Gets the starting byte index of the given line number (0 indexed).
    #[must_use]
    pub fn get_starting_byte_index_of_line(
        &self,
        line: usize,
    ) -> Option<ByteIndex> {
        self.lines.get(line).map(|range| range.start)
    }

    /// Gets the line of the source file at the given line number.
    ///
    /// The line number starts at 0.
    #[must_use]
    pub fn get_line(&self, line: usize) -> Option<&str> {
        self.lines.get(line).map(|range| &self.content[range.clone()])
    }

    /// Replaces the content of the source file with the given content.
    pub fn replace(&mut self, content: String) {
        self.lines = get_line_byte_positions(&content);
        self.content = content;
    }

    /// Gets the number of lines in the source file.
    #[must_use]
    pub const fn line_coount(&self) -> usize { self.lines.len() }

    /// Loads the source file from the given file path.
    ///
    /// # Errors
    /// - [`Error::Io`]: Error occurred when mapping the file to memory.
    /// - [`Error::Utf8`]: Error occurred when converting the mapped bytes to a
    ///   string.
    pub fn load(mut file: File, path: PathBuf) -> Result<Self, std::io::Error> {
        let mut string = String::new();
        file.read_to_string(&mut string)?;

        Ok(Self::new(string, path))
    }

    /// Gets the byte index from the given editor location.
    #[must_use]
    pub fn get_byte_index_from_editor_location(
        &self,
        editor_location: &EditorLocation,
    ) -> ByteIndex {
        let line_starting_byte =
            self.get_starting_byte_index_of_line(editor_location.line).unwrap();

        let line_str = self.get_line(editor_location.line).unwrap();

        let mut byte_index = line_starting_byte;
        for (i, ch) in line_str.char_indices() {
            if i >= editor_location.column {
                break;
            }
            byte_index += ch.len_utf8();
        }

        byte_index
    }

    /// Gets the [`Location`] of the given byte index.
    #[must_use]
    #[allow(clippy::missing_panics_doc)]
    pub fn get_location(
        &self,
        byte_index: ByteIndex,
    ) -> Option<EditorLocation> {
        // pointing at the end of the file
        if byte_index == self.content.len() {
            return Some(EditorLocation {
                line: if self.lines.is_empty() {
                    0
                } else {
                    self.lines.len() - 1
                },
                column: if self.lines.is_empty() {
                    0
                } else {
                    let last_line_range = &self.lines[self.lines.len() - 1];
                    let last_line_str = &self.content[last_line_range.clone()];

                    last_line_str.chars().count()
                },
            });
        }

        if !self.content.is_char_boundary(byte_index) {
            return None;
        }

        // gets the line number by binary searching the line ranges
        let line = self
            .lines
            .binary_search_by(|range| {
                if range.contains(&byte_index) {
                    Ordering::Equal
                } else if byte_index < range.start {
                    Ordering::Greater
                } else {
                    Ordering::Less
                }
            })
            .ok()?;

        let line_starting_byte_index = self.lines[line].start;
        let line_str = self.get_line(line).unwrap();

        // gets the column number by iterating through the utf-8 characters
        let column = line_str
            .char_indices()
            .take_while(|(i, _)| *i + line_starting_byte_index < byte_index)
            .count();

        Some(EditorLocation { line, column })
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

fn get_line_byte_positions(text: &str) -> Vec<Range<usize>> {
    let mut current_position = 0;
    let mut results = Vec::new();

    let mut skip = false;

    for (byte, char) in text.char_indices() {
        if skip {
            skip = false;
            continue;
        }

        // ordinary lf
        if char == '\n' {
            #[allow(clippy::range_plus_one)]
            // skipcq: RS-W1003
            results.push(current_position..byte + 1);

            current_position = byte + 1;
        }

        // crlf
        if char == '\r' {
            if text.as_bytes().get(byte + 1) == Some(&b'\n') {
                #[allow(clippy::range_plus_one)]
                results.push(current_position..byte + 2);

                current_position = byte + 2;

                skip = true;
            } else {
                #[allow(clippy::range_plus_one)]
                // skipcq: RS-W1003
                results.push(current_position..byte + 1);

                current_position = byte + 1;
            }
        }
    }

    results.push(current_position..text.len());

    results
}

/// A map of source files, accessing through the [`GlobalSourceID`].
#[derive(Debug, Clone, Default, Encode, Decode)]
pub struct SourceMap {
    source_files_by_id: DashMap<GlobalSourceID, SourceFile>,
}

impl SourceMap {
    /// Creates a new empty [`SourceMap`].
    #[must_use]
    pub fn new() -> Self { Self { source_files_by_id: DashMap::new() } }

    /// Registers a source file in the map. If the source file is already
    /// registered, it returns the `Err(ID)` of the existing source file.
    /// If the source file is not registered, it returns the `Ok(ID)` of the
    /// newly registered source file.
    #[must_use]
    pub fn register(
        &self,
        target_id: TargetID,
        source: SourceFile,
    ) -> LocalSourceID {
        let patb = &source.path;
        let mut hasher = siphasher::sip128::SipHasher24::default();
        patb.hash(&mut hasher);

        let finalize_hash = |hasher: siphasher::sip128::SipHasher24| {
            let mut attempt = 0;
            loop {
                let mut final_hasher = hasher;
                attempt.hash(&mut final_hasher);

                let hash128 = final_hasher.finish128();
                let lo = hash128.h1;
                let hi = hash128.h2;

                let candidate_branch_id = LocalSourceID { lo, hi };

                // avoid hash collision
                if !self
                    .source_files_by_id
                    .contains_key(&target_id.make_global(candidate_branch_id))
                {
                    return candidate_branch_id;
                }

                attempt += 1;
            }
        };

        let hash = finalize_hash(hasher);

        // insert the source file into the map
        assert!(
            self.source_files_by_id
                .insert(target_id.make_global(hash), source)
                .is_none()
        );

        hash
    }

    /// Gets the source file by its ID.
    #[must_use]
    pub fn get(
        &self,
        id: GlobalSourceID,
    ) -> Option<Ref<'_, GlobalSourceID, SourceFile>> {
        self.source_files_by_id.get(&id)
    }

    /// Gets the source file by its ID.
    #[must_use]
    pub fn get_mut(
        &self,
        id: GlobalSourceID,
    ) -> Option<RefMut<'_, GlobalSourceID, SourceFile>> {
        self.source_files_by_id.get_mut(&id)
    }
}

/// A wrapper around [`MappedRef`] that implements [`std::fmt::Display`]
/// allowing it to be used as a [`codespan_reporting::files::Name`].
#[derive(Debug)]
pub struct DisplayPathBuf<'a>(
    pub MappedRef<'a, GlobalSourceID, SourceFile, PathBuf>,
);

impl std::fmt::Display for DisplayPathBuf<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0.display(), f)
    }
}

impl<'a> codespan_reporting::files::Files<'a> for SourceMap {
    type FileId = GlobalSourceID;
    type Name = DisplayPathBuf<'a>;
    type Source = MappedRef<'a, GlobalSourceID, SourceFile, String>;

    fn name(
        &'a self,
        id: Self::FileId,
    ) -> Result<Self::Name, codespan_reporting::files::Error> {
        let source = self
            .source_files_by_id
            .get(&id)
            .ok_or(codespan_reporting::files::Error::FileMissing)?;

        Ok(DisplayPathBuf(source.map(|x| x.path())))
    }

    fn source(
        &'a self,
        id: Self::FileId,
    ) -> Result<Self::Source, codespan_reporting::files::Error> {
        let source = self
            .source_files_by_id
            .get(&id)
            .ok_or(codespan_reporting::files::Error::FileMissing)?;

        Ok(source.map(|x| x.content_string()))
    }

    fn line_index(
        &'a self,
        id: Self::FileId,
        byte_index: usize,
    ) -> Result<usize, codespan_reporting::files::Error> {
        let source = self
            .source_files_by_id
            .get(&id)
            .ok_or(codespan_reporting::files::Error::FileMissing)?;

        if byte_index == source.content().len() {
            return Ok(if source.lines.is_empty() {
                0
            } else {
                source.lines.len() - 1
            });
        }

        let line = source.get_line_of_byte_index(byte_index).ok_or(
            codespan_reporting::files::Error::IndexTooLarge {
                given: byte_index,
                max: source.content().len(),
            },
        )?;

        Ok(line)
    }

    fn line_range(
        &'a self,
        id: Self::FileId,
        line_index: usize,
    ) -> Result<Range<usize>, codespan_reporting::files::Error> {
        let source = self
            .source_files_by_id
            .get(&id)
            .ok_or(codespan_reporting::files::Error::FileMissing)?;

        if line_index == source.lines.len() {
            return Ok(source.lines.last().map_or(0..0, |x| x.start..x.end));
        }

        let line_range = source.lines.get(line_index).ok_or({
            codespan_reporting::files::Error::IndexTooLarge {
                given: line_index,
                max: source.lines.len(),
            }
        })?;

        Ok(line_range.clone())
    }
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
#[value(Result<Arc<SourceFile>, Error>)]
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

/// Calculates the ID of the source file based on their path.
///
/// The path will be canonicalized before calculating the ID. Therefore,
/// two different path strings that point to the same file will ultimately
/// yield the same source file ID.
///
/// Internally this uses a hash function to derive a stable ID for the source
/// file.
#[extend]
pub async fn calculate_path_id(
    self: &TrackedEngine,
    path: &Path,
    target_id: TargetID,
) -> Result<LocalSourceID, std::io::Error> {
    let hasher = SipHasher24::default();

    let target_args = self.get_invocation_arguments(target_id).await;

    if path.is_absolute().not() {
        return Err(std::io::Error::other("the given path is not absolute"));
    }

    if target_args.command.input().file.is_absolute().not() {
        return Err(std::io::Error::other(
            "the target root file path is not absolute",
        ));
    }

    let target_directory = target_args
        .command
        .input()
        .file
        .parent()
        .unwrap_or_else(|| std::path::Path::new(""));

    // skip the prefix components to avoid absolute path issues (e.g. \\?\ on
    // Windows)
    let this_components = path.components().skip(1).collect::<Vec<_>>();
    let target_components =
        target_directory.components().skip(1).collect::<Vec<_>>();

    if target_components.len() > this_components.len() {
        return Err(std::io::Error::other(
            "the given path is not in the target directory",
        ));
    }

    for (a, b) in target_components.iter().zip(this_components.iter()) {
        if a != b {
            return Err(std::io::Error::other(
                "the given path is not in the target directory",
            ));
        }
    }

    let mut hasher = sip128::SipHasher24::default();

    // hash only the relative path from the target directory
    for component in this_components.iter().skip(target_components.len()) {
        component.as_os_str().hash(&mut hasher);
    }

    let target_seed = self.get_target_seed(target_id).await;
    target_seed.hash(&mut hasher);

    let hash128 = hasher.finish128();

    let lo = hash128.h1;
    let hi = hash128.h2;

    Ok(LocalSourceID { lo, hi })
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

/// Obtains the [`Arc<SourceFile>`] by its associated ID.
#[pernixc_extend::extend]
pub async fn get_source_file_by_id(
    self: &TrackedEngine,
    id: GlobalSourceID,
) -> Arc<SourceFile> {
    let source_path = self.get_source_file_path(id).await;
    let target_id = id.target_id;

    self.query(&Key { path: source_path, target_id }).await.unwrap()
}

#[cfg(test)]
mod test;
