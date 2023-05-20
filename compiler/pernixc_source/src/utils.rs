//! Contains utilities for testing purposes.

use std::{fs::File, io::Write, sync::Arc};

use derive_more::{Deref, DerefMut};
use getset::Getters;
use tempfile::TempDir;

use crate::{LoadError, SourceFile};

/// Is a temporary [`SourceFile`] that is created temporarily for testing purposes.
#[derive(Debug, Getters, Deref, DerefMut)]
pub struct TempSourceFile {
    _temp_dir: TempDir,
    _temp_file: File,

    #[deref]
    #[deref_mut]
    source_file: Arc<SourceFile>,
}

impl TempSourceFile {
    /// Creates a new [`TempSourceFile`] from the given source.
    ///
    /// # Errors
    /// If the source cannot be written to the temporary file.
    pub fn new(source: &str) -> Result<Self, LoadError> {
        const TEMP_FILE_NAME: &str = "temp";

        let temp_dir = tempfile::tempdir()?;
        let temp_file_path = temp_dir.path().join(format!("{TEMP_FILE_NAME}.pnx"));
        let mut temp_file = std::fs::File::create(temp_file_path.clone())?;

        write!(&mut temp_file, "{source}")?;

        Ok(Self {
            _temp_dir: temp_dir,
            _temp_file: temp_file,
            source_file: SourceFile::load(&temp_file_path, vec!["temp".to_owned()])?,
        })
    }
}
