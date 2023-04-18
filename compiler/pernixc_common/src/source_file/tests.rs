use std::{error::Error, path::PathBuf};

use super::SourceFile;
use crate::source_file::Location;

#[test]
fn location_test() -> Result<(), Box<dyn Error>> {
    let source_file = SourceFile::new(
        PathBuf::default(),
        "test".to_string(),
        "first\nsecond\nthird".to_string(),
        vec!["test".to_string()],
    )?;

    assert_eq!(
        source_file.get_location(0),
        Some(Location { line: 1, column: 1 })
    );

    assert_eq!(
        source_file.get_location(6),
        Some(Location { line: 2, column: 1 })
    );

    assert_eq!(source_file.get_location(18), None);

    Ok(())
}
