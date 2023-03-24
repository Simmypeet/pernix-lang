use std::{error::Error, path::PathBuf};

use super::SourceFile;

#[test]
fn source_file_line_test() -> Result<(), Box<dyn Error>> {
    let source_file = SourceFile::load(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("resource")
            .join("sourceFileTest.pnx"),
        vec!["test".to_string()],
    )?;

    assert!(source_file.get_line(0).is_none());
    assert_eq!(source_file.get_line(1).unwrap(), "Hello\n");
    assert_eq!(source_file.get_line(2).unwrap(), "World");
    assert!(source_file.get_line(3).is_none());

    Ok(())
}

#[test]
fn source_file_iterator_test() -> Result<(), Box<dyn Error>> {
    let source_file = SourceFile::load(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("resource")
            .join("sourceFileTest.pnx"),
        vec!["test".to_string()],
    )?;

    let mut iter = source_file.iter();

    assert!({
        let (location, c) = iter.next().unwrap();
        c == 'H' && location.line == 1 && location.column == 1 && location.byte == 0
    });
    assert!({
        let (location, c) = iter.next().unwrap();
        c == 'e' && location.line == 1 && location.column == 2 && location.byte == 1
    });
    assert!({
        let (location, c) = iter.next().unwrap();
        c == 'l' && location.line == 1 && location.column == 3 && location.byte == 2
    });
    assert!({
        let (location, c) = iter.next().unwrap();
        c == 'l' && location.line == 1 && location.column == 4 && location.byte == 3
    });
    assert!({
        let (location, c) = iter.next().unwrap();
        c == 'o' && location.line == 1 && location.column == 5 && location.byte == 4
    });
    assert!({
        let (location, c) = iter.next().unwrap();
        c == '\n' && location.line == 1 && location.column == 6 && location.byte == 5
    });
    assert!({
        let (location, c) = iter.next().unwrap();
        c == 'W' && location.line == 2 && location.column == 1 && location.byte == 6
    });
    assert!({
        let (location, c) = iter.next().unwrap();
        c == 'o' && location.line == 2 && location.column == 2 && location.byte == 7
    });
    assert!({
        let (location, c) = iter.next().unwrap();
        c == 'r' && location.line == 2 && location.column == 3 && location.byte == 8
    });
    assert!({
        let (location, c) = iter.next().unwrap();
        c == 'l' && location.line == 2 && location.column == 4 && location.byte == 9
    });
    assert!({
        let (location, c) = iter.next().unwrap();
        c == 'd' && location.line == 2 && location.column == 5 && location.byte == 10
    });
    assert!(iter.next().is_none());

    Ok(())
}
