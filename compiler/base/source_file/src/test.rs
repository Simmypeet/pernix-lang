use std::path::PathBuf;

use qbice::storage::intern::Interned;

use super::{EditorLocation, SourceFile};

#[test]
fn replace_range_as_insert() {
    let mut source_file = SourceFile::from_str(
        "ABCD\nEFGH\nIJKL",
        Interned::new_duplicating_unsized(PathBuf::from("test")),
    );

    source_file.replace_range(5..5, "1234");

    assert_eq!(source_file.content(), "ABCD\n1234EFGH\nIJKL");

    assert_eq!(source_file.line_coount(), 3);

    assert_eq!(source_file.get_line(0).unwrap(), "ABCD\n");
    assert_eq!(source_file.get_line(1).unwrap(), "1234EFGH\n");
    assert_eq!(source_file.get_line(2).unwrap(), "IJKL");
}

#[test]
fn replace_range_in_same_line_one_line() {
    let mut source_file = SourceFile::from_str(
        "ABCD\nEFGH\nIJKL",
        Interned::new_duplicating_unsized(PathBuf::from("test")),
    );

    source_file.replace_range(6..8, "1234");

    assert_eq!(source_file.content(), "ABCD\nE1234H\nIJKL");

    assert_eq!(source_file.line_coount(), 3);

    assert_eq!(source_file.get_line(0).unwrap(), "ABCD\n");
    assert_eq!(source_file.get_line(1).unwrap(), "E1234H\n");
    assert_eq!(source_file.get_line(2).unwrap(), "IJKL");

    source_file.replace_range(6..10, "FG");

    assert_eq!(source_file.content(), "ABCD\nEFGH\nIJKL");

    assert_eq!(source_file.line_coount(), 3);

    assert_eq!(source_file.get_line(0).unwrap(), "ABCD\n");
    assert_eq!(source_file.get_line(1).unwrap(), "EFGH\n");
    assert_eq!(source_file.get_line(2).unwrap(), "IJKL");
}

#[test]
fn replace_range_multi_line_one_line() {
    let mut source_file = SourceFile::from_str(
        "ABCD\nEFGH\nIJKL\nMNOP",
        Interned::new_duplicating_unsized(PathBuf::from("test")),
    );

    source_file.replace_range(5..11, "1234");

    assert_eq!(source_file.content(), "ABCD\n1234JKL\nMNOP");

    assert_eq!(source_file.line_coount(), 3);

    assert_eq!(source_file.get_line(0).unwrap(), "ABCD\n");
    assert_eq!(source_file.get_line(1).unwrap(), "1234JKL\n");
    assert_eq!(source_file.get_line(2).unwrap(), "MNOP");
}

#[test]
fn replace_range_multi_line() {
    let mut source_file = SourceFile::from_str(
        "ABC\nDEF\nGHI\nJ",
        Interned::new_duplicating_unsized(PathBuf::from("test")),
    );

    source_file.replace_range(5..6, "12\n3\n4");

    assert_eq!(source_file.line_coount(), 6);
    assert_eq!(source_file.content(), "ABC\nD12\n3\n4F\nGHI\nJ");

    assert_eq!(source_file.get_line(0).unwrap(), "ABC\n");
    assert_eq!(source_file.get_line(1).unwrap(), "D12\n");
    assert_eq!(source_file.get_line(2).unwrap(), "3\n");
    assert_eq!(source_file.get_line(3).unwrap(), "4F\n");
    assert_eq!(source_file.get_line(4).unwrap(), "GHI\n");
    assert_eq!(source_file.get_line(5).unwrap(), "J");
}

#[test]
fn replace_trimming() {
    let mut source_file = SourceFile::from_str(
        "ABC\nDEF\nGHI\nJ",
        Interned::new_duplicating_unsized(PathBuf::from("test")),
    );

    source_file.replace_range(1..10, "1\n2");

    assert_eq!(source_file.content(), "A1\n2I\nJ");

    assert_eq!(source_file.line_coount(), 3);

    assert_eq!(source_file.get_line(0).unwrap(), "A1\n");
    assert_eq!(source_file.get_line(1).unwrap(), "2I\n");
    assert_eq!(source_file.get_line(2).unwrap(), "J");
}

#[test]
fn replace_add_line() {
    let mut source_file = SourceFile::from_str(
        "ABC\nDEF\nGHI\nJ",
        Interned::new_duplicating_unsized(PathBuf::from("test")),
    );

    source_file.replace_range(5..5, "\n");

    assert_eq!(source_file.line_coount(), 5);

    assert_eq!(source_file.content(), "ABC\nD\nEF\nGHI\nJ");

    assert_eq!(source_file.get_line(0).unwrap(), "ABC\n");
    assert_eq!(source_file.get_line(1).unwrap(), "D\n");
    assert_eq!(source_file.get_line(2).unwrap(), "EF\n");
    assert_eq!(source_file.get_line(3).unwrap(), "GHI\n");
    assert_eq!(source_file.get_line(4).unwrap(), "J");
}

#[test]
fn replace_range_as_append() {
    let mut source_file = SourceFile::from_str(
        "ABC",
        Interned::new_duplicating_unsized(PathBuf::from("test")),
    );

    let position = EditorLocation::new(0, 3);
    let byte_index =
        source_file.get_byte_index_from_editor_location(&position).unwrap();

    source_file.replace_range(byte_index..byte_index, "D");

    assert_eq!(source_file.content(), "ABCD");

    assert_eq!(source_file.line_coount(), 1);
}

#[test]
fn replace_range_delete() {
    let mut source_file = SourceFile::from_str(
        "AAA\nBBB\nCCC",
        Interned::new_duplicating_unsized(PathBuf::from("test")),
    );

    let start = EditorLocation::new(0, 1);
    let end = EditorLocation::new(2, 2);

    let start_byte_index =
        source_file.get_byte_index_from_editor_location(&start).unwrap();
    let end_byte_index =
        source_file.get_byte_index_from_editor_location(&end).unwrap();

    source_file.replace_range(start_byte_index..end_byte_index, "");

    assert_eq!(source_file.content(), "AC");
}
