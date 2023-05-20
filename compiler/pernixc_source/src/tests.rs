use std::path::PathBuf;

use proptest::prelude::*;

use super::SourceFile;
use crate::Location;

fn line_string_strategy() -> impl Strategy<Value = String> {
    proptest::collection::vec(
        proptest::char::any().prop_filter("only non-control character", |x| !x.is_control()),
        0..=30,
    )
    .prop_map(|vec| vec.into_iter().collect::<String>())
}

fn lines_strategy() -> impl Strategy<Value = Vec<String>> {
    proptest::collection::vec(line_string_strategy(), 1..=100)
}

proptest! {
    #[test]
    fn line_test(lines in lines_strategy()) {
        let source = lines.join(SourceFile::NEW_LINE_STR);
        let source_file = SourceFile::new(
            PathBuf::new(),
            "test".to_string(),
            source,
            vec!["test".to_string()]
        )?;

        // line number check
        prop_assert_eq!(source_file.line_number(), lines.len());

        // line content check
        for (i, line) in lines.iter().enumerate() {

            if i < lines.len() - 1 {
                prop_assert_eq!(source_file.get_line(i + 1).unwrap(), line.clone() + SourceFile::NEW_LINE_STR);
            } else {
                prop_assert_eq!(source_file.get_line(i + 1).unwrap(), line);
            }
        }
    }

    #[test]
    fn get_location_test(lines in lines_strategy()) {
        let source = lines.join(SourceFile::NEW_LINE_STR);
        let source_file = SourceFile::new(
            PathBuf::new(),
            "test".to_string(),
            source,
            vec!["test".to_string()]
        )?;

        let mut line = 1;
        let mut column = 1;

        // iterator check
        for (byte_index, c) in source_file.iter() {
            prop_assert_eq!(
                source_file.get_location(byte_index).unwrap(),
                Location { line, column }
            );

            // update line and column
            if c == SourceFile::NEW_LINE {
                line += 1;
                column = 1;
            } else {
                column += 1;
            }
        }
    }
}
