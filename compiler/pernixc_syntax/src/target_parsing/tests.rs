use std::{error::Error, path::PathBuf};

use pernixc_source::SourceFile;
use pernixc_system::error_handler::ErrorVec;

use crate::target_parsing::{AllParsingError, TargetParsing};

#[test]
fn module_branching_test() -> Result<(), Box<dyn Error>> {
    let source_file = SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("resource")
            .join("moduleBranchingTest")
            .join("main.pnx"),
        vec!["test".to_string()],
    )?;

    let error_vec: ErrorVec<AllParsingError> = ErrorVec::default();
    let syntax_trees = TargetParsing::parse(source_file, &error_vec)?;

    assert!(error_vec.into_vec().is_empty());
    assert_eq!(syntax_trees.len(), 4);

    // test
    assert!(syntax_trees.iter().any(|syntax_tree| {
        *syntax_tree.source_file.module_hierarchy() == vec!["test"]
            && *syntax_tree.source_file.parent_directory()
                == PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                    .join("resource")
                    .join("moduleBranchingTest")
    }));

    // test::sub
    assert!(syntax_trees.iter().any(|syntax_tree| {
        *syntax_tree.source_file.module_hierarchy() == vec!["test", "sub"]
            && *syntax_tree.source_file.parent_directory()
                == PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                    .join("resource")
                    .join("moduleBranchingTest")
    }));

    // test::sub::innerSub1
    assert!(syntax_trees.iter().any(|syntax_tree| {
        *syntax_tree.source_file.module_hierarchy() == vec!["test", "sub", "innerSub1"]
            && *syntax_tree.source_file.parent_directory()
                == PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                    .join("resource")
                    .join("moduleBranchingTest")
                    .join("sub")
    }));

    // test::sub::innerSub2
    assert!(syntax_trees.iter().any(|syntax_tree| {
        *syntax_tree.source_file.module_hierarchy() == vec!["test", "sub", "innerSub2"]
            && *syntax_tree.source_file.parent_directory()
                == PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                    .join("resource")
                    .join("moduleBranchingTest")
                    .join("sub")
    }));

    Ok(())
}
