use std::{error::Error, path::PathBuf};

use pernixc_common::source_file::SourceFile;

#[test]
fn module_branching_test() -> Result<(), Box<dyn Error>> {
    let source_file = SourceFile::load(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("resource")
            .join("moduleBranchingTest")
            .join("main.pnx"),
        vec!["test".to_string()],
    )?;

    let syntax_trees = super::parse_files(source_file)?;

    assert_eq!(syntax_trees.len(), 4);

    // test
    assert!(syntax_trees.iter().any(|syntax_tree| {
        *syntax_tree.source_file.module_heirarchy() == vec!["test"]
            && *syntax_tree.source_file.full_path()
                == PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                    .join("resource")
                    .join("moduleBranchingTest")
                    .join("main.pnx")
    }));

    // test::sub
    assert!(syntax_trees.iter().any(|syntax_tree| {
        *syntax_tree.source_file.module_heirarchy() == vec!["test", "sub"]
            && *syntax_tree.source_file.full_path()
                == PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                    .join("resource")
                    .join("moduleBranchingTest")
                    .join("sub.pnx")
    }));

    // test::sub::innerSub1
    assert!(syntax_trees.iter().any(|syntax_tree| {
        *syntax_tree.source_file.module_heirarchy() == vec!["test", "sub", "innerSub1"]
            && *syntax_tree.source_file.full_path()
                == PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                    .join("resource")
                    .join("moduleBranchingTest")
                    .join("sub")
                    .join("innerSub1.pnx")
    }));

    // test::sub::innerSub2
    assert!(syntax_trees.iter().any(|syntax_tree| {
        *syntax_tree.source_file.module_heirarchy() == vec!["test", "sub", "innerSub2"]
            && *syntax_tree.source_file.full_path()
                == PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                    .join("resource")
                    .join("moduleBranchingTest")
                    .join("sub")
                    .join("innerSub2.pnx")
    }));

    Ok(())
}
