use std::sync::Arc;

use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_base::{diagnostic::Storage, source_file::SourceFile};
use pernixc_tests::input::Input;
use proptest::{
    prelude::{Arbitrary, TestCaseError},
    proptest,
};

use crate::syntax_tree::{
    self,
    target::{strategy::ModuleTree, Target},
};

proptest! {
    #[test]
    #[allow(clippy::redundant_closure_for_method_calls, clippy::ignored_unit_patterns)]
    fn target(
        mut target_module_tree in ModuleTree::arbitrary()
    ) {
        target_module_tree.signature = None;

        let target_dir = target_module_tree.create_target()?;
        let file = std::fs::File::open(target_dir.path().join("main.pnx"))?;

        let root_source_file = Arc::new(
            SourceFile::load(file, target_dir.path().join("main.pnx"))?
        );
        let storage = Storage::<Error>::new();
        let target = Target::parse(&root_source_file, "test".to_string(), &storage);

        if !storage.as_vec().is_empty() {
            return Err(TestCaseError::fail(format!("parsing error: {:#?}",storage.as_vec())));
        }

        target_module_tree.assert(target.module_tree())?;
    }
}

#[derive(Debug, EnumAsInner, From)]
#[allow(dead_code)]
enum Error {
    Lexical(pernixc_lexical::error::Error),
    Syntax(crate::error::Error),
    Target(syntax_tree::target::Error),
}