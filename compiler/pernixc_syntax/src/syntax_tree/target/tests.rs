use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_source::SourceFile;
use pernixc_system::{diagnostic::Storage, input::Input};
use proptest::{prelude::Arbitrary, proptest, test_runner::TestCaseError};

use crate::syntax_tree::target::Target;

proptest! {
    #[test]
    #[allow(clippy::redundant_closure_for_method_calls)]
    fn module_path_test(
        module_path_input in super::input::ModulePath::arbitrary()
    ) {
        let source = module_path_input.to_string();

        let module_path = crate::syntax_tree::tests::parse(
            &source,
            |parser, handler| parser.parse_module_path(handler)
        )?;

        module_path_input.assert(&module_path)?;
    }

    #[test]
    #[allow(clippy::redundant_closure_for_method_calls)]
    fn using_test(
        using_input in super::input::Using::arbitrary()
    ) {
        let source = using_input.to_string();

        let using = crate::syntax_tree::tests::parse(
            &source,
            |parser, handler| parser.parse_using(handler)
        )?;

        using_input.assert(&using)?;
    }

    #[test]
    #[allow(clippy::redundant_closure_for_method_calls)]
    fn module_test(
        module_input in super::input::Module::arbitrary()
    ) {
        let source = module_input.to_string();

        let module = crate::syntax_tree::tests::parse(
            &source,
            |parser, handler| parser.parse_module(handler)
        )?;

        module_input.assert(&module)?;
    }

    #[test]
    #[allow(clippy::redundant_closure_for_method_calls)]
    fn target_test(
        target_input in super::input::File::arbitrary()
    ) {
        let target_dir = target_input.create_target()?;
        let root_source_file = SourceFile::load(&target_dir.path().join("main.pnx"))?;
        let storage = Storage::<Error>::new();
        let target = Target::parse(root_source_file, "test".to_string(), &storage);

        if !storage.as_vec().is_empty() {
            return Err(TestCaseError::fail(format!("parsing error: {:#?}",storage.as_vec())));
        }

        target_input.assert(&target.root_file)?;
    }
}

/// Enumeration of all error types that can be returned when parsing a target tree.
#[derive(Debug, EnumAsInner, From)]
enum Error {
    Lexical(pernixc_lexical::error::Error),
    Syntax(crate::error::Error),
    Target(crate::syntax_tree::target::Error),
}
