use std::fmt::{Debug, Display};

use pernixc_parser::abstract_tree::AbstractTree;
use pernixc_source_file::{GlobalSourceID, SourceFile, SourceMap};
use pernixc_target::TargetID;
use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, TestCaseError},
    prop_assert,
    test_runner::TestCaseResult,
};

use crate::{
    arbitrary::{self, IndentDisplay, IndentDisplayItem},
    QualifiedIdentifier,
};

pub fn parse_token_tree(
    source_map: &mut SourceMap,
    source_code: &str,
) -> (pernixc_lexical::tree::Tree, GlobalSourceID) {
    let source_id = source_map.register(
        TargetID::Local,
        SourceFile::new(source_code.to_string(), "test".into()),
    );
    let source =
        source_map.get(TargetID::Local.make_global(source_id)).unwrap();

    let tree = pernixc_lexical::tree::Tree::from_source(
        source.content(),
        TargetID::Local.make_global(source_id),
        &pernixc_handler::Panic,
    );

    (tree, TargetID::Local.make_global(source_id))
}

pub fn verify_ref<TR: IndentDisplay, TAst: AbstractTree + Debug>(
    ast_ref: &TR,
) -> TestCaseResult
where
    for<'x, 'y> &'x TR: Input<&'y TAst, ()>,
{
    let mut source_map = SourceMap::new();

    let source = IndentDisplayItem(0, ast_ref).to_string();
    println!("{source}");
    println!("===============================================================");
    let (token_tree, _) = parse_token_tree(&mut source_map, &source);

    let (tree, errors) = TAst::parse(&token_tree);
    let tree = tree.ok_or_else(|| {
        TestCaseError::fail(format!("Failed to parse tree: {errors:?}"))
    })?;

    prop_assert!(errors.is_empty(), "{errors:?}");

    ast_ref.assert(&tree, ())
}

pub fn verify_ref_display<TR: Display, TAst: AbstractTree + Debug>(
    ast_ref: &TR,
) -> TestCaseResult
where
    for<'x, 'y> &'x TR: Input<&'y TAst, ()>,
{
    let mut source_map = SourceMap::new();

    let source = ast_ref.to_string();
    println!("{source}");
    println!("===============================================================");
    let (token_tree, _) = parse_token_tree(&mut source_map, &source);

    let (tree, errors) = TAst::parse(&token_tree);
    let tree = tree.ok_or_else(|| {
        TestCaseError::fail(format!("Failed to parse tree: {errors:?}"))
    })?;

    prop_assert!(errors.is_empty(), "{errors:?}");

    ast_ref.assert(&tree, ())
}

proptest::proptest! {
    #[test]
    fn qualified_identifier(
        reference in arbitrary::QualifiedIdentifier::arbitrary()
    ) {
        verify_ref::<_, QualifiedIdentifier>(&reference)?;
    }
}
