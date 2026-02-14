use std::{
    fmt::{Debug, Display},
    path::PathBuf,
};

use pernixc_parser::abstract_tree::AbstractTree;
use pernixc_qbice::DuplicatingInterner;
use pernixc_source_file::{
    GlobalSourceID, SourceFile, simple_source_map::SimpleSourceMap,
};
use pernixc_target::TargetID;
use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, TestCaseError},
    prop_assert,
    test_runner::TestCaseResult,
};
use qbice::storage::intern::Interned;

use crate::{
    QualifiedIdentifier,
    arbitrary::{self, IndentDisplay, IndentDisplayItem},
};

pub fn parse_token_tree(
    source_map: &mut SimpleSourceMap,
    source_code: &str,
) -> (pernixc_lexical::tree::Tree, GlobalSourceID) {
    let source_id = source_map.register(
        TargetID::TEST,
        SourceFile::from_str(
            source_code,
            Interned::new_duplicating_unsized(PathBuf::from("test")),
        ),
    );
    let interned = DuplicatingInterner;
    let source = source_map.get(TargetID::TEST.make_global(source_id)).unwrap();

    let tree = pernixc_lexical::tree::Tree::from_source(
        &source,
        TargetID::TEST.make_global(source_id),
        &interned,
        &pernixc_handler::Panic,
    );

    (tree, TargetID::TEST.make_global(source_id))
}

pub fn verify_ref<TR: IndentDisplay, TAst: AbstractTree + Debug>(
    ast_ref: &TR,
) -> TestCaseResult
where
    for<'x, 'y> &'x TR: Input<&'y TAst, ()>,
{
    let mut source_map = SimpleSourceMap::new();

    let source = IndentDisplayItem(0, ast_ref).to_string();
    println!("{source}");
    println!("===============================================================");
    let (token_tree, _) = parse_token_tree(&mut source_map, &source);

    let interned = DuplicatingInterner;
    let (tree, errors) = TAst::parse(&token_tree, &interned);

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
    let mut source_map = SimpleSourceMap::new();

    let source = ast_ref.to_string();
    println!("{source}");
    println!("===============================================================");
    let (token_tree, _) = parse_token_tree(&mut source_map, &source);

    let interned = DuplicatingInterner;
    let (tree, errors) = TAst::parse(&token_tree, &interned);

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
