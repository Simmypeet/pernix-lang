use pernixc_parser::abstract_tree::AbstractTree;
use pernixc_qbice::DuplicatingInterner;

use crate::{
    expression::block::{GroupOrIfElse, IfCondition, IfElse},
    test::parse_token_tree,
};

fn parse_if_else(source: &str) -> IfElse {
    let mut source_map =
        pernixc_source_file::simple_source_map::SimpleSourceMap::new();
    let (token_tree, _) = parse_token_tree(&mut source_map, source);
    let (tree, errors) = IfElse::parse(&token_tree, &DuplicatingInterner);

    assert!(errors.is_empty(), "{errors:?}");

    tree.expect("expected `if` expression to parse")
}

#[test]
fn if_match_inline_condition() {
    let tree = parse_if_else("if option match as Some(value): value");

    let IfCondition::Match(condition) = tree.condition().unwrap() else {
        panic!("expected `if-match` condition");
    };

    assert!(condition.binary().is_some());
    assert!(
        condition.refutable_pattern().is_some_and(|pattern| pattern.is_enum())
    );
}

#[test]
fn if_match_with_nested_else_if() {
    let tree = parse_if_else(
        "if option match as Some(value): value else if fallback: 0",
    );

    let Some(GroupOrIfElse::IfElse(else_if)) =
        tree.r#else().and_then(|x| x.group_or_if_else())
    else {
        panic!("expected nested `else if`");
    };

    assert!(matches!(else_if.condition(), Some(IfCondition::Boolean(_))));
}

#[test]
fn if_match_with_indented_then_group() {
    let tree = parse_if_else(
        "if option match as Some(value):\n    value\nelse:\n    0\n",
    );

    assert!(matches!(tree.condition(), Some(IfCondition::Match(_))));
    assert!(tree.then().is_some_and(|group| group.is_indented()));
    assert!(
        tree.r#else()
            .and_then(|x| x.group_or_if_else())
            .is_some_and(|group| matches!(group, GroupOrIfElse::Group(group) if group.is_indented()))
    );
}
