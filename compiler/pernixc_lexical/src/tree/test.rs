use pernixc_handler::Storage;
use pernixc_source_file::{SourceFile, SourceMap};
use pernixc_target::TargetID;
use pernixc_test_input::Input;
use proptest::{prop_assert, proptest, test_runner::TestCaseResult};

use super::{arbitrary, Tree, ROOT_BRANCH_ID};
use crate::{
    error::Error,
    tree::{arbitrary::arbitrary_nodes, BranchKind, DelimiterKind},
};

#[test]
fn basic_delimiter() {
    let source = "+ { - } *";
    let mut source_map = SourceMap::default();
    let source_id = TargetID::Local.make_global(source_map.register(
        TargetID::Local,
        SourceFile::new(source.to_string(), "test".into()),
    ));

    let source_content = source_map[source_id].content();

    let handler = pernixc_handler::Panic;
    let tree = Tree::from_source(source_content, source_id, &handler);

    let root_branch = &tree[ROOT_BRANCH_ID];

    assert_eq!(root_branch.nodes.len(), 3);
    assert_eq!(root_branch.kind, BranchKind::Root);

    assert_eq!(
        **root_branch.nodes[0]
            .as_leaf()
            .unwrap()
            .kind
            .as_punctuation()
            .unwrap(),
        '+'
    );

    {
        let branch = *root_branch.nodes[1].as_branch().unwrap();
        let branch = &tree[branch];

        assert_eq!(branch.nodes.len(), 1);
        assert!(branch
            .kind
            .as_fragment()
            .and_then(|x| x.fragment_kind.as_delimiter())
            .is_some_and(|x| x.delimiter == DelimiterKind::Brace));

        assert_eq!(
            **branch.nodes[0].as_leaf().unwrap().kind.as_punctuation().unwrap(),
            '-'
        );
    }

    assert_eq!(
        **root_branch.nodes[2]
            .as_leaf()
            .unwrap()
            .kind
            .as_punctuation()
            .unwrap(),
        '*'
    );
}

#[test]
fn basic_indentation() {
    let source: &str = "+:\n\t-\n*";

    let mut source_map = SourceMap::default();
    let source_id = TargetID::Local.make_global(source_map.register(
        TargetID::Local,
        SourceFile::new(source.to_string(), "test".into()),
    ));

    let source_content = source_map[source_id].content();

    let handler = pernixc_handler::Panic;
    let tree = Tree::from_source(source_content, source_id, &handler);

    let root_branch = &tree[ROOT_BRANCH_ID];

    assert_eq!(root_branch.nodes.len(), 4);
    assert_eq!(root_branch.kind, BranchKind::Root);

    assert_eq!(
        **root_branch.nodes[0]
            .as_leaf()
            .unwrap()
            .kind
            .as_punctuation()
            .unwrap(),
        '+'
    );

    {
        let branch = *root_branch.nodes[1].as_branch().unwrap();
        let branch = &tree[branch];

        assert_eq!(branch.nodes.len(), 1);
        assert!(branch
            .kind
            .as_fragment()
            .is_some_and(|x| x.fragment_kind.is_indentation()));

        assert_eq!(
            **branch.nodes[0].as_leaf().unwrap().kind.as_punctuation().unwrap(),
            '-'
        );
    }

    assert!(root_branch.nodes[2].as_leaf().unwrap().kind.is_new_line());
    assert_eq!(
        **root_branch.nodes[3]
            .as_leaf()
            .unwrap()
            .kind
            .as_punctuation()
            .unwrap(),
        '*'
    );
}

const NESTED_SINGLE_POP_INDENTATION: &str = "+:
    -:
        *
    /
,";

#[test]
fn nested_single_pop_indentation() {
    let mut source_map = SourceMap::default();
    let source_id = TargetID::Local.make_global(source_map.register(
        TargetID::Local,
        SourceFile::new(
            NESTED_SINGLE_POP_INDENTATION.to_string(),
            "test".into(),
        ),
    ));

    let source_content = source_map[source_id].content();

    let handler = pernixc_handler::Panic;
    let tree = Tree::from_source(source_content, source_id, &handler);

    let root_branch = &tree[ROOT_BRANCH_ID];

    assert_eq!(root_branch.nodes.len(), 4);
    assert_eq!(root_branch.kind, BranchKind::Root);

    assert_eq!(
        **root_branch.nodes[0]
            .as_leaf()
            .unwrap()
            .kind
            .as_punctuation()
            .unwrap(),
        '+'
    );

    {
        let branch = *root_branch.nodes[1].as_branch().unwrap();
        let branch = &tree[branch];

        assert_eq!(branch.nodes.len(), 4);
        assert!(branch
            .kind
            .as_fragment()
            .is_some_and(|x| x.fragment_kind.is_indentation()));

        assert_eq!(
            **branch.nodes[0].as_leaf().unwrap().kind.as_punctuation().unwrap(),
            '-'
        );

        {
            let branch = *branch.nodes[1].as_branch().unwrap();
            let branch = &tree[branch];

            assert_eq!(branch.nodes.len(), 1);
            assert!(branch
                .kind
                .as_fragment()
                .is_some_and(|x| x.fragment_kind.is_indentation()));

            assert_eq!(
                **branch.nodes[0]
                    .as_leaf()
                    .unwrap()
                    .kind
                    .as_punctuation()
                    .unwrap(),
                '*'
            );
        }

        assert!(branch.nodes[2].as_leaf().unwrap().kind.is_new_line());
        assert_eq!(
            **branch.nodes[3].as_leaf().unwrap().kind.as_punctuation().unwrap(),
            '/'
        );
    }

    assert!(root_branch.nodes[2].as_leaf().unwrap().kind.is_new_line());
    assert_eq!(
        **root_branch.nodes[3]
            .as_leaf()
            .unwrap()
            .kind
            .as_punctuation()
            .unwrap(),
        ','
    );
}

const NESTED_MULTI_POP_INDENTATION: &str = "+:
    -:
        :
            *
    /
,";

#[test]
fn nested_multi_pop_indentation() {
    let mut source_map = SourceMap::default();
    let source_id = TargetID::Local.make_global(source_map.register(
        TargetID::Local,
        SourceFile::new(
            NESTED_MULTI_POP_INDENTATION.to_string(),
            "test".into(),
        ),
    ));

    let source_content = source_map[source_id].content();

    let handler = pernixc_handler::Panic;
    let tree = Tree::from_source(source_content, source_id, &handler);

    let root_branch = &tree[ROOT_BRANCH_ID];

    assert_eq!(root_branch.nodes.len(), 4);
    assert_eq!(root_branch.kind, BranchKind::Root);

    assert_eq!(
        **root_branch.nodes[0]
            .as_leaf()
            .unwrap()
            .kind
            .as_punctuation()
            .unwrap(),
        '+'
    );

    {
        let branch = *root_branch.nodes[1].as_branch().unwrap();
        let branch = &tree[branch];

        assert_eq!(branch.nodes.len(), 4);
        assert!(branch
            .kind
            .as_fragment()
            .is_some_and(|x| x.fragment_kind.is_indentation()));

        assert_eq!(
            **branch.nodes[0].as_leaf().unwrap().kind.as_punctuation().unwrap(),
            '-'
        );

        {
            let branch = *branch.nodes[1].as_branch().unwrap();
            let branch = &tree[branch];

            assert_eq!(branch.nodes.len(), 1);
            assert!(branch
                .kind
                .as_fragment()
                .is_some_and(|x| x.fragment_kind.is_indentation()));

            {
                let branch = *branch.nodes[0].as_branch().unwrap();
                let branch = &tree[branch];

                assert_eq!(branch.nodes.len(), 1);
                assert!(branch
                    .kind
                    .as_fragment()
                    .is_some_and(|x| x.fragment_kind.is_indentation()));

                assert_eq!(
                    **branch.nodes[0]
                        .as_leaf()
                        .unwrap()
                        .kind
                        .as_punctuation()
                        .unwrap(),
                    '*'
                );
            }
        }

        assert!(branch.nodes[2].as_leaf().unwrap().kind.is_new_line());
        assert_eq!(
            **branch.nodes[3].as_leaf().unwrap().kind.as_punctuation().unwrap(),
            '/'
        );
    }

    assert!(root_branch.nodes[2].as_leaf().unwrap().kind.is_new_line());
    assert_eq!(
        **root_branch.nodes[3]
            .as_leaf()
            .unwrap()
            .kind
            .as_punctuation()
            .unwrap(),
        ','
    );
}

const INDENTATION_POP_IN_DELIMITER: &str = ":
    (:
        *)
+";

#[test]
fn indentation_pop_in_delimiter() {
    let mut source_map = SourceMap::default();
    let source_id = TargetID::Local.make_global(source_map.register(
        TargetID::Local,
        SourceFile::new(
            INDENTATION_POP_IN_DELIMITER.to_string(),
            "test".into(),
        ),
    ));

    let source_content = source_map[source_id].content();

    let handler = pernixc_handler::Panic;
    let tree = Tree::from_source(source_content, source_id, &handler);

    let root_branch = &tree[ROOT_BRANCH_ID];

    assert_eq!(root_branch.nodes.len(), 3);
    assert_eq!(root_branch.kind, BranchKind::Root);

    {
        let branch = *root_branch.nodes[0].as_branch().unwrap();
        let branch = &tree[branch];

        assert_eq!(branch.nodes.len(), 1);
        assert!(branch
            .kind
            .as_fragment()
            .is_some_and(|x| x.fragment_kind.is_indentation()));

        {
            let branch = *branch.nodes[0].as_branch().unwrap();
            let branch = &tree[branch];

            assert_eq!(branch.nodes.len(), 1);
            assert!(branch
                .kind
                .as_fragment()
                .is_some_and(|x| x.fragment_kind.as_delimiter().is_some_and(
                    |x| x.delimiter == DelimiterKind::Parenthesis
                )));

            {
                let branch = *branch.nodes[0].as_branch().unwrap();
                let branch = &tree[branch];

                assert_eq!(branch.nodes.len(), 1);
                assert!(branch
                    .kind
                    .as_fragment()
                    .is_some_and(|x| x.fragment_kind.is_indentation()));

                assert_eq!(
                    **branch.nodes[0]
                        .as_leaf()
                        .unwrap()
                        .kind
                        .as_punctuation()
                        .unwrap(),
                    '*'
                );
            }
        }
    }

    assert!(root_branch.nodes[1].as_leaf().unwrap().kind.is_new_line());
    assert_eq!(
        **root_branch.nodes[2]
            .as_leaf()
            .unwrap()
            .kind
            .as_punctuation()
            .unwrap(),
        '+'
    );
}

const INDENTATION_POP_ALL_AT_END: &str = ":
    +
    :
        -
";

#[test]
fn indentation_pop_all_at_end() {
    let mut source_map = SourceMap::default();
    let source_id = TargetID::Local.make_global(source_map.register(
        TargetID::Local,
        SourceFile::new(INDENTATION_POP_ALL_AT_END.to_string(), "test".into()),
    ));

    let source_content = source_map[source_id].content();

    let handler = pernixc_handler::Panic;
    let tree = Tree::from_source(source_content, source_id, &handler);

    let root_branch = &tree[ROOT_BRANCH_ID];

    assert_eq!(root_branch.nodes.len(), 2);
    assert_eq!(root_branch.kind, BranchKind::Root);

    {
        let branch = *root_branch.nodes[0].as_branch().unwrap();
        let branch = &tree[branch];

        assert_eq!(branch.nodes.len(), 3);
        assert!(branch
            .kind
            .as_fragment()
            .is_some_and(|x| x.fragment_kind.is_indentation()));

        assert_eq!(
            **branch.nodes[0].as_leaf().unwrap().kind.as_punctuation().unwrap(),
            '+'
        );
        assert!(branch.nodes[1].as_leaf().unwrap().kind.is_new_line());

        {
            let branch = *branch.nodes[2].as_branch().unwrap();
            let branch = &tree[branch];

            assert_eq!(branch.nodes.len(), 1);
            assert!(branch
                .kind
                .as_fragment()
                .is_some_and(|x| x.fragment_kind.is_indentation()));

            assert_eq!(
                **branch.nodes[0]
                    .as_leaf()
                    .unwrap()
                    .kind
                    .as_punctuation()
                    .unwrap(),
                '-'
            );
        }
    }

    assert!(root_branch.nodes[1].as_leaf().unwrap().kind.is_new_line());
}

const UNCLOSED_DELIMITER: &str = "-([{}";

#[test]
fn unclosed_delimiter() {
    let mut source_map = SourceMap::default();
    let source_id = TargetID::Local.make_global(source_map.register(
        TargetID::Local,
        SourceFile::new(UNCLOSED_DELIMITER.to_string(), "test".into()),
    ));

    let source_content = source_map[source_id].content();

    let storage = Storage::<Error>::new();
    let tree = Tree::from_source(source_content, source_id, &storage);

    let root_branch = &tree[ROOT_BRANCH_ID];

    assert_eq!(root_branch.nodes.len(), 4);
    assert_eq!(root_branch.kind, BranchKind::Root);

    assert_eq!(
        **root_branch.nodes[0]
            .as_leaf()
            .unwrap()
            .kind
            .as_punctuation()
            .unwrap(),
        '-'
    );
    assert_eq!(
        **root_branch.nodes[1]
            .as_leaf()
            .unwrap()
            .kind
            .as_punctuation()
            .unwrap(),
        '('
    );
    assert_eq!(
        **root_branch.nodes[2]
            .as_leaf()
            .unwrap()
            .kind
            .as_punctuation()
            .unwrap(),
        '['
    );
    {
        let branch = *root_branch.nodes[3].as_branch().unwrap();
        let branch = &tree[branch];

        assert_eq!(branch.nodes.len(), 0);
        assert!(branch.kind.as_fragment().is_some_and(|x| x
            .fragment_kind
            .as_delimiter()
            .is_some_and(|x| x.delimiter == DelimiterKind::Brace)));
    }

    let errors = storage.into_vec();
    assert_eq!(errors.len(), 2);

    assert!(errors.iter().any(|x| {
        x.as_undelimited_delimiter().is_some_and(|x| {
            x.delimiter == DelimiterKind::Parenthesis
                && x.opening_span.start == 1
                && x.opening_span.end == 2
        })
    }));

    assert!(errors.iter().any(|x| {
        x.as_undelimited_delimiter().is_some_and(|x| {
            x.delimiter == DelimiterKind::Bracket
                && x.opening_span.start == 2
                && x.opening_span.end == 3
        })
    }));
}

fn verify_tree(input: &arbitrary::Nodes) -> TestCaseResult {
    let mut source_map = SourceMap::new();
    let source = input.to_string();

    let source_file = SourceFile::new(source, "test".into());

    let id = source_map.register(TargetID::Local, source_file);
    let id = TargetID::Local.make_global(id);

    let storage = Storage::<Error>::new();
    let tree = super::Tree::from_source(source_map[id].content(), id, &storage);

    let storage = storage.as_vec();
    prop_assert!(storage.is_empty(), "{storage:?}");

    let root = &tree[ROOT_BRANCH_ID];
    input.assert(&root.nodes, (&source_map, &tree))?;

    Ok(())
}

proptest! {
   #[test]
    fn tree(
        input in arbitrary_nodes()
    ) {
        verify_tree(&input)?;
    }
}
