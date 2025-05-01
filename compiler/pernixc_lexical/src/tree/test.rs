/*
use pernixc_arena::ID;
use pernixc_handler::Panic;
use pernixc_source_file::{
    ByteIndex, GlobalSourceID, SourceElement, SourceFile, SourceMap,
};
use pernixc_target::TargetID;
use proptest::{
    prop_assert, prop_assert_eq, proptest, test_runner::TestCaseResult,
};

use super::{Branch, BranchKind, FragmentBranch, RelativeSpan};
use crate::{
    error::Error,
    token_stream::{
        arbitrary::arbitrary_indentation, FragmentKind, TokenStream,
    },
    tree::Tree,
};

fn add_source_element_string<
    S: SourceElement<Span = RelativeSpan<GlobalSourceID>> + std::fmt::Debug,
>(
    tree: &Tree<GlobalSourceID>,
    source_element: &S,
    source: &str,
    dest: &mut String,
) {
    let absolute_span = tree.absolute_span_of(&source_element.span());
    let absolute_range = absolute_span.range();

    let str = &source[absolute_range.start..absolute_range.end];
    dest.push_str(str);
}

fn create_string_from_tree(
    tree: &Tree<GlobalSourceID>,
    branch_id: ID<Branch<GlobalSourceID>>,
    source: &str,
    dest: &mut String,
) {
    let branch = &tree.branches()[branch_id];

    match &branch.kind {
        BranchKind::Fragment(fragment_branch) => {
            match &fragment_branch.fragment_kind {
                FragmentKind::Delimiter(delimiter) => {
                    add_source_element_string(
                        tree,
                        &delimiter.open,
                        source,
                        dest,
                    );
                }
                FragmentKind::Indentation(indentation) => {
                    add_source_element_string(
                        tree,
                        &indentation.colon,
                        source,
                        dest,
                    );
                    add_source_element_string(
                        tree,
                        &indentation.new_line,
                        source,
                        dest,
                    );
                }
            }
        }
        BranchKind::Root => {}
    }

    for node in &branch.nodes {
        match node {
            super::Node::Leaf(leaf) => {
                add_source_element_string(tree, leaf, source, dest);
            }
            super::Node::Branch(branch_id) => {
                create_string_from_tree(tree, *branch_id, source, dest);
            }
        }
    }

    if let BranchKind::Fragment(FragmentBranch {
        fragment_kind: FragmentKind::Delimiter(delimiter),
        ..
    }) = &branch.kind
    {
        add_source_element_string(tree, &delimiter.close, source, dest);
    }
}

fn tree_to_string(
    input: &crate::token_stream::arbitrary::Indentation,
) -> TestCaseResult {
    let mut source_map = SourceMap::default();
    let string = input.to_string();

    let file = SourceFile::new(string.clone(), "test".into());
    let id = source_map.register(TargetID::Local, file);
    let id = TargetID::Local.make_global(id);

    let storage =
        pernixc_handler::Storage::<Error<ByteIndex, GlobalSourceID>>::new();

    let token_stream =
        TokenStream::tokenize(source_map[id].content(), id, &storage);

    let storage = storage.into_vec();
    prop_assert!(storage.is_empty(), "{storage:?}");

    let tree = Tree::from_token_stream(token_stream, source_map[id].content());

    let mut dest = String::new();
    create_string_from_tree(
        &tree,
        tree.root_id(),
        source_map[id].content(),
        &mut dest,
    );

    prop_assert_eq!(dest, string, "{:#?} != {:#?}", input, tree);

    Ok(())
}

proptest! {
    #[test]
    fn tree(
        input in arbitrary_indentation()
    ) {
        tree_to_string(&input)?;
    }
}
const STABLE_NODE_ID_1: &str = "a a {b b} c c"; const STABLE_NODE_ID_2: &str = "a a c c";
const STABLE_NODE_ID_3: &str = "a a {c b c} c c";
const STABLE_NODE_ID_4: &str = "a a {c {d} c} c c";

fn calculate_root_node_id(source: String) -> u64 {
    let mut source_map = SourceMap::default();
    let file = SourceFile::new(source, "test".into());

    let id = source_map.register(TargetID::Local, file);
    let id = TargetID::Local.make_global(id);

    let token_stream =
        TokenStream::tokenize(source_map[id].content(), id, &Panic);

    let tree = Tree::from_token_stream(token_stream, source_map[id].content());

    tree.root_id().index()
}

#[test]
fn stable_node_id() {
    let stable_root_ids = [
        STABLE_NODE_ID_1,
        STABLE_NODE_ID_2,
        STABLE_NODE_ID_3,
        STABLE_NODE_ID_4,
    ]
    .into_iter()
    .map(ToString::to_string)
    .map(calculate_root_node_id);

    let mut iter = stable_root_ids.clone();
    let first = iter.next().unwrap();

    for id in iter {
        assert_eq!(first, id);
    }
}
*/

use pernixc_handler::Storage;
use pernixc_source_file::{SourceFile, SourceMap};
use pernixc_target::TargetID;
use pernixc_test_input::Input;
use proptest::{prop_assert, proptest, test_runner::TestCaseResult};

use super::{
    arbitrary::{self, IndentationBlock, IndentationLine, Nodes},
    Tree, ROOT_BRANCH_ID,
};
use crate::{
    error::Error,
    kind::{self, Keyword},
    token,
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
        let branch = dbg!(&tree[branch]);

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

fn verify_tree(input: &arbitrary::Nodes) -> TestCaseResult {
    println!("{input}");
    let mut source_map = SourceMap::new();
    let source = input.to_string();

    let source_file = SourceFile::new(source, "test".into());

    let id = source_map.register(TargetID::Local, source_file);
    let id = TargetID::Local.make_global(id);

    let storage = Storage::<Error>::new();
    let tree =
        dbg!(super::Tree::from_source(source_map[id].content(), id, &storage));

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

#[test]
fn minimal() -> Result<(), proptest::prelude::TestCaseError> {
    verify_tree(&arbitrary::Nodes(vec![
        arbitrary::Node::Fragment(arbitrary::Fragment::Delimited(
            arbitrary::Delimited {
                delimiter: DelimiterKind::Bracket,
                open_insignificant: None,
                close_insignificant: None,
                nodes: Nodes(vec![]),
            },
        )),
        arbitrary::Node::Fragment(arbitrary::Fragment::Delimited(
            arbitrary::Delimited {
                delimiter: DelimiterKind::Bracket,
                open_insignificant: None,
                close_insignificant: None,
                nodes: Nodes(vec![arbitrary::Node::Leaf(
                    token::arbitrary::Token {
                        kind: kind::arbitrary::Kind::Keyword(Keyword::True),
                        prior_insignificant: None,
                    },
                )]),
            },
        )),
    ]))
}
