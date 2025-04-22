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

const STABLE_NODE_ID_1: &str = "a a {b b} c c";
const STABLE_NODE_ID_2: &str = "a a c c";
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
