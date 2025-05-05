use flexstr::SharedStr;
use pernixc_lexical::{token, tree::RelativeLocation};
use pernixc_source_file::{GlobalSourceID, SourceFile, SourceMap};
use pernixc_target::TargetID;

use crate::{
    abstract_tree::{abstract_tree, AbstractTree, Tag},
    expect,
    parser::ast,
};

abstract_tree! {
    struct BasicSequence {
        public_keyword: token::Keyword<RelativeLocation>
            = expect::Keyword::Public,

        struct_keyword: token::Keyword<RelativeLocation>
            = expect::Keyword::Struct,

        identifier: token::Identifier<RelativeLocation>
            = expect::Identifier,

        semicolon: token::Punctuation<RelativeLocation>
            = ';'
    }
}

fn parse_token_tree(
    source_map: &mut SourceMap,
    source_code: &str,
) -> (pernixc_lexical::tree::Tree, GlobalSourceID) {
    let source_id = source_map.register(
        TargetID::Local,
        SourceFile::new(source_code.to_string(), "test".into()),
    );
    let source = &source_map[TargetID::Local.make_global(source_id)];

    let tree = pernixc_lexical::tree::Tree::from_source(
        source.content(),
        TargetID::Local.make_global(source_id),
        &pernixc_handler::Panic,
    );

    (tree, TargetID::Local.make_global(source_id))
}

fn check_basic_sequence(
    basic_sequence: &BasicSequence,
    expected_name: SharedStr,
) {
    assert_eq!(
        basic_sequence.public_keyword().map(|x| x.kind),
        Some(expect::Keyword::Public)
    );

    assert_eq!(
        basic_sequence.struct_keyword().map(|x| x.kind),
        Some(expect::Keyword::Struct)
    );

    assert_eq!(
        basic_sequence.identifier().map(|x| x.kind.0),
        Some(expected_name)
    );

    assert_eq!(basic_sequence.semicolon().map(|x| x.kind.0), Some(';'));
}

#[test]
fn basic_sequence() {
    let mut source_map = SourceMap::new();
    let (tree, _) = parse_token_tree(&mut source_map, "public struct Foo;");

    let (tree, errors) = BasicSequence::parse(&tree);
    let tree = tree.unwrap();

    check_basic_sequence(&tree, "Foo".into());

    assert!(errors.is_empty());
}

#[test]
fn basic_sequence_missing_semicolon() {
    let mut source_map = SourceMap::new();
    let (tree, _) = parse_token_tree(&mut source_map, "public struct Foo");

    let (tree, errors) = BasicSequence::parse(&tree);
    let tree = tree.unwrap();

    assert_eq!(errors.len(), 1);

    assert_eq!(
        tree.public_keyword().map(|x| x.kind),
        Some(expect::Keyword::Public)
    );

    assert_eq!(
        tree.struct_keyword().map(|x| x.kind),
        Some(expect::Keyword::Struct)
    );

    assert_eq!(tree.identifier().map(|x| x.kind.0), Some("Foo".into()));

    assert_eq!(tree.semicolon(), None);
}

abstract_tree! {
    #[derive(Debug)]
    struct TwoBasicSequences {
        first: Tag<BasicSequence, 1> = ast::<Tag<BasicSequence, 1>>(),
        second: Tag<BasicSequence, 2> = ast::<Tag<BasicSequence, 2>>(),
    }
}

#[test]
fn two_basic_sequences() {
    let mut source_map = SourceMap::new();
    let (tree, _) = parse_token_tree(
        &mut source_map,
        "public struct Foo; public struct Bar;",
    );

    let (tree, errors) = TwoBasicSequences::parse(&tree);
    let tree = dbg!(tree.unwrap());

    assert!(errors.is_empty());

    check_basic_sequence(&tree.first().unwrap(), "Foo".into());
    check_basic_sequence(&tree.second().unwrap(), "Bar".into());
}

abstract_tree! {
    #{fragment = expect::Fragment::Indentation}
    struct Body {
        private_keyword: token::Keyword<RelativeLocation>
            = expect::Keyword::Private,
        identifier: token::Identifier<RelativeLocation>
            = expect::Identifier,
        colon: token::Punctuation<RelativeLocation>
            = ':',
        int32_keyword: token::Keyword<RelativeLocation>
            = expect::Keyword::Int32,
    }
}

abstract_tree! {
    struct SequenceWithFragment {
        public_keyword: token::Keyword<RelativeLocation>
            = expect::Keyword::Public,
        struct_keyword: token::Keyword<RelativeLocation>
            = expect::Keyword::Struct,
        identifier: token::Identifier<RelativeLocation>
            = expect::Identifier,
        body: Body = ast::<Body>(),
    }
}

const SEQUENCE_WITH_FRAGMENT: &str = "
public struct Foo: 
    private bar: int32
";

#[test]
fn sequence_with_fragment() {
    let mut source_map = SourceMap::new();
    let (tree, _) = parse_token_tree(&mut source_map, SEQUENCE_WITH_FRAGMENT);

    let (tree, errors) = SequenceWithFragment::parse(&tree);
    let tree = tree.unwrap();

    assert!(errors.is_empty());

    assert_eq!(
        tree.public_keyword().map(|x| x.kind),
        Some(expect::Keyword::Public)
    );

    assert_eq!(
        tree.struct_keyword().map(|x| x.kind),
        Some(expect::Keyword::Struct)
    );

    assert_eq!(tree.identifier().map(|x| x.kind.0), Some("Foo".into()));

    let body = tree.body().unwrap();

    assert_eq!(
        body.private_keyword().map(|x| x.kind),
        Some(expect::Keyword::Private)
    );

    assert_eq!(body.identifier().map(|x| x.kind.0), Some("bar".into()));

    assert_eq!(body.colon().map(|x| x.kind.0), Some(':'));

    assert_eq!(
        body.int32_keyword().map(|x| x.kind),
        Some(expect::Keyword::Int32)
    );
}
