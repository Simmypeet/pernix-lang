use pernixc_lexical::{token, tree::RelativeLocation};
use pernixc_source_file::{GlobalSourceID, SourceFile, SourceMap};
use pernixc_target::TargetID;

use crate::{
    abstract_tree::{abstract_tree, AbstractTree},
    expect,
};

abstract_tree! {
    pub struct BasicSequence {
        pub public_keyword: token::Keyword<RelativeLocation>
            = expect::Keyword::Public,

        pub struct_keyword: token::Keyword<RelativeLocation>
            = expect::Keyword::Struct,

        pub identifier: token::Identifier<RelativeLocation>
            = expect::Identifier,

        pub semicolon: token::Punctuation<RelativeLocation>
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

#[test]
fn basic_sequence() {
    let mut source_map = SourceMap::new();
    let (tree, _) = parse_token_tree(&mut source_map, "public struct Foo;");

    let (tree, errors) = BasicSequence::parse(&tree);
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

    assert_eq!(tree.semicolon().map(|x| x.kind.0), Some(';'));
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
