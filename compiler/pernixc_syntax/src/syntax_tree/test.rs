use std::sync::Arc;

use pernixc_handler::Storage;
use pernixc_lexical::{
    token::KeywordKind,
    token_stream::{DelimiterKind, TokenStream, Tree},
};
use pernixc_source_file::SourceFile;
use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, TestCaseError},
    proptest,
};

use super::SyntaxTree;
use crate::{
    error,
    state_machine::{parse::Parse, StateMachine},
    syntax_tree::{
        strategy::QualifiedIdentifier, EnclosedConnectedList, ParseExt,
    },
};

pub fn parse<S: SyntaxTree>(source: &str) -> Result<S, TestCaseError> {
    let (token_stream, _) = create_token_stream(source.to_string());

    let tree = Tree::new(&token_stream);
    let storage: Storage<error::Error> = Storage::new();

    let output = S::parse.parse_syntax(&tree, &storage);

    if !storage.as_vec().is_empty() {
        return Err(TestCaseError::fail(format!(
            "found syntax error(s): {:#?};\nsource: {source}",
            storage.as_vec(),
        )));
    }

    output.map_or_else(
        || {
            Err(TestCaseError::fail(format!(
                "failed to parse the source code: {source}",
            )))
        },
        |output| Ok(output),
    )
}

proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 10000,
        ..proptest::test_runner::Config::default()
    })]
    #[allow(clippy::ignored_unit_patterns)]
    #[test]
    fn qualified_identifier(
        qualified_identifier_input in QualifiedIdentifier::arbitrary(),
    ) {
        let source = qualified_identifier_input.to_string();
        let qualified_identifier = parse::<super::QualifiedIdentifier>(&source)?;

        qualified_identifier_input.assert(&qualified_identifier)?;
    }
}

#[test]
fn empty_enclosed_connected_list() {
    let (token_stream, _) = create_token_stream("[]".to_string());

    let tree = Tree::new(&token_stream);
    let mut state_machine = StateMachine::new(&tree);

    let EnclosedConnectedList { open, connected_list, close } =
        KeywordKind::Continue
            .enclosed_connected_list(','.to_owned(), DelimiterKind::Bracket)
            .parse(&mut state_machine, &pernixc_handler::Panic)
            .unwrap();

    assert_eq!(open.punctuation, '[');
    assert_eq!(connected_list, None);
    assert_eq!(close.punctuation, ']');
}

#[test]
fn single_trailing_separator_enclosed_connected_list() {
    let (token_stream, _) = create_token_stream("[continue,]".to_string());

    let tree = Tree::new(&token_stream);
    let mut state_machine = StateMachine::new(&tree);

    let EnclosedConnectedList { open, connected_list, close } =
        KeywordKind::Continue
            .enclosed_connected_list(','.to_owned(), DelimiterKind::Bracket)
            .parse(&mut state_machine, &pernixc_handler::Panic)
            .unwrap();

    let connected_list = connected_list.unwrap();

    assert_eq!(open.punctuation, '[');
    assert_eq!(connected_list.first.kind, KeywordKind::Continue);
    assert_eq!(connected_list.rest.len(), 0);
    assert!(connected_list.trailing_separator.is_some());
    assert_eq!(close.punctuation, ']');
}

#[test]
fn multiple_elements_enclosed_connected_list() {
    let (token_stream, _) =
        create_token_stream("[continue, continue, continue]".to_string());

    let tree = Tree::new(&token_stream);
    let mut state_machine = StateMachine::new(&tree);

    let EnclosedConnectedList { open, connected_list, close } =
        KeywordKind::Continue
            .enclosed_connected_list(','.to_owned(), DelimiterKind::Bracket)
            .parse(&mut state_machine, &pernixc_handler::Panic)
            .unwrap();

    let connected_list = connected_list.unwrap();

    assert_eq!(open.punctuation, '[');
    assert_eq!(connected_list.first.kind, KeywordKind::Continue);
    assert_eq!(connected_list.rest.len(), 2);
    assert!(connected_list.trailing_separator.is_none());
    assert_eq!(close.punctuation, ']');
}

fn create_token_stream(source: String) -> (TokenStream, Arc<SourceFile>) {
    let source_file = Arc::new(SourceFile::new(source, "test".into()));
    let token_stream =
        TokenStream::tokenize(source_file.clone(), &pernixc_handler::Panic);

    (token_stream, source_file)
}

#[test]
fn multiple_elements_with_trailing_separator_enclosed_connected_list() {
    let (token_stream, _) =
        create_token_stream("[continue, continue, continue,]".to_string());

    let tree = Tree::new(&token_stream);
    let mut state_machine = StateMachine::new(&tree);

    let EnclosedConnectedList { open, connected_list, close } =
        KeywordKind::Continue
            .enclosed_connected_list(','.to_owned(), DelimiterKind::Bracket)
            .parse(&mut state_machine, &pernixc_handler::Panic)
            .unwrap();

    let connected_list = connected_list.unwrap();

    assert_eq!(open.punctuation, '[');
    assert_eq!(connected_list.first.kind, KeywordKind::Continue);
    assert_eq!(connected_list.rest.len(), 2);
    assert!(connected_list.trailing_separator.is_some());
    assert_eq!(close.punctuation, ']');
}
