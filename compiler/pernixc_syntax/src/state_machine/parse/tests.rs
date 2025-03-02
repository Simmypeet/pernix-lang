use std::sync::Arc;

use pernixc_handler::{Handler, Storage};
use pernixc_lexical::{
    token::{Keyword, KeywordKind, Punctuation, Token},
    token_stream::{DelimiterKind, TokenStream, Tree},
};
use pernixc_source_file::SourceFile;

use super::ExpectExt;
use crate::{
    error::{self, Found},
    state_machine::{
        parse::{self, Branch, Parse},
        StateMachine,
    },
};

fn create_token_stream(source: String) -> (TokenStream, Arc<SourceFile>) {
    let source_file = Arc::new(SourceFile::new(source, "test".into()));
    let token_stream =
        TokenStream::tokenize(source_file.clone(), &pernixc_handler::Panic);

    (token_stream, source_file)
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum ConsList {
    Cons(Keyword, Box<ConsList>),
    Nil(Punctuation),
}

impl ConsList {
    fn count(&self) -> usize {
        match self {
            Self::Cons(_, b) => 1 + b.count(),
            Self::Nil(_) => 0,
        }
    }
}

fn cons_list_syntax(
    state_machine: &mut StateMachine,
    handler: &dyn Handler<error::Error>,
) -> parse::Result<ConsList> {
    (
        (KeywordKind::Continue.to_owned(), cons_list_syntax.map(Box::new))
            .map(|(keyword, list)| ConsList::Cons(keyword, list)),
        ';'.to_owned().map(ConsList::Nil),
    )
        .branch()
        .parse(state_machine, handler)
}

#[test]
fn cons_list() {
    let (token_stream, _) =
        create_token_stream("continue continue continue ;".to_string());

    let tree = Tree::new(&token_stream);

    let cons_list =
        cons_list_syntax.parse_syntax(&tree, &pernixc_handler::Panic).unwrap();

    assert_eq!(cons_list.count(), 3);
}

#[test]
fn cons_list_missing_semi_colon() {
    let (token_stream, _) =
        create_token_stream("continue continue continue".to_string());

    let tree = Tree::new(&token_stream);

    let storage = Storage::<error::Error>::new();
    let cons_list = cons_list_syntax.parse_syntax(&tree, &storage);

    assert!(cons_list.is_none());

    let mut errors = storage.into_vec();
    assert_eq!(errors.len(), 1);

    let error = errors.pop().unwrap();

    assert_eq!(error.expected().len(), 2);
    assert!(error.expected().contains(&';'.into()));
    assert!(error.expected().contains(&KeywordKind::Continue.into()));
}

#[test]
fn keepe_take() {
    let (token_stream, _) =
        create_token_stream("continue continue continue ;".to_string());

    let tree = Tree::new(&token_stream);

    let (continues, _) = (KeywordKind::Continue.keep_take(), ';')
        .parse_syntax(&tree, &pernixc_handler::Panic)
        .unwrap();

    assert_eq!(continues.len(), 3);
}

#[test]
fn keep_take_no_skip() {
    let (token_stream, _) = create_token_stream("+++ ++".to_string());

    let tree = Tree::new(&token_stream);

    let pluses = '+'
        .no_skip()
        .keep_take()
        .parse_syntax(&tree, &pernixc_handler::Panic)
        .unwrap();

    assert_eq!(pluses.len(), 3);
}

#[test]
fn step_into() {
    let (token_stream, _) =
        create_token_stream("[continue continue continue]".to_string());

    let tree = Tree::new(&token_stream);

    let (open, continues, close) = KeywordKind::Continue
        .keep_take()
        .step_into(DelimiterKind::Bracket)
        .parse_syntax(&tree, &pernixc_handler::Panic)
        .unwrap();

    assert_eq!(open.punctuation, '[');
    assert_eq!(continues.len(), 3);
    assert_eq!(close.punctuation, ']');
}

#[test]
fn step_into_mismatched_delimiter() {
    let (token_stream, _) =
        create_token_stream("[continue continue continue]".to_string());

    let tree = Tree::new(&token_stream);

    let storage = Storage::<error::Error>::new();
    let result = KeywordKind::Continue
        .keep_take()
        .step_into(DelimiterKind::Brace)
        .parse_syntax(&tree, &storage);

    assert!(result.is_none());

    let mut errors = storage.into_vec();
    assert_eq!(errors.len(), 1);

    let error = errors.pop().unwrap();

    assert_eq!(error.expected().len(), 1);
    assert!(error.expected().contains(&DelimiterKind::Brace.into()));
}

#[test]
fn step_into_dont_take_all() {
    let (token_stream, _) =
        create_token_stream("[continue continue continue;]".to_string());

    let tree = Tree::new(&token_stream);

    let storage = Storage::<error::Error>::new();

    let (open, continues, close) = KeywordKind::Continue
        .keep_take()
        .step_into(DelimiterKind::Bracket)
        .parse_syntax(&tree, &storage)
        .unwrap();

    assert_eq!(open.punctuation, '[');
    assert_eq!(continues.len(), 3);
    assert_eq!(close.punctuation, ']');

    let errors = storage.into_vec();

    assert_eq!(errors.len(), 1);
    let error = errors.first().unwrap();

    assert_eq!(error.expected().len(), 1);
    let expected = error.expected().first().unwrap();

    assert!(matches!(
        error.found(),
        Found::Token(Token::Punctuation(Punctuation { punctuation: ';', .. }))
    ));

    assert_eq!(*expected, ']'.into());
}

#[test]
fn tuple_branch_report_eaten_more() {
    let (token_stream, _) = create_token_stream("continue break ;".to_string());

    let tree = Tree::new(&token_stream);
    let storage = Storage::<error::Error>::new();

    let result = (
        (KeywordKind::Continue, KeywordKind::Break, '!').map(|_| ()),
        (KeywordKind::Continue, '!').map(|_| ()),
    )
        .branch()
        .parse_syntax(&tree, &storage);

    assert!(result.is_none());

    let mut errors = storage.into_vec();

    assert_eq!(errors.len(), 1);

    let error = errors.pop().unwrap();

    assert_eq!(error.expected().len(), 1);
    assert!(error.expected().contains(&'!'.into()));
    assert_eq!(
        error.found().as_token().unwrap().as_punctuation().unwrap().punctuation,
        ';'
    );

    let storage = Storage::<error::Error>::new();

    // swap place
    let result = (
        (KeywordKind::Continue, '!').map(|_| ()),
        (KeywordKind::Continue, KeywordKind::Break, '!').map(|_| ()),
    )
        .branch();

    assert!(result.parse_syntax(&tree, &storage).is_none());

    let mut errors = storage.into_vec();

    assert_eq!(errors.len(), 1);

    let error = errors.pop().unwrap();

    assert_eq!(error.expected().len(), 1);
    assert!(error.expected().contains(&'!'.into()));
    assert_eq!(
        error.found().as_token().unwrap().as_punctuation().unwrap().punctuation,
        ';'
    );
}
