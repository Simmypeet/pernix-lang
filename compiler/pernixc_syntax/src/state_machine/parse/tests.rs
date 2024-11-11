use std::sync::Arc;

use pernixc_base::{
    handler::{Handler, Panic, Storage},
    source_file::SourceFile,
};
use pernixc_lexical::{
    token::{Keyword, KeywordKind, Punctuation, Token},
    token_stream::{Delimiter, TokenStream, Tree},
};

use super::ExpectExt;
use crate::{
    error::{self, Found},
    state_machine::{
        parse::{self, ExpectedIterator, Parse},
        StateMachine,
    },
};

fn create_token_stream(source: String) -> (TokenStream, Arc<SourceFile>) {
    let source_file = Arc::new(SourceFile::new(source, "test".into()));
    let token_stream = TokenStream::tokenize(source_file.clone(), &Panic);

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
            ConsList::Cons(_, b) => 1 + b.count(),
            ConsList::Nil(_) => 0,
        }
    }
}

fn cons_list_syntax<'a>(
    state_machine: &mut StateMachine<'a>,
    handler: &dyn Handler<error::Error>,
) -> parse::Result<'a, ConsList, impl ExpectedIterator> {
    (KeywordKind::Continue.to_owned(), cons_list_syntax.map(Box::new).boxed())
        .map(|(keyword, list)| ConsList::Cons(keyword, list))
        .or_else(';'.to_owned().map(ConsList::Nil))
        .parse(state_machine, handler)
}

#[test]
fn cons_list() {
    let (token_stream, _) =
        create_token_stream("continue continue continue ;".to_string());

    let tree = Tree::new(&token_stream);
    let mut state_machine = StateMachine::new(&tree);

    let cons_list = cons_list_syntax(&mut state_machine, &Panic).unwrap();

    assert_eq!(cons_list.count(), 3);
}

#[test]
fn cons_list_missing_semi_colon() {
    let (token_stream, _) =
        create_token_stream("continue continue continue".to_string());

    let tree = Tree::new(&token_stream);
    let mut state_machine = StateMachine::new(&tree);

    let cons_list = cons_list_syntax(&mut state_machine, &Panic);
    let error = cons_list.unwrap_err();

    assert!(error.found.is_none());

    let expecteds = error.expected.collect::<Vec<_>>();

    assert_eq!(expecteds.len(), 2);
    assert!(expecteds.contains(&';'.into()));
    assert!(expecteds.contains(&KeywordKind::Continue.into()));
}

#[test]
fn keepe_take() {
    let (token_stream, _) =
        create_token_stream("continue continue continue ;".to_string());

    let tree = Tree::new(&token_stream);
    let mut state_machine = StateMachine::new(&tree);

    let (continues, _) = (KeywordKind::Continue.keep_take(), ';')
        .parse(&mut state_machine, &Panic)
        .unwrap();

    assert_eq!(continues.len(), 3);
}

#[test]
fn keep_take_no_skip() {
    let (token_stream, _) = create_token_stream("+++ ++".to_string());

    let tree = Tree::new(&token_stream);
    let mut state_machine = StateMachine::new(&tree);

    let pluses =
        '+'.no_skip().keep_take().parse(&mut state_machine, &Panic).unwrap();

    assert_eq!(pluses.len(), 3);
}

#[test]
fn step_into() {
    let (token_stream, _) =
        create_token_stream("[continue continue continue]".to_string());

    let tree = Tree::new(&token_stream);
    let mut state_machine = StateMachine::new(&tree);

    let (open, continues, close) = KeywordKind::Continue
        .keep_take()
        .step_into(Delimiter::Bracket)
        .parse(&mut state_machine, &Panic)
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
    let mut state_machine = StateMachine::new(&tree);

    let result = KeywordKind::Continue
        .keep_take()
        .step_into(Delimiter::Brace)
        .parse(&mut state_machine, &Panic);

    let error = result.unwrap_err();

    let expected = error.expected.collect::<Vec<_>>();
    assert_eq!(expected.len(), 1);
    assert!(expected.contains(&Delimiter::Brace.into()))
}

#[test]
fn step_into_dont_take_all() {
    let (token_stream, _) =
        create_token_stream("[continue continue continue;]".to_string());

    let tree = Tree::new(&token_stream);
    let mut state_machine = StateMachine::new(&tree);

    let storage = Storage::<error::Error>::new();

    let (open, continues, close) = KeywordKind::Continue
        .keep_take()
        .step_into(Delimiter::Bracket)
        .parse(&mut state_machine, &storage)
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
