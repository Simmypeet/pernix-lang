use std::sync::Arc;

use pernixc_base::{
    handler::{Panic, Storage},
    source_file::SourceFile,
};
use pernixc_lexical::{
    token::{Keyword, KeywordKind, Punctuation},
    token_stream::TokenStream,
};

use super::Syntax;
use crate::{
    error::{self, SyntaxKind},
    parser::{syntax::With, Parser},
};

fn create_token_stream(source: String) -> (TokenStream, Arc<SourceFile>) {
    let source_file = Arc::new(SourceFile::new(source, "test".into()));
    let token_stream = TokenStream::tokenize(&source_file, &Panic);
    (token_stream, source_file)
}

#[test]
fn all_or_else_are_none() {
    let (token_stream, source_file) = create_token_stream("".to_string());
    let mut parser = Parser::new(&token_stream, source_file);

    let syntax = ':'.or_none().or_else(';'.or_none()).or_else('?'.or_none());

    assert_eq!(parser.parse(syntax, &Panic).unwrap(), None);
}

#[test]
fn expected_token_after_or_none() {
    let (token_stream, source_file) = create_token_stream("".to_string());
    let mut parser = Parser::new(&token_stream, source_file);

    let syntax = (':'.or_none(), ';'.or_none(), '?', '+'.or_none());

    let storage = Storage::<error::Error>::new();
    assert!(parser.parse(syntax, &storage).is_err());

    let mut errors = storage.into_vec();
    assert_eq!(errors.len(), 1);

    let error = errors.pop().unwrap();

    assert_eq!(error.expected_syntaxes.len(), 1);
    assert_eq!(error.expected_syntaxes[0], SyntaxKind::Punctuation('?'));
}

#[test]
fn or_else_with_condition_and_no_condition() {
    let (token_stream, source_file) = create_token_stream("x".to_string());
    let mut parser = Parser::new(&token_stream, source_file);

    let first_arm =
        (':'.or_none(), '?'.or_none(), ';', '?'.or_none()).map(|_| ());
    let second_arm = ('+'.or_none(), '-').map(|_| ());
    let third_arm = '<'.map(|_| ());

    let syntax = first_arm.or_else(second_arm).or_else(third_arm);

    let storage = Storage::<error::Error>::new();

    assert!(parser.parse(syntax, &storage).is_err());

    let mut errors = storage.into_vec();
    assert_eq!(errors.len(), 1);

    let error = errors.pop().unwrap();

    assert_eq!(error.expected_syntaxes.len(), 3);

    assert!(error.expected_syntaxes.contains(&SyntaxKind::Punctuation(';')));
    assert!(error.expected_syntaxes.contains(&SyntaxKind::Punctuation('-')));
    assert!(error.expected_syntaxes.contains(&SyntaxKind::Punctuation('<')));
}

#[test]
fn loop_while() {
    let (token_stream, source_file) =
        create_token_stream("! ! ! ! ! ;".to_string());

    let mut parser = Parser::new(&token_stream, source_file);

    let syntax = '!'.loop_while();
    let result = parser.parse(syntax, &Panic).unwrap();

    assert_eq!(result.len(), 5);

    for (i, token) in result.into_iter().enumerate() {
        assert_eq!(token.punctuation, '!');
        assert_eq!(token.span.start(), i * 2);
        assert_eq!(token.span.str(), "!");
    }
}

#[test]
fn cons_list() {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    enum ConsList {
        Cons(Keyword, Box<ConsList>),
        Nil(Punctuation),
    }

    fn cons_list_count(c: &ConsList) -> usize {
        match c {
            ConsList::Cons(_, b) => 1 + cons_list_count(b),
            ConsList::Nil(_) => 0,
        }
    }

    fn cons_list() -> impl Syntax<Output = ConsList> {
        (KeywordKind::Continue, With(cons_list))
            .map(|(a, b)| ConsList::Cons(a, Box::new(b)))
            .or_else(';'.map(ConsList::Nil))
    }

    let (token_stream, source_file) =
        create_token_stream("continue continue continue ;".to_string());

    let mut parser = Parser::new(&token_stream, source_file);
    let cons_list = parser.parse(cons_list(), &Panic).unwrap();

    assert_eq!(cons_list_count(&cons_list), 3);
}
