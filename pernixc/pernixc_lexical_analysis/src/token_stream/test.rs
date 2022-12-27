use pernixc_common::source_file::SourceFile;

use crate::token::{Keyword, TokenKind};

use super::TokenStream;

static SOURCE_FILE: &str = "void main() {
    return 0;
}";

#[test]
fn token_stream_test() {
    let source_file =
        SourceFile::new(SOURCE_FILE.to_string(), "test.pnx".to_string());
    let token_stream = TokenStream::tokenize(&source_file).unwrap();

    // create an iterator that ignores the whitespace and comments
    let mut iter = token_stream.iter().filter(|token| {
        !matches!(token.token_kind, TokenKind::Comment | TokenKind::Space)
    });

    assert!(matches!(
        iter.next().unwrap().token_kind,
        TokenKind::Identifier
    ));
    assert!(matches!(
        iter.next().unwrap().token_kind,
        TokenKind::Identifier
    ));
    assert!(matches!(
        iter.next().unwrap().token_kind,
        TokenKind::Punctuator('(')
    ));
    assert!(matches!(
        iter.next().unwrap().token_kind,
        TokenKind::Punctuator(')')
    ));
    assert!(matches!(
        iter.next().unwrap().token_kind,
        TokenKind::Punctuator('{')
    ));
    assert!(matches!(
        iter.next().unwrap().token_kind,
        TokenKind::Keyword(Keyword::Return)
    ));
    assert!(matches!(
        iter.next().unwrap().token_kind,
        TokenKind::LiteralConstant(_)
    ));
    assert!(matches!(
        iter.next().unwrap().token_kind,
        TokenKind::Punctuator(';')
    ));
    assert!(matches!(
        iter.next().unwrap().token_kind,
        TokenKind::Punctuator('}')
    ));
    assert!(matches!(
        iter.next().unwrap().token_kind,
        TokenKind::EndOfFile
    ));
    assert!(iter.next().is_none());
}
