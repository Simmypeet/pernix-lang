use crate::token::{Keyword, TokenType};

use super::TokenStream;

static SOURCE_CODE: &str = "void main() {
    return 0;
}";

#[test]
fn token_stream_test() {
    let token_stream = TokenStream::tokenize(SOURCE_CODE).0;

    // create an iterator that ignores the whitespace and comments
    let mut iter = token_stream
        .iter()
        .filter(|token| !matches!(token.token_type, TokenType::Comment(_) | TokenType::Space));

    assert!(matches!(
        iter.next().unwrap().token_type,
        TokenType::Keyword(Keyword::Void)
    ));
    assert!(matches!(
        iter.next().unwrap().token_type,
        TokenType::Identifier(identifier)
        if identifier == "main"
    ));
    assert!(matches!(
        iter.next().unwrap().token_type,
        TokenType::Punctuation('(')
    ));
    assert!(matches!(
        iter.next().unwrap().token_type,
        TokenType::Punctuation(')')
    ));
    assert!(matches!(
        iter.next().unwrap().token_type,
        TokenType::Punctuation('{')
    ));
    assert!(matches!(
        iter.next().unwrap().token_type,
        TokenType::Keyword(Keyword::Return)
    ));
    assert!(matches!(
        iter.next().unwrap().token_type,
        TokenType::Literal(_)
    ));
    assert!(matches!(
        iter.next().unwrap().token_type,
        TokenType::Punctuation(';')
    ));
    assert!(matches!(
        iter.next().unwrap().token_type,
        TokenType::Punctuation('}')
    ));
    assert!(matches!(
        iter.next().unwrap().token_type,
        TokenType::EndOfFile
    ));
    assert!(iter.next().is_none());
}
