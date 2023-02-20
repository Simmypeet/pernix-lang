use std::path::PathBuf;

use pernixc_common::source_file::SourceFile;

use crate::token::{CommentKind, Keyword, Token, TokenTrait, TokenizationError};

#[test]
fn token_test() -> Result<(), Box<dyn std::error::Error>> {
    let source_file = SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("src")
            .join("token")
            .join("tokenTest.pnx"),
    )?;

    let mut iter = source_file.iter().peekable();

    assert!({
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_identifier()
            .copied()
            .unwrap();
        token.lexeme() == "helloWorld"
    });

    assert!(Token::tokenize(&mut iter)
        .unwrap()
        .as_white_space()
        .is_some());

    assert!({
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_identifier()
            .copied()
            .unwrap();
        token.lexeme() == "@Test"
    });

    assert!(Token::tokenize(&mut iter)
        .unwrap()
        .as_white_space()
        .is_some());

    assert!({
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_identifier()
            .copied()
            .unwrap();
        token.lexeme() == "_1"
    });

    assert!(Token::tokenize(&mut iter)
        .unwrap()
        .as_white_space()
        .is_some());

    assert!({
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_keyword()
            .copied()
            .unwrap();
        token.lexeme() == "public" && *token.keyword() == Keyword::Public
    });

    assert!(Token::tokenize(&mut iter)
        .unwrap()
        .as_white_space()
        .is_some());

    assert!({
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_keyword()
            .copied()
            .unwrap();
        token.lexeme() == "void" && *token.keyword() == Keyword::Void
    });

    assert!(Token::tokenize(&mut iter)
        .unwrap()
        .as_white_space()
        .is_some());

    assert!({
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_comment()
            .copied()
            .unwrap();
        token.lexeme() == "// Test\n" && *token.kind() == CommentKind::SingleLine
    });

    assert!({
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_comment()
            .copied()
            .unwrap();
        token.lexeme() == "/*Multi\nLine*/" && *token.kind() == CommentKind::Delimited
    });

    assert!(Token::tokenize(&mut iter)
        .unwrap()
        .as_white_space()
        .is_some());

    assert!({
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_numeric_literal()
            .copied()
            .unwrap();
        token.lexeme() == "1231"
            && token.value_span().string() == "1231"
            && token.suffix_span().is_none()
    });

    assert!(Token::tokenize(&mut iter)
        .unwrap()
        .as_white_space()
        .is_some());

    assert!({
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_numeric_literal()
            .copied()
            .unwrap();
        token.lexeme() == "12.42f32"
            && token.value_span().string() == "12.42"
            && token.suffix_span().unwrap().string() == "f32"
    });

    assert!(Token::tokenize(&mut iter)
        .unwrap()
        .as_white_space()
        .is_some());

    assert!({
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_numeric_literal()
            .copied()
            .unwrap();
        token.lexeme() == "123"
            && token.value_span().string() == "123"
            && token.suffix_span().is_none()
    });

    assert!({
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_punctuation()
            .copied()
            .unwrap();
        token.lexeme() == "." && token.punctuation == '.'
    });

    assert!({
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_identifier()
            .copied()
            .unwrap();
        token.lexeme() == "f32"
    });

    assert!(Token::tokenize(&mut iter)
        .unwrap()
        .as_white_space()
        .is_some());

    assert!({
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_character_literal()
            .copied()
            .unwrap();
        token.lexeme() == "'a'" && token.character == 'a'
    });

    assert!(Token::tokenize(&mut iter)
        .unwrap()
        .as_white_space()
        .is_some());

    assert!({
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_punctuation()
            .copied()
            .unwrap();
        token.lexeme() == "'" && token.punctuation == '\''
    });

    assert!({
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_identifier()
            .copied()
            .unwrap();
        token.lexeme() == "ab"
    });

    assert!({
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_punctuation()
            .copied()
            .unwrap();
        token.lexeme() == "'" && token.punctuation == '\''
    });

    assert!(Token::tokenize(&mut iter)
        .unwrap()
        .as_white_space()
        .is_some());

    assert!({
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_character_literal()
            .copied()
            .unwrap();
        token.lexeme() == "'\\0'" && token.character == '\0'
    });

    assert!(Token::tokenize(&mut iter)
        .unwrap()
        .as_white_space()
        .is_some());

    assert!({
        let err = Token::tokenize(&mut iter).err().unwrap();
        let err = err
            .as_lexical()
            .unwrap()
            .as_invalid_escape_character_sequences()
            .unwrap();
        err[0].string() == "1" && err.len() == 1
    });

    assert!(Token::tokenize(&mut iter)
        .unwrap()
        .as_white_space()
        .is_some());

    assert!({
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_string_literal()
            .copied()
            .unwrap();
        token.lexeme() == "\"Hello, world!\""
    });

    assert!(Token::tokenize(&mut iter)
        .unwrap()
        .as_white_space()
        .is_some());

    assert!({
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_string_literal()
            .copied()
            .unwrap();
        token.lexeme() == "\"\\\"With escape sequences\\\"\""
    });

    assert!(Token::tokenize(&mut iter)
        .unwrap()
        .as_white_space()
        .is_some());

    assert!({
        let err = Token::tokenize(&mut iter).err().unwrap();
        let err = err
            .as_lexical()
            .unwrap()
            .as_invalid_escape_character_sequences()
            .unwrap();
        err[0].string() == "1" && err[1].string() == "2" && err[2].string() == "3" && err.len() == 3
    });

    assert!({
        let error = Token::tokenize(&mut iter).err().unwrap();
        matches!(error, TokenizationError::EndOfSourceCodeIteratorArgument)
    });

    Ok(())
}
