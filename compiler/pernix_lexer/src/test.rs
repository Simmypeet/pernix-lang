use pernix_project::source_code::{SourceCode, SourcePosition};

use crate::{
    error::{Error, UnterminatedMultilineComment},
    token::{Keyword, LiteralConstantToken, TokenKind},
    Lexer,
};

#[test]
// Test for the literal constant token correctness
fn literal_test() {
    let source = SourceCode::new(
        "123i32 123.456 true false".to_string(),
        "test".to_string(),
    );
    let mut lexer = Lexer::new(&source);

    // expect 123 integer
    assert!({
        let token = lexer.lex().ok().unwrap();

        matches!(
            token.token_kind(),
            TokenKind::LiteralConstant(LiteralConstantToken::Number {
                value: "123",
                literal_suffix: Some("i32"),
                is_decimal: false
            })
        ) && token.position_range().start
            == SourcePosition {
                line: 1,
                column: 1,
                byte_index: 0,
            }
            && token.position_range().end
                == SourcePosition {
                    line: 1,
                    column: 7,
                    byte_index: 6,
                }
    });

    // expect space
    assert!({
        let token = lexer.lex().ok().unwrap();

        matches!(token.token_kind(), TokenKind::Space)
            && token.position_range().start
                == SourcePosition {
                    line: 1,
                    column: 7,
                    byte_index: 6,
                }
            && token.position_range().end
                == SourcePosition {
                    line: 1,
                    column: 8,
                    byte_index: 7,
                }
    });

    // expect 123.456 float
    assert!({
        let token = lexer.lex().ok().unwrap();

        matches!(
            token.token_kind(),
            TokenKind::LiteralConstant(LiteralConstantToken::Number {
                value: "123.456",
                literal_suffix: None,
                is_decimal: true
            })
        ) && token.position_range().start
            == SourcePosition {
                line: 1,
                column: 8,
                byte_index: 7,
            }
            && token.position_range().end
                == SourcePosition {
                    line: 1,
                    column: 15,
                    byte_index: 14,
                }
    });

    // expect space
    assert!({
        let token = lexer.lex().ok().unwrap();

        matches!(token.token_kind(), TokenKind::Space)
            && token.position_range().start
                == SourcePosition {
                    line: 1,
                    column: 15,
                    byte_index: 14,
                }
            && token.position_range().end
                == SourcePosition {
                    line: 1,
                    column: 16,
                    byte_index: 15,
                }
    });

    // expect true boolean
    assert!({
        let token = lexer.lex().ok().unwrap();

        matches!(
            token.token_kind(),
            TokenKind::LiteralConstant(LiteralConstantToken::Boolean(true))
        ) && token.position_range().start
            == SourcePosition {
                line: 1,
                column: 16,
                byte_index: 15,
            }
            && token.position_range().end
                == SourcePosition {
                    line: 1,
                    column: 20,
                    byte_index: 19,
                }
    });

    // expect space
    assert!({
        let token = lexer.lex().ok().unwrap();

        matches!(token.token_kind(), TokenKind::Space)
            && token.position_range().start
                == SourcePosition {
                    line: 1,
                    column: 20,
                    byte_index: 19,
                }
            && token.position_range().end
                == SourcePosition {
                    line: 1,
                    column: 21,
                    byte_index: 20,
                }
    });

    // expect false boolean
    assert!({
        let token = lexer.lex().ok().unwrap();

        matches!(
            token.token_kind(),
            TokenKind::LiteralConstant(LiteralConstantToken::Boolean(false))
        ) && token.position_range().start
            == SourcePosition {
                line: 1,
                column: 21,
                byte_index: 20,
            }
            && token.position_range().end
                == SourcePosition {
                    line: 1,
                    column: 26,
                    byte_index: 25,
                }
    });
}

#[test]
// Tests for the space token gap correctness
fn space_test() {
    let source = SourceCode::new("  | |\n ".to_string(), "test".to_string());
    let mut lexer = Lexer::new(&source);

    assert!({
        let token = lexer.lex().ok().unwrap();

        matches!(token.token_kind(), TokenKind::Space)
            && token.position_range().start
                == SourcePosition {
                    line: 1,
                    column: 1,
                    byte_index: 0,
                }
            && token.position_range().end
                == SourcePosition {
                    line: 1,
                    column: 3,
                    byte_index: 2,
                }
    });

    assert!({
        let token = lexer.lex().ok().unwrap();

        matches!(token.token_kind(), TokenKind::Punctuator('|'))
            && token.position_range().start
                == SourcePosition {
                    line: 1,
                    column: 3,
                    byte_index: 2,
                }
            && token.position_range().end
                == SourcePosition {
                    line: 1,
                    column: 4,
                    byte_index: 3,
                }
    });

    assert!({
        let token = lexer.lex().ok().unwrap();

        matches!(token.token_kind(), TokenKind::Space)
            && token.position_range().start
                == SourcePosition {
                    line: 1,
                    column: 4,
                    byte_index: 3,
                }
            && token.position_range().end
                == SourcePosition {
                    line: 1,
                    column: 5,
                    byte_index: 4,
                }
    });

    assert!({
        let token = lexer.lex().ok().unwrap();

        matches!(token.token_kind(), TokenKind::Punctuator('|'))
            && token.position_range().start
                == SourcePosition {
                    line: 1,
                    column: 5,
                    byte_index: 4,
                }
            && token.position_range().end
                == SourcePosition {
                    line: 1,
                    column: 6,
                    byte_index: 5,
                }
    });

    assert!({
        let token = lexer.lex().ok().unwrap();

        matches!(token.token_kind(), TokenKind::Space)
            && token.position_range().start
                == SourcePosition {
                    line: 1,
                    column: 6,
                    byte_index: 5,
                }
            && token.position_range().end
                == SourcePosition {
                    line: 2,
                    column: 2,
                    byte_index: 7,
                }
    });

    assert!(matches!(
        lexer.lex().ok().unwrap().token_kind(),
        TokenKind::EndOfFile
    ));
}

#[test]
// Tests whether the lexer can differentiate between identifiers and keywords
fn identifier_and_keyword_test() {
    let source = SourceCode::new(
        "return some_name _name 23name".to_string(),
        "test".to_string(),
    );
    let mut lexer = Lexer::new(&source);

    assert!(matches!(
        lexer.lex().ok().unwrap().token_kind(),
        TokenKind::Keyword(Keyword::Return)
    ));

    assert!(matches!(
        lexer.lex().ok().unwrap().token_kind(),
        TokenKind::Space
    ));

    assert!({
        let token = lexer.lex().ok().unwrap();
        token.lexeme() == "some_name"
            && matches!(token.token_kind(), TokenKind::Identifier)
    });

    assert!(matches!(
        lexer.lex().ok().unwrap().token_kind(),
        TokenKind::Space
    ));

    assert!({
        let token = lexer.lex().ok().unwrap();
        token.lexeme() == "_name"
            && matches!(token.token_kind(), TokenKind::Identifier)
    });

    assert!(matches!(
        lexer.lex().ok().unwrap().token_kind(),
        TokenKind::Space
    ));

    assert!({
        let token = lexer.lex().ok().unwrap();
        token.lexeme() == "23name"
            && matches!(
                token.token_kind(),
                TokenKind::LiteralConstant(LiteralConstantToken::Number {
                    value: "23",
                    literal_suffix: Some("name"),
                    is_decimal: false
                })
            )
    });

    assert!(matches!(
        lexer.lex().ok().unwrap().token_kind(),
        TokenKind::EndOfFile
    ));
}

#[test]
// Test whether the lexer can correctly lex comment or not
fn comment_test() {
    let source = SourceCode::new(
        "// Hello\n return// Another".to_string(),
        "test".to_string(),
    );
    let mut lexer = Lexer::new(&source);

    assert!({
        let token = lexer.lex().ok().unwrap();
        token.lexeme() == "// Hello\n"
            && matches!(token.token_kind(), TokenKind::Comment)
    });

    assert!(matches!(
        lexer.lex().ok().unwrap().token_kind(),
        TokenKind::Space
    ));

    assert!(matches!(
        lexer.lex().ok().unwrap().token_kind(),
        TokenKind::Keyword(Keyword::Return)
    ));

    assert!({
        let token = lexer.lex().ok().unwrap();
        token.lexeme() == "// Another"
            && matches!(token.token_kind(), TokenKind::Comment)
    });

    assert!(matches!(
        lexer.lex().ok().unwrap().token_kind(),
        TokenKind::EndOfFile
    ));
}

#[test]
// Test whether the lexer can correctly lex multiline comment or not
fn multiline_comment_test() {
    let source = SourceCode::new(
        "/* Hello */ return/* Another */ /* Hello".to_string(),
        "test".to_string(),
    );
    let mut lexer = Lexer::new(&source);

    assert!({
        let token = lexer.lex().ok().unwrap();
        token.lexeme() == "/* Hello */"
            && matches!(token.token_kind(), TokenKind::Comment)
    });

    assert!(matches!(
        lexer.lex().ok().unwrap().token_kind(),
        TokenKind::Space
    ));

    assert!(matches!(
        lexer.lex().ok().unwrap().token_kind(),
        TokenKind::Keyword(Keyword::Return)
    ));

    assert!({
        let token = lexer.lex().ok().unwrap();
        token.lexeme() == "/* Another */"
            && matches!(token.token_kind(), TokenKind::Comment)
    });

    assert!(matches!(
        lexer.lex().ok().unwrap().token_kind(),
        TokenKind::Space
    ));

    assert!({
        let err = lexer.lex().err().unwrap();

        match err {
            Error::UnterminatedMultilineComment(
                UnterminatedMultilineComment {
                    multiline_comment_position,
                },
            ) => {
                multiline_comment_position.line == 1
                    && multiline_comment_position.column == 33
            }
            _ => false,
        }
    });

    assert!(matches!(
        lexer.lex().ok().unwrap().token_kind(),
        TokenKind::EndOfFile
    ));
}
