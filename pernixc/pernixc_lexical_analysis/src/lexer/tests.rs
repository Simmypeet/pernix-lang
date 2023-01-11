use pernixc_common::source_file::TextPosition;

use crate::{
    error::LexicalError,
    lexer::Lexer,
    token::{Keyword, LiteralToken, NumberLiteralSuffix, TokenType},
};

#[test]
// Test for the literal constant token correctness
fn literal_test() {
    let mut lexer = Lexer::new("123i32 123.456 true false");

    // expect 123 integer
    assert!({
        let token = lexer.lex().ok().unwrap();

        matches!(
            token.token_type,
            TokenType::Literal(LiteralToken::NumberLiteral {
                value,
                suffix: Some(NumberLiteralSuffix::Int32),
                is_decimal: false
            }) if value == "123"
        ) && token.position_range.start == TextPosition { line: 1, column: 1 }
            && token.position_range.end == TextPosition { line: 1, column: 7 }
    });

    // expect space
    assert!({
        let token = lexer.lex().ok().unwrap();

        matches!(token.token_type, TokenType::Space)
            && token.position_range.start == TextPosition { line: 1, column: 7 }
            && token.position_range.end == TextPosition { line: 1, column: 8 }
    });

    // expect 123.456 decimal
    assert!({
        let token = lexer.lex().ok().unwrap();

        matches!(
            token.token_type,
            TokenType::Literal(LiteralToken::NumberLiteral {
                value,
                suffix: None,
                is_decimal: true
            }
        ) if value == "123.456"
        ) && token.position_range.start == TextPosition { line: 1, column: 8 }
            && token.position_range.end
                == TextPosition {
                    line: 1,
                    column: 15,
                }
    });

    // expect space
    assert!({
        let token = lexer.lex().ok().unwrap();

        matches!(token.token_type, TokenType::Space)
            && token.position_range.start
                == TextPosition {
                    line: 1,
                    column: 15,
                }
            && token.position_range.end
                == TextPosition {
                    line: 1,
                    column: 16,
                }
    });

    // expect true boolean
    assert!({
        let token = lexer.lex().ok().unwrap();

        matches!(
            token.token_type,
            TokenType::Literal(LiteralToken::BoolLiteral(true))
        ) && token.position_range.start
            == TextPosition {
                line: 1,
                column: 16,
            }
            && token.position_range.end
                == TextPosition {
                    line: 1,
                    column: 20,
                }
    });

    // expect space
    assert!({
        let token = lexer.lex().ok().unwrap();

        matches!(token.token_type, TokenType::Space)
            && token.position_range.start
                == TextPosition {
                    line: 1,
                    column: 20,
                }
            && token.position_range.end
                == TextPosition {
                    line: 1,
                    column: 21,
                }
    });

    // expect false boolean
    assert!({
        let token = lexer.lex().ok().unwrap();

        matches!(
            token.token_type,
            TokenType::Literal(LiteralToken::BoolLiteral(false))
        ) && token.position_range.start
            == TextPosition {
                line: 1,
                column: 21,
            }
            && token.position_range.end
                == TextPosition {
                    line: 1,
                    column: 26,
                }
    });
}

#[test]
// Tests for the space token gap correctness
fn space_test() {
    let mut lexer = Lexer::new("  | |\n ");

    assert!({
        let token = lexer.lex().ok().unwrap();

        matches!(token.token_type, TokenType::Space)
            && token.position_range.start == TextPosition { line: 1, column: 1 }
            && token.position_range.end == TextPosition { line: 1, column: 3 }
    });

    assert!({
        let token = lexer.lex().ok().unwrap();

        matches!(token.token_type, TokenType::Punctuation('|'))
            && token.position_range.start == TextPosition { line: 1, column: 3 }
            && token.position_range.end == TextPosition { line: 1, column: 4 }
    });

    assert!({
        let token = lexer.lex().ok().unwrap();

        matches!(token.token_type, TokenType::Space)
            && token.position_range.start == TextPosition { line: 1, column: 4 }
            && token.position_range.end == TextPosition { line: 1, column: 5 }
    });

    assert!({
        let token = lexer.lex().ok().unwrap();

        matches!(token.token_type, TokenType::Punctuation('|'))
            && token.position_range.start == TextPosition { line: 1, column: 5 }
            && token.position_range.end == TextPosition { line: 1, column: 6 }
    });

    assert!({
        let token = lexer.lex().ok().unwrap();

        matches!(token.token_type, TokenType::Space)
            && token.position_range.start == TextPosition { line: 1, column: 6 }
            && token.position_range.end == TextPosition { line: 2, column: 2 }
    });

    assert!(matches!(
        lexer.lex().ok().unwrap().token_type,
        TokenType::EndOfFile
    ));
}

#[test]
// Tests whether the lexer can differentiate between identifiers and keywords
fn identifier_and_keyword_test() {
    let mut lexer = Lexer::new("return some_name _name 23i64");

    assert!(matches!(
        lexer.lex().ok().unwrap().token_type,
        TokenType::Keyword(Keyword::Return)
    ));

    assert!(matches!(
        lexer.lex().ok().unwrap().token_type,
        TokenType::Space
    ));

    assert!({
        let token = lexer.lex().ok().unwrap();
        matches!(token.token_type, TokenType::Identifier(identifier) if identifier == "some_name")
    });

    assert!(matches!(
        lexer.lex().ok().unwrap().token_type,
        TokenType::Space
    ));

    assert!({
        let token = lexer.lex().ok().unwrap();
        matches!(token.token_type, TokenType::Identifier(identifier) if identifier == "_name")
    });

    assert!(matches!(
        lexer.lex().ok().unwrap().token_type,
        TokenType::Space
    ));

    assert!({
        let token = lexer.lex().ok().unwrap();
        matches!(
            token.token_type,
            TokenType::Literal(LiteralToken::NumberLiteral {
                value,
                suffix: Some(NumberLiteralSuffix::Int64),
                is_decimal: false
            })
            if value == "23"
        )
    });

    assert!(matches!(
        lexer.lex().ok().unwrap().token_type,
        TokenType::EndOfFile
    ));
}

#[test]
// Test whether the lexer can correctly lex comment or not
fn comment_test() {
    let mut lexer = Lexer::new("// Hello\n return// Another");

    assert!({
        let token = lexer.lex().ok().unwrap();
        matches!(token.token_type, TokenType::Comment(comment) if comment == "// Hello\n")
    });

    assert!(matches!(
        lexer.lex().ok().unwrap().token_type,
        TokenType::Space
    ));

    assert!(matches!(
        lexer.lex().ok().unwrap().token_type,
        TokenType::Keyword(Keyword::Return)
    ));

    assert!({
        let token = lexer.lex().ok().unwrap();
        matches!(token.token_type, TokenType::Comment(comment) if comment == "// Another")
    });

    assert!(matches!(
        lexer.lex().ok().unwrap().token_type,
        TokenType::EndOfFile
    ));
}

#[test]
// Test whether the lexer can correctly lex multiline comment or not
fn multiline_comment_test() {
    let mut lexer = Lexer::new("/* Hello */ return/* Another */ /* Hello");

    assert!({
        let token = lexer.lex().ok().unwrap();
        matches!(token.token_type, TokenType::Comment(comment) if comment == "/* Hello */")
    });

    assert!(matches!(
        lexer.lex().ok().unwrap().token_type,
        TokenType::Space
    ));

    assert!(matches!(
        lexer.lex().ok().unwrap().token_type,
        TokenType::Keyword(Keyword::Return)
    ));

    assert!({
        let token = lexer.lex().ok().unwrap();
        matches!(
            token.token_type,
            TokenType::Comment(comment) if comment == "/* Another */"
        )
    });

    assert!(matches!(
        lexer.lex().ok().unwrap().token_type,
        TokenType::Space
    ));

    assert!({
        let err = lexer.lex().err().unwrap();

        match err {
            LexicalError::UnterminatedMultilineComment {
                multiline_comment_position,
            } => multiline_comment_position.line == 1 && multiline_comment_position.column == 33,
            _ => false,
        }
    });

    assert!(matches!(
        lexer.lex().ok().unwrap().token_type,
        TokenType::EndOfFile
    ));
}
