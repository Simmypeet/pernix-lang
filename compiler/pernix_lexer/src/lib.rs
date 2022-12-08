pub mod error;
pub mod token;

use error::Error;
use lazy_static::lazy_static;
use pernix_project::source_code::{SourceCode, SourcePosition};
use std::{collections::HashMap, iter::Peekable, str::CharIndices};
use token::{Keyword, Token, TokenKind};

lazy_static! {
    static ref KEYWORD: HashMap<&'static str, Keyword> = {
        let mut map = HashMap::new();
        map.insert("return", Keyword::Return);
        map
    };
}

/// Represents a lexer in lexical analysis which transforms source code into a
/// token stream
pub struct Lexer<'a> {
    source_code: &'a SourceCode,
    chars: Peekable<CharIndices<'a>>,
    current_position: SourcePosition,
}

impl<'a> Lexer<'a> {
    /// Creates a new lexer targeting to the given `source_code` char slice
    pub fn new(source_code: &'a SourceCode) -> Lexer<'a> {
        Lexer {
            source_code,
            chars: source_code.source_code().char_indices().peekable(),
            current_position: SourcePosition { column: 1, line: 1 },
        }
    }

    // returns the current character and the its byte index , and moves to the
    // next character
    fn next(&mut self) -> Option<(usize, char)> {
        let next = self.chars.next();

        // increment to the next position
        match next {
            Some(char) => {
                if char.1 == '\n' {
                    self.current_position.line += 1;
                    self.current_position.column = 1;
                } else {
                    self.current_position.column += 1;
                }
            }
            _ => {}
        }
        next
    }

    // checks if the character can be an identifier character
    fn is_identifier_character(char: char) -> bool {
        return char == '_' || char.is_alphanumeric();
    }

    // the first character in identifier can't be a number
    fn is_first_identifier_character(char: char) -> bool {
        return char == '_' || char.is_alphabetic();
    }

    /// Lexes the current word; returns the corresponding token and moves to
    /// the next token.
    pub fn lex(&mut self) -> Result<Token<'a>, Error<'a>> {
        // the current source file position
        let first_position = self.current_position;

        let first_char;
        let begin_index;
        let token_kind;

        match self.next() {
            None => {
                return Ok(Token::new(
                    TokenKind::EndOfFile,
                    first_position..self.current_position,
                    "",
                ))
            }
            Some((index, char)) => {
                first_char = char;
                begin_index = index;
            }
        }

        // found space token
        if first_char.is_whitespace() {
            // loop until non-space character is found
            while {
                match self.chars.peek() {
                    None => false,
                    Some((_, char)) => char.is_whitespace(),
                }
            } {
                self.next();
            }

            token_kind = TokenKind::Space;
        }
        // might found a comment
        else if first_char == '/' {
            match self.chars.peek() {
                // found a single line comment
                Some((_, '/')) => {
                    // eat the another '/'
                    self.next();

                    loop {
                        match self.chars.peek() {
                            Some((_, '\n')) => {
                                self.next();
                                token_kind = TokenKind::Comment;
                                break;
                            }
                            None => {
                                token_kind = TokenKind::Comment;
                                break;
                            }
                            _ => {
                                self.next();
                            }
                        }
                    }
                }
                // found a multi-line comment
                Some((_, '*')) => {
                    // eat the '*'
                    self.next();

                    loop {
                        match self.chars.peek() {
                            Some((_, '*')) => {
                                self.next();
                                match self.chars.peek() {
                                    Some((_, '/')) => {
                                        self.next();
                                        token_kind = TokenKind::Comment;
                                        break;
                                    }
                                    _ => {}
                                }
                            }
                            None => {
                                // error: multi-line comment is not closed
                                return Err(
                                    Error::UnterminatedMultilineComment {
                                        multiline_comment_position:
                                            first_position,
                                        source_refernece: self.source_code,
                                    },
                                );
                            }
                            _ => {
                                self.next();
                            }
                        }
                    }
                }
                _ => {
                    token_kind = TokenKind::Punctuator('/');
                }
            }
        }
        // found identifier
        else if Self::is_first_identifier_character(first_char) {
            // string for the identifier
            let mut identifier_string = String::new();
            identifier_string.push(first_char);

            // loop until non-identifier character is found
            while {
                match self.chars.peek() {
                    None => false,
                    Some((_, char)) => Self::is_identifier_character(*char),
                }
            } {
                identifier_string.push(self.chars.peek().unwrap().1);
                self.next();
            }

            // check if the identifier_string matches to any keywords
            match KEYWORD.get(identifier_string.as_str()) {
                // is identifier
                None => token_kind = TokenKind::Identifier,

                // is keyword
                Some(keyword) => {
                    token_kind = TokenKind::Keyword(*keyword);
                }
            }
        }
        // found punctuator
        else if first_char.is_ascii_punctuation() {
            token_kind = TokenKind::Punctuator(first_char);
        }
        // found a number literal
        else if first_char.is_digit(10) {
            // string for the number literal
            let mut number_literal_string = String::new();
            number_literal_string.push(first_char);

            // loop until non-digit character is found
            while {
                match self.chars.peek() {
                    None => false,
                    Some((_, char)) => char.is_digit(10),
                }
            } {
                number_literal_string.push(self.chars.peek().unwrap().1);
                self.next();
            }
            token_kind = TokenKind::LiteralConstant;
        }
        // this character can't be categorized under any token kinds
        else {
            return Err(Error::InvalidCharacter {
                position: first_position,
                character: first_char,
                source_refernece: self.source_code,
            });
        }

        // return the token
        return Ok(Token::new(
            token_kind,
            first_position..self.current_position,
            {
                match self.chars.peek() {
                    Some((index, _)) => {
                        &self.source_code.source_code()[begin_index..*index]
                    }
                    None => &self.source_code.source_code()[begin_index..],
                }
            },
        ));
    }
}

#[cfg(test)]
mod test {
    use pernix_project::source_code::{SourceCode, SourcePosition};

    use crate::{
        error::Error,
        token::{Keyword, TokenKind},
        Lexer,
    };

    #[test]
    // Tests for the space token gap correctness
    fn space_test() {
        let source =
            SourceCode::new("  | |\n ".to_string(), "test".to_string());
        let mut lexer = Lexer::new(&source);

        assert!({
            let token = lexer.lex().ok().unwrap();

            matches!(token.token_kind(), TokenKind::Space)
                && token.position_range().start
                    == SourcePosition { line: 1, column: 1 }
                && token.position_range().end
                    == SourcePosition { line: 1, column: 3 }
        });

        assert!({
            let token = lexer.lex().ok().unwrap();

            matches!(token.token_kind(), TokenKind::Punctuator('|'))
                && token.position_range().start
                    == SourcePosition { line: 1, column: 3 }
                && token.position_range().end
                    == SourcePosition { line: 1, column: 4 }
        });

        assert!({
            let token = lexer.lex().ok().unwrap();

            matches!(token.token_kind(), TokenKind::Space)
                && token.position_range().start
                    == SourcePosition { line: 1, column: 4 }
                && token.position_range().end
                    == SourcePosition { line: 1, column: 5 }
        });

        assert!({
            let token = lexer.lex().ok().unwrap();

            matches!(token.token_kind(), TokenKind::Punctuator('|'))
                && token.position_range().start
                    == SourcePosition { line: 1, column: 5 }
                && token.position_range().end
                    == SourcePosition { line: 1, column: 6 }
        });

        assert!({
            let token = lexer.lex().ok().unwrap();

            matches!(token.token_kind(), TokenKind::Space)
                && token.position_range().start
                    == SourcePosition { line: 1, column: 6 }
                && token.position_range().end
                    == SourcePosition { line: 2, column: 2 }
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
            token.lexeme() == "23"
                && matches!(token.token_kind(), TokenKind::LiteralConstant)
        });

        assert!({
            let token = lexer.lex().ok().unwrap();
            token.lexeme() == "name"
                && matches!(token.token_kind(), TokenKind::Identifier)
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
                Error::UnterminatedMultilineComment {
                    multiline_comment_position,
                    source_refernece: _,
                } => {
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
}
