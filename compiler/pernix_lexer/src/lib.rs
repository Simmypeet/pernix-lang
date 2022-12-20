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
        map.insert("let", Keyword::Let);
        map.insert("using", Keyword::Using);
        map.insert("namespace", Keyword::Namespace);
        map.insert("mutable", Keyword::Mutable);
        map.insert("if", Keyword::If);
        map.insert("else", Keyword::Else);
        map.insert("while", Keyword::While);
        map.insert("break", Keyword::Break);
        map.insert("continue", Keyword::Continue);
        map
    };
}

/// Represent a state-machine that lexes the source code and outputs a token
/// one at a time.
pub struct Lexer<'a> {
    source_code: &'a SourceCode,
    chars: Peekable<CharIndices<'a>>,
    current_position: SourcePosition,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer targeting to the given `source_code` char slice
    pub fn new(source_code: &'a SourceCode) -> Lexer<'a> {
        Lexer {
            source_code,
            chars: source_code.source_code().char_indices().peekable(),
            current_position: SourcePosition {
                column: 1,
                line: 1,
                byte_index: 0,
            },
        }
    }

    // return the current character and the its byte index , and moves to the
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

                self.current_position.byte_index = {
                    self.chars
                        .peek() // peek the next character
                        .map(|next_char| next_char.0) // get the byte index of the next character
                        .unwrap_or(self.source_code.source_code().len()) // if there is no next character, get the length of the source code
                };
            }
            _ => {}
        }
        next
    }

    // check if the character can be an identifier character
    fn is_identifier_character(char: char) -> bool {
        return char == '_' || char.is_alphanumeric();
    }

    // the first character in identifier can't be a number
    fn is_first_identifier_character(char: char) -> bool {
        return char == '_' || char.is_alphabetic();
    }

    /// Lex the current word; returns the corresponding token and move to the
    /// next token.
    pub fn lex(&mut self) -> Result<Token<'a>, Error> {
        // the current source file position
        let first_position = self.current_position;

        let first_char;
        let begin_index;
        let token_kind;

        match self.next() {
            None => {
                return Ok(Token::new(
                    TokenKind::EndOfFile,
                    first_position..SourcePosition {
                        line: first_position.line,
                        column: first_position.column,
                        byte_index: self.source_code.source_code().len(),
                    },
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

            match identifier_string.as_str() {
                "true" => {
                    token_kind = TokenKind::LiteralConstant(
                        token::LiteralConstantToken::Boolean(true),
                    )
                }
                "false" => {
                    token_kind = TokenKind::LiteralConstant(
                        token::LiteralConstantToken::Boolean(false),
                    )
                }
                _ => {
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
            }
        }
        // found punctuator
        else if first_char.is_ascii_punctuation() {
            token_kind = TokenKind::Punctuator(first_char);
        }
        // found a number literal
        else if first_char.is_digit(10) {
            // loop until non-digit character is found
            while {
                match self.chars.peek() {
                    None => false,
                    Some((_, char)) => char.is_digit(10),
                }
            } {
                self.next();
            }

            let mut is_decimal = false;

            // if found a dot, it might be a float literal if the next character
            // after the dot is a digit
            if match self.chars.peek() {
                Some((_, '.')) => true,
                _ => false,
            } {
                // eat the dot and check if the next character is a digit
                // if it is, it is a float literal. if not, move back to the
                // dot position
                let dot_chars = self.chars.clone();
                let dot_current_position = self.current_position;

                // eat dot
                self.next();

                match self.chars.peek() {
                    Some((_, char)) => {
                        if char.is_digit(10) {
                            is_decimal = true;
                            self.next();

                            // loop until non-digit character is found
                            while {
                                match self.chars.peek() {
                                    None => false,
                                    Some((_, char)) => char.is_digit(10),
                                }
                            } {
                                self.next();
                            }
                        } else {
                            self.chars = dot_chars;
                            self.current_position = dot_current_position;
                        }
                    }
                    None => {
                        self.chars = dot_chars;
                        self.current_position = dot_current_position;
                    }
                }
            }

            let value_str = {
                match self.chars.peek() {
                    Some((index, _)) => {
                        &self.source_code.source_code()[begin_index..*index]
                    }
                    None => &self.source_code.source_code()[begin_index..],
                }
            };

            let mut literal_prefix = None;

            if match self.chars.peek() {
                Some((_, char)) => Self::is_first_identifier_character(*char),
                _ => false,
            } {
                let prefix_starting_index = self.chars.peek().unwrap().0;

                // loop until non-identifier character is found
                while {
                    match self.chars.peek() {
                        None => false,
                        Some((_, char)) => Self::is_identifier_character(*char),
                    }
                } {
                    self.next();
                }

                literal_prefix = Some(match self.chars.peek() {
                    Some((index, _)) => &self.source_code.source_code()
                        [prefix_starting_index..*index],
                    None => {
                        &self.source_code.source_code()[prefix_starting_index..]
                    }
                });
            }

            token_kind = TokenKind::LiteralConstant(
                token::LiteralConstantToken::Number {
                    value: value_str,
                    literal_suffix: literal_prefix,
                    is_decimal,
                },
            )
        }
        // this character can't be categorized under any token kinds
        else {
            return Err(Error::InvalidCharacter {
                position: first_position,
                character: first_char,
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

    /// Return a reference to the source code of this [`Lexer`].
    pub fn source_code(&self) -> &'a SourceCode {
        self.source_code
    }
}

#[cfg(test)]
mod test;
