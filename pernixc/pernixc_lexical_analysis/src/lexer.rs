use std::{iter::Peekable, str::CharIndices};

use pernixc_common::source_file::{SourceFile, SourcePosition};

use crate::{
    error::{Error, LexicalError},
    token::{Keyword, LiteralConstantToken, Token, TokenKind},
};

/// Represent a state-machine that lexes the source code and outputs a token
/// one at a time.
#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    source_file: &'src SourceFile,
    chars: Peekable<CharIndices<'src>>,
    current_position: SourcePosition,
}

impl<'src> Lexer<'src> {
    /// Create a new lexer targeting to the given `source_file` char slice
    pub fn new(source_file: &'src SourceFile) -> Lexer<'src> {
        Lexer {
            source_file,
            chars: source_file.source_code().char_indices().peekable(),
            current_position: SourcePosition {
                column: 1,
                line: 1,
                byte_index: 0,
            },
        }
    }

    /// Return a character at its current position and increment the position
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
                        // peek the next character
                        .peek()
                        // get the byte index of the next character
                        .map(|next_char| next_char.0)
                        // if there is no next character, get the length of the
                        // source code
                        .unwrap_or(self.source_file.source_code().len())
                };
            }
            _ => {}
        }
        next
    }

    /// Check if the character can be an identifier character
    fn is_identifier_character(char: char) -> bool {
        return char == '_' || char.is_alphanumeric();
    }

    /// Check if the character can be the first character in an identifier
    fn is_first_identifier_character(char: char) -> bool {
        return char == '_' || char.is_alphabetic();
    }

    /// Keep lexing until the predicate returns false
    fn lex_predicate(&mut self, predicate: fn(char) -> bool) {
        while {
            match self.chars.peek() {
                None => false,
                Some((_, char)) => predicate(*char),
            }
        } {
            self.next();
        }
    }

    /// Lex the current word; returns the corresponding token and move to the
    /// next token.
    pub fn lex(&mut self) -> Result<Token<'src>, Error<'src>> {
        // the current source file position
        let first_position = self.current_position;

        let first_char;
        let begin_index;
        let token_kind;

        match self.next() {
            None => {
                return Ok(Token {
                    token_kind: TokenKind::EndOfFile,
                    position_range: first_position..first_position,
                    lexeme: "",
                });
            }
            Some((index, char)) => {
                first_char = char;
                begin_index = index;
            }
        }

        // found space token
        if first_char.is_whitespace() {
            self.lex_predicate(|char| char.is_whitespace());

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
                                return Err(Error {
                                    source_file: self.source_file,
                                    lexical_error:
                                        LexicalError::UnterminatedMultilineComment {
                                            multiline_comment_position:
                                                first_position.into(),
                                        },
                                });
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
            self.lex_predicate(Self::is_identifier_character);

            let identifier_string = &self.source_file.source_code()
                [begin_index..self.current_position.byte_index];

            match identifier_string {
                "true" => {
                    token_kind = TokenKind::LiteralConstant(
                        LiteralConstantToken::Boolean(true),
                    )
                }
                "false" => {
                    token_kind = TokenKind::LiteralConstant(
                        LiteralConstantToken::Boolean(false),
                    )
                }
                "return" => token_kind = TokenKind::Keyword(Keyword::Return),
                "let" => token_kind = TokenKind::Keyword(Keyword::Let),
                "using" => token_kind = TokenKind::Keyword(Keyword::Using),
                "namespace" => {
                    token_kind = TokenKind::Keyword(Keyword::Namespace)
                }
                "if" => token_kind = TokenKind::Keyword(Keyword::If),
                "else" => token_kind = TokenKind::Keyword(Keyword::Else),
                "while" => token_kind = TokenKind::Keyword(Keyword::While),
                "break" => token_kind = TokenKind::Keyword(Keyword::Break),
                "public" => token_kind = TokenKind::Keyword(Keyword::Public),
                "private" => token_kind = TokenKind::Keyword(Keyword::Private),
                "continue" => {
                    token_kind = TokenKind::Keyword(Keyword::Continue)
                }
                "new" => token_kind = TokenKind::Keyword(Keyword::New),
                "mutable" => token_kind = TokenKind::Keyword(Keyword::Mutable),
                "class" => token_kind = TokenKind::Keyword(Keyword::Class),
                "int8" => token_kind = TokenKind::Keyword(Keyword::Int8),
                "int16" => token_kind = TokenKind::Keyword(Keyword::Int16),
                "int32" => token_kind = TokenKind::Keyword(Keyword::Int32),
                "int64" => token_kind = TokenKind::Keyword(Keyword::Int64),
                "uint8" => token_kind = TokenKind::Keyword(Keyword::Uint8),
                "uint16" => token_kind = TokenKind::Keyword(Keyword::Uint16),
                "uint32" => token_kind = TokenKind::Keyword(Keyword::Uint32),
                "uint64" => token_kind = TokenKind::Keyword(Keyword::Uint64),
                "float32" => token_kind = TokenKind::Keyword(Keyword::Float32),
                "float64" => token_kind = TokenKind::Keyword(Keyword::Float64),
                "bool" => token_kind = TokenKind::Keyword(Keyword::Bool),
                "void" => token_kind = TokenKind::Keyword(Keyword::Void),
                _ => token_kind = TokenKind::Identifier,
            }
        }
        // found punctuator
        else if first_char.is_ascii_punctuation() {
            token_kind = TokenKind::Punctuator(first_char);
        }
        // found a number literal
        else if first_char.is_digit(10) {
            // loop until non-digit character is found
            self.lex_predicate(|char| char.is_digit(10));

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
                            self.lex_predicate(|char| char.is_digit(10));
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
                        &self.source_file.source_code()[begin_index..*index]
                    }
                    None => &self.source_file.source_code()[begin_index..],
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
                    Some((index, _)) => &self.source_file.source_code()
                        [prefix_starting_index..*index],
                    None => {
                        &self.source_file.source_code()[prefix_starting_index..]
                    }
                });
            }

            token_kind =
                TokenKind::LiteralConstant(LiteralConstantToken::Number {
                    value: value_str,
                    literal_suffix: literal_prefix,
                    is_decimal,
                })
        }
        // this character can't be categorized under any token kinds
        else {
            return Err(Error {
                source_file: self.source_file,
                lexical_error: LexicalError::InvalidCharacter {
                    position: self.current_position.into(),
                    character: first_char,
                },
            });
        }

        // return the token
        return Ok(Token {
            token_kind,
            position_range: first_position..self.current_position,
            lexeme: {
                match self.chars.peek() {
                    Some((index, _)) => {
                        &self.source_file.source_code()[begin_index..*index]
                    }
                    None => &self.source_file.source_code()[begin_index..],
                }
            },
        });
    }
}

#[cfg(test)]
mod test;
