use std::{iter::Peekable, str::CharIndices};

use pernixc_common::source_file::TextPosition;

use crate::{
    error::LexicalError,
    token::{Keyword, LiteralToken, NumberLiteralSuffix, Token, TokenType},
};

/// Represents a state-machine that lexes the source code and outputs a token one at a time. This
/// struct is the main component of the lexical analysis phase.
#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    source_code: &'src str,
    chars: Peekable<CharIndices<'src>>,
    current_position: (TextPosition, usize),
}

impl<'src> Lexer<'src> {
    /// Creates a new lexer targeting to the given `source_code` char slice
    pub fn new(source_code: &'src str) -> Lexer<'src> {
        Lexer {
            source_code,
            chars: source_code.char_indices().peekable(),
            current_position: (TextPosition { column: 1, line: 1 }, 0),
        }
    }

    /// Returns a character at its current position and increment the position
    fn next(&mut self) -> Option<(usize, char)> {
        let next = self.chars.next();

        // increment to the next position
        match next {
            Some(char) => {
                if char.1 == '\n' {
                    self.current_position.0.line += 1;
                    self.current_position.0.column = 1;
                } else {
                    self.current_position.0.column += 1;
                }

                self.current_position.1 = {
                    self.chars
                        // peek the next character
                        .peek()
                        // get the byte index of the next character
                        .map(|next_char| next_char.0)
                        // if there is no next character, get the length of the
                        // source code
                        .unwrap_or(self.source_code.len())
                };
            }
            _ => {}
        }
        next
    }

    /// Checks if the character can be an identifier character
    fn is_identifier_character(char: char) -> bool {
        return char == '_'
            || (!char.is_ascii_punctuation() && !char.is_whitespace() && !char.is_control());
    }

    /// Checks if the character can be the first character in an identifier
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
    pub fn lex(&mut self) -> Result<Token<'src>, LexicalError> {
        // the current source file position
        let first_position = self.current_position;

        let first_char;
        let begin_index;
        let token_type;

        match self.next() {
            None => {
                return Ok(Token {
                    token_type: TokenType::EndOfFile,
                    position_range: first_position.0..first_position.0,
                    byte_index_range: first_position.1..first_position.1,
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

            token_type = TokenType::Space;
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
                                token_type = TokenType::Comment(
                                    &self.source_code[begin_index..self.current_position.1],
                                );
                                break;
                            }
                            None => {
                                token_type = TokenType::Comment(&self.source_code[begin_index..]);
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
                                        token_type = TokenType::Comment(
                                            &self.source_code[begin_index..self.current_position.1],
                                        );
                                        break;
                                    }
                                    _ => {}
                                }
                            }
                            None => {
                                // error: multi-line comment is not closed
                                return Err(LexicalError::UnterminatedMultilineComment {
                                    multiline_comment_position: first_position.0,
                                });
                            }
                            _ => {
                                self.next();
                            }
                        }
                    }
                }
                _ => {
                    token_type = TokenType::Punctuation('/');
                }
            }
        }
        // found identifier
        else if Self::is_first_identifier_character(first_char) {
            self.lex_predicate(Self::is_identifier_character);

            let identifier_string = &self.source_code[begin_index..self.current_position.1];

            match identifier_string {
                "true" => token_type = TokenType::Literal(LiteralToken::BoolLiteral(true)),
                "false" => token_type = TokenType::Literal(LiteralToken::BoolLiteral(false)),
                "using" => token_type = TokenType::Keyword(Keyword::Using),
                "class" => token_type = TokenType::Keyword(Keyword::Class),
                "public" => token_type = TokenType::Keyword(Keyword::Public),
                "private" => token_type = TokenType::Keyword(Keyword::Private),
                "function" => token_type = TokenType::Keyword(Keyword::Function),
                "module" => token_type = TokenType::Keyword(Keyword::Module),
                "void" => token_type = TokenType::Keyword(Keyword::Void),
                "int8" => token_type = TokenType::Keyword(Keyword::Int8),
                "int16" => token_type = TokenType::Keyword(Keyword::Int16),
                "int32" => token_type = TokenType::Keyword(Keyword::Int32),
                "int64" => token_type = TokenType::Keyword(Keyword::Int64),
                "uint8" => token_type = TokenType::Keyword(Keyword::Uint8),
                "uint16" => token_type = TokenType::Keyword(Keyword::Uint16),
                "uint32" => token_type = TokenType::Keyword(Keyword::Uint32),
                "uint64" => token_type = TokenType::Keyword(Keyword::Uint64),
                "float32" => token_type = TokenType::Keyword(Keyword::Float32),
                "float64" => token_type = TokenType::Keyword(Keyword::Float64),
                "bool" => token_type = TokenType::Keyword(Keyword::Bool),
                "if" => token_type = TokenType::Keyword(Keyword::If),
                "else" => token_type = TokenType::Keyword(Keyword::Else),
                "while" => token_type = TokenType::Keyword(Keyword::While),
                "break" => token_type = TokenType::Keyword(Keyword::Break),
                "continue" => token_type = TokenType::Keyword(Keyword::Continue),
                "return" => token_type = TokenType::Keyword(Keyword::Return),
                "let" => token_type = TokenType::Keyword(Keyword::Let),
                "mutable" => token_type = TokenType::Keyword(Keyword::Mutable),
                "var" => token_type = TokenType::Keyword(Keyword::Var),
                "export" => token_type = TokenType::Keyword(Keyword::Export),
                "new" => token_type = TokenType::Keyword(Keyword::New),
                "import" => token_type = TokenType::Keyword(Keyword::Import),
                _ => token_type = TokenType::Identifier(identifier_string),
            }
        }
        // found punctuator
        else if first_char.is_ascii_punctuation() {
            token_type = TokenType::Punctuation(first_char);
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
                    Some((index, _)) => &self.source_code[begin_index..*index],
                    None => &self.source_code[begin_index..],
                }
            };

            let mut literal_suffix = None;
            let suffix_starting_text_position = self.current_position.0;

            if match self.chars.peek() {
                Some((_, char)) => Self::is_first_identifier_character(*char),
                _ => false,
            } {
                let suffix_starting_index = self.chars.peek().unwrap().0;

                // loop until non-identifier character is found
                while {
                    match self.chars.peek() {
                        None => false,
                        Some((_, char)) => Self::is_identifier_character(*char),
                    }
                } {
                    self.next();
                }

                literal_suffix = Some(match self.chars.peek() {
                    Some((index, _)) => &self.source_code[suffix_starting_index..*index],
                    None => &self.source_code[suffix_starting_index..],
                });
            }

            let suffix = if let Some(literal_suffix) = literal_suffix {
                match literal_suffix {
                    "i8" => Some(NumberLiteralSuffix::Int8),
                    "i16" => Some(NumberLiteralSuffix::Int16),
                    "i32" => Some(NumberLiteralSuffix::Int32),
                    "i64" => Some(NumberLiteralSuffix::Int64),
                    "u8" => Some(NumberLiteralSuffix::Uint8),
                    "u16" => Some(NumberLiteralSuffix::Uint16),
                    "u32" => Some(NumberLiteralSuffix::Uint32),
                    "u64" => Some(NumberLiteralSuffix::Uint64),
                    "f32" => Some(NumberLiteralSuffix::Float32),
                    "f64" => Some(NumberLiteralSuffix::Float64),
                    _ => {
                        return Err(LexicalError::InvalidLiteralSuffix {
                            literal_suffix_position: suffix_starting_text_position
                                ..self.current_position.0,
                        })
                    }
                }
            } else {
                None
            };

            token_type = TokenType::Literal(LiteralToken::NumberLiteral {
                value: value_str,
                suffix,
                is_decimal,
            })
        }
        // this character can't be categorized under any token kinds
        else {
            return Err(LexicalError::InvalidCharacter {
                position: self.current_position.0,
                character: first_char,
            });
        }

        // return the token
        return Ok(Token {
            token_type,
            position_range: first_position.0..self.current_position.0,
            byte_index_range: first_position.1..self.current_position.1,
        });
    }
}

#[cfg(test)]
mod tests;
