use crate::{
    error::LexicalError,
    lexer::Lexer,
    token::{Token, TokenType},
};

/// Represent a struct that contains a vector of [`Token`]s and a reference to
/// the source file. This struct is used to iterate over the tokens of a source
/// file.
#[derive(Debug, Clone)]
pub struct TokenStream<'src> {
    tokens: Vec<Token<'src>>,
    source_code: &'src str,
}

impl<'src> TokenStream<'src> {
    /// Tokenize the given `source_file` and return a [`TokenStream`] containing
    /// the tokens of the source file.
    pub fn tokenize(source_code: &'src str) -> (Self, Vec<LexicalError>) {
        let mut lexer = Lexer::new(source_code);
        let mut errors = Vec::new();
        let mut tokens = Vec::new();

        // lex the source file until EOF is reached
        loop {
            match lexer.lex() {
                Ok(token) => {
                    let is_eof = matches!(token.token_type, TokenType::EndOfFile);

                    tokens.push(token);

                    if is_eof {
                        break;
                    }
                }
                Err(lexical_error) => {
                    errors.push(lexical_error);
                }
            }
        }

        (
            Self {
                tokens,
                source_code,
            },
            errors,
        )
    }

    /// Return a reference to the tokens of this [`TokenStream`].
    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    /// Get the token at the given `index` of this [`TokenStream`].
    /// Return `None` if the index is out of bounds.
    pub fn get(&self, index: usize) -> Option<&Token> {
        self.tokens.get(index)
    }

    /// Return an iterator over the tokens of this [`TokenStream`].
    pub fn iter(&self) -> std::slice::Iter<Token> {
        self.tokens.iter()
    }

    /// Return a reference to the source code of this [`TokenStream`].
    pub fn source_code(&self) -> &str {
        self.source_code
    }
}

impl<'src> std::ops::Index<usize> for TokenStream<'src> {
    type Output = Token<'src>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.tokens[index]
    }
}

#[cfg(test)]
mod tests;
