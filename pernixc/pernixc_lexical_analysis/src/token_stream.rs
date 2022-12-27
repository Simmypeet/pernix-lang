use pernixc_common::source_file::SourceFile;

use crate::{
    error::Error,
    lexer::Lexer,
    token::{Token, TokenKind},
};

/// Represent a struct that contains a vector of [`Token`]s and a reference to
/// the source file. This struct is used to iterate over the tokens of a source
/// file.
#[derive(Debug, Clone)]
pub struct TokenStream<'src> {
    source_file: &'src SourceFile,
    tokens: Vec<Token<'src>>,
}

impl<'src> TokenStream<'src> {
    /// Tokenize the given `source_file` and return a [`TokenStream`] containing
    /// the tokens of the source file.
    pub fn tokenize(
        source_file: &'src SourceFile,
    ) -> Result<Self, Vec<Error<'src>>> {
        let mut lexer = Lexer::new(source_file);
        let mut errors = Vec::new();
        let mut tokens = Vec::new();

        // lex the source file until EOF is reached
        loop {
            match lexer.lex() {
                Ok(token) => {
                    let is_eof =
                        matches!(token.token_kind, TokenKind::EndOfFile);

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

        if errors.is_empty() {
            Ok(Self {
                source_file,
                tokens,
            })
        } else {
            Err(errors)
        }
    }

    /// Return a reference to the source file of this [`TokenStream`].
    pub fn source_file(&self) -> &'src SourceFile {
        self.source_file
    }

    /// Return a reference to the tokens of this [`TokenStream`].
    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    /// Get the token at the given `index` of this [`TokenStream`].
    /// Return `None` if the index is out of bounds.
    pub fn get(&self, index: usize) -> Option<&Token<'src>> {
        self.tokens.get(index)
    }

    /// Return an iterator over the tokens of this [`TokenStream`].
    pub fn iter(&self) -> std::slice::Iter<'_, Token<'src>> {
        self.tokens.iter()
    }
}

impl<'src> std::ops::Index<usize> for TokenStream<'src> {
    type Output = Token<'src>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.tokens[index]
    }
}

#[cfg(test)]
mod test;
