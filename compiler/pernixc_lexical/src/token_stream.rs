//! Contains the definition of the [`TokenStream`] and its iterators.

use std::ops::{Index, IndexMut};

use delegate::delegate;
use pernixc_common::source_file::SourceFileIterator;

use crate::{
    errors::LexicalError,
    token::{Token, TokenizationError},
};

/// Is a list of tokenized [`Token`]s.
///
/// This struct is the final output of the lexical analysis phase and is meant to be used by the
/// next stage of the compilation process.
///
/// This struct is meant to represent only a valid token stream. Therefore, it is not possible to
/// create an invalid token stream using the public API.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TokenStream(Vec<Token>);

/// Is an iterator over the tokens of a [`TokenStream`].
pub struct Iter<'a>(std::slice::Iter<'a, Token>);

/// Is a mutable iterator over the tokens of a [`TokenStream`].
pub struct IterMut<'a>(std::slice::IterMut<'a, Token>);

impl TokenStream {
    delegate! {
        to self.0 {
            pub fn is_empty(&self) -> bool;
            pub fn len(&self) -> usize;
            pub fn get(&self, index: usize) -> Option<&Token>;
            pub fn get_mut(&mut self, index: usize) -> Option<&mut Token>;
            pub fn first(&self) -> Option<&Token>;
            pub fn first_mut(&mut self) -> Option<&mut Token>;
            pub fn last(&self) -> Option<&Token>;
            pub fn last_mut(&mut self) -> Option<&mut Token>;
        }
    }

    /// Tokenizes the given source code.
    ///
    /// This function tokenizes the given iterator of source code by calling the
    /// [`Token::tokenize()`] repeatedly until the iterator is exhausted.
    ///
    /// # Parameters
    /// - `source_file_iterator`: The iterator that iterates over the source code.
    ///
    /// # Returns
    /// A tuple containing the stream of successfully tokenized tokens and a list of lexical errors
    /// encountered during tokenization.
    pub fn tokenize(mut source_file_iterator: SourceFileIterator) -> (Self, Vec<LexicalError>) {
        // list of tokens to return
        let mut tokens = Vec::new();

        // list of lexical errors encountered during tokenization
        let mut lexical_errors = Vec::new();

        loop {
            // Tokenizes the next token
            match Token::tokenize(&mut source_file_iterator) {
                Ok(token) => tokens.push(token),
                Err(TokenizationError::Lexical(lexical_error)) => {
                    lexical_errors.push(lexical_error)
                }
                Err(TokenizationError::EndOfSourceCodeIteratorArgument) => {
                    break (Self(tokens), lexical_errors)
                }
            }
        }
    }

    /// Returns an iterator over the tokens of the token stream.
    pub fn iter(&self) -> Iter { Iter(self.0.iter()) }

    /// Returns a mutable iterator over the tokens of the token stream.
    pub fn iter_mut(&mut self) -> IterMut { IterMut(self.0.iter_mut()) }
}

impl Index<usize> for TokenStream {
    type Output = Token;

    fn index(&self, index: usize) -> &Self::Output { &self.0[index] }
}

impl IndexMut<usize> for TokenStream {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output { &mut self.0[index] }
}
