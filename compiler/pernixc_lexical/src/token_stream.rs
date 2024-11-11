//! Contains the [`TokenStream`] struct and its related types.

use std::{collections::HashMap, ops::Index, sync::Arc};

use derive_more::{Deref, From};
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_base::{handler::Handler, source_file::SourceFile};

use crate::{
    error::{self, UndelimitedDelimiter},
    token::{self, Punctuation, Token},
};

pub mod strategy;

/// Is an enumeration of the different types of delimiters in the [`Delimited`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Delimiter {
    Parenthesis,
    Brace,
    Bracket,
}

/// Represents a list of tokens enclosed by a pair of delimiters.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Delimited {
    /// The opening delimiter.
    pub open: Punctuation,

    /// The stream of tokens inside the delimiter.
    pub token_stream: TokenStream,

    /// The closing delimiter.
    pub close: Punctuation,

    /// The type of delimiter.
    pub delimiter: Delimiter,
}

/// Is an enumeration of either a [`Token`] or a [`Delimited`].
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From,
)]
#[allow(missing_docs)]
pub enum TokenKind {
    Token(Token),
    Delimited(Delimited),
}

/// Is a list of well structured tokens in a tree-like structure.
///
/// The tree-like structure is formed by the [`Delimited`] tokens. The [`Token`]
/// tokens are the leaves of the tree. This struct isn't very easy to traverse
/// since it doesn't have a clear parent-child relationship between the tokens.
/// To make it easier to traverse, the [`Tree`] struct is used.
///
/// This struct is the final output of the lexical analysis phase and is meant
/// to be used by the next stage of the compilation process.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Deref, Getters,
)]
pub struct TokenStream {
    #[deref]
    tokens: Vec<TokenKind>,

    /// The source file used to generate the tokens.
    #[get = "pub"]
    source_file: Arc<SourceFile>,
}

impl TokenStream {
    /// Tokenizes the given source code.
    ///
    /// This function tokenizes the given iterator of source code by calling the
    /// [`Token::lex()`] repeatedly until the iterator is exhausted.
    ///
    /// # Parameters
    /// - `source_file_iterator`: The iterator that iterates over the source
    ///   code.
    ///
    /// # Returns
    /// A tuple containing the stream of successfully tokenized tokens and a
    /// list of lexical errors encountered during tokenization.
    #[must_use]
    pub fn tokenize(
        source_file: Arc<SourceFile>,
        handler: &dyn Handler<error::Error>,
    ) -> Self {
        // list of tokens to return
        let mut tokens = Vec::new();
        let mut source_file_iterator = source_file.iter();

        loop {
            // Tokenizes the next token
            match Token::lex(&mut source_file_iterator, handler) {
                Ok(token) => tokens.push(token),
                Err(token::Error::EndOfSourceCodeIteratorArgument) => {
                    break;
                }
                Err(token::Error::FatalLexicalError) => (),
            }
        }

        // reverse the tokens so that the `pop` method can be used to get the
        // next token
        tokens.reverse();

        // structure the tokens into a token stream
        let mut token_trees = Vec::new();
        while let Some(token_tree) =
            Self::handle_token(&mut tokens, &source_file, handler)
        {
            token_trees.push(token_tree);
        }

        Self { tokens: token_trees, source_file: source_file.clone() }
    }

    fn handle_token(
        tokens: &mut Vec<Token>,
        source_file: &Arc<SourceFile>,
        handler: &dyn Handler<error::Error>,
    ) -> Option<TokenKind> {
        tokens.pop().and_then(|x| {
            Self::handle_popped_token(tokens, x, source_file, handler)
        })
    }

    fn handle_popped_token(
        tokens: &mut Vec<Token>,
        popped_token: Token,
        source_file: &Arc<SourceFile>,
        handler: &dyn Handler<error::Error>,
    ) -> Option<TokenKind> {
        match popped_token {
            Token::Punctuation(pun) if pun.punctuation == '{' => {
                Self::handle_delimited(
                    tokens,
                    pun,
                    Delimiter::Brace,
                    source_file,
                    handler,
                )
                .map(TokenKind::Delimited)
            }
            Token::Punctuation(pun) if pun.punctuation == '[' => {
                Self::handle_delimited(
                    tokens,
                    pun,
                    Delimiter::Bracket,
                    source_file,
                    handler,
                )
                .map(TokenKind::Delimited)
            }
            Token::Punctuation(pun) if pun.punctuation == '(' => {
                Self::handle_delimited(
                    tokens,
                    pun,
                    Delimiter::Parenthesis,
                    source_file,
                    handler,
                )
                .map(TokenKind::Delimited)
            }
            token => Some(TokenKind::Token(token)),
        }
    }

    fn handle_delimited(
        tokens: &mut Vec<Token>,
        open: Punctuation,
        delimiter: Delimiter,
        source_file: &Arc<SourceFile>,
        handler: &dyn Handler<error::Error>,
    ) -> Option<Delimited> {
        let mut token_trees = Vec::new();

        while let Some(token) = tokens.pop() {
            match (token, delimiter) {
                (Token::Punctuation(close), Delimiter::Brace)
                    if close.punctuation == '}' =>
                {
                    return Some(Delimited {
                        open,
                        token_stream: Self {
                            tokens: token_trees,
                            source_file: source_file.clone(),
                        },
                        close,
                        delimiter,
                    })
                }
                (Token::Punctuation(close), Delimiter::Bracket)
                    if close.punctuation == ']' =>
                {
                    return Some(Delimited {
                        open,
                        token_stream: Self {
                            tokens: token_trees,
                            source_file: source_file.clone(),
                        },
                        close,
                        delimiter,
                    })
                }
                (Token::Punctuation(close), Delimiter::Parenthesis)
                    if close.punctuation == ')' =>
                {
                    return Some(Delimited {
                        open,
                        token_stream: Self {
                            tokens: token_trees,
                            source_file: source_file.clone(),
                        },
                        close,
                        delimiter,
                    })
                }
                (token, _) => {
                    let Some(token_tree) = Self::handle_popped_token(
                        tokens,
                        token,
                        source_file,
                        handler,
                    ) else {
                        break;
                    };

                    token_trees.push(token_tree);
                }
            }
        }

        handler.receive(error::Error::UndelimitedDelimiter(
            UndelimitedDelimiter { opening_span: open.span, delimiter },
        ));

        None
    }

    /// Dissolves this struct into a tuple of its components.
    #[must_use]
    pub fn dissolve(self) -> Vec<TokenKind> { self.tokens }
}

impl Index<usize> for TokenStream {
    type Output = TokenKind;

    fn index(&self, index: usize) -> &Self::Output { &self.tokens[index] }
}

/// Represents a kind of node in the tree; either the root or a child delimited
/// node.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
pub enum NodeKind<'a> {
    /// The root of the tree. Always has the index `0`.
    Root(&'a TokenStream),

    /// The child node, which is delimited.
    Delimited {
        /// The delimited node
        delimited: &'a Delimited,

        /// The index of the parent node.
        parent: usize,
    },
}

impl<'a> NodeKind<'a> {
    /// Gets the index of the parent node. Returns `None` if the node is the
    /// root node.
    pub fn get_parent(&self) -> Option<usize> {
        match &self {
            NodeKind::Root(_) => None,
            NodeKind::Delimited { parent, .. } => Some(*parent),
        }
    }

    /// Gets the token stream of the node.
    pub fn token_stream(&self) -> &'a TokenStream {
        match &self {
            NodeKind::Root(token_stream) => token_stream,
            NodeKind::Delimited { delimited, .. } => &delimited.token_stream,
        }
    }
}

/// Represents a location to a particular token in the token stream [`Tree`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location {
    /// The index of the node in the [`Tree`].
    pub node_index: usize,

    /// The index of the token in the token stream.
    pub token_index: usize,
}

impl Location {
    /// Constructs a new location.
    pub fn new(node_index: usize, token_index: usize) -> Self {
        Self { node_index, token_index }
    }
}

/// Represents a node in the tree.
#[derive(Debug, Clone, PartialEq, Eq, derive_more::Deref)]
pub struct Node<'a> {
    /// The kind of node.
    #[deref]
    pub kind: NodeKind<'a>,

    /// Maps the index of the token in this node [`TokenStream`] to the index
    /// of the child node in the [`Tree`].
    pub child_ids_by_token_index: HashMap<usize, usize>,
}

/// Represents a tree of [`TokenStream`]s. This allows easier traversal of the
/// [`TokenStream`] tree.
///
/// The tree is stored in pre-order traversal. The first node is always the
/// root node.
///
/// The node is uniquely identified by its index in the tree. The indices used
/// in identifying the nodes are assumed to be always valid when used in the
/// various methods of this struct; therefore, it is the responsibility of the
/// caller to ensure that the indices are valid or else a panic will occur.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tree<'a> {
    /// The nodes are stored in pre-order traversal. The first node is always
    /// the root node.
    nodes: Vec<Node<'a>>,
}

impl<'a> Tree<'a> {
    /// Constructs a new tree from the given [`TokenStream`] as the root node.
    #[must_use]
    pub fn new(token_stream: &'a TokenStream) -> Self {
        let mut result = Self { nodes: Vec::new() };

        result.nodes.push(Node {
            kind: NodeKind::Root(token_stream),
            child_ids_by_token_index: HashMap::new(),
        });

        result.add(token_stream, 0);

        result
    }

    fn add(&mut self, token_stream: &'a TokenStream, node_index: usize) {
        for (tok_index, tree) in token_stream.iter().enumerate() {
            let TokenKind::Delimited(delimited) = tree else {
                continue;
            };

            let child_index = self.nodes.len();
            self.nodes.push(Node {
                kind: NodeKind::Delimited { delimited, parent: node_index },
                child_ids_by_token_index: HashMap::new(),
            });

            assert!(self.nodes[node_index]
                .child_ids_by_token_index
                .insert(tok_index, child_index)
                .is_none());

            self.add(&delimited.token_stream, child_index);
        }
    }

    /// Returns the node at the given index.
    ///
    /// # Panics
    ///
    /// Panics if the index is out of bounds.
    #[must_use]
    pub fn get_node(&self, index: usize) -> &Node<'a> { &self.nodes[index] }

    /// Returns the number of nodes in the tree.
    pub fn len(&self) -> usize { self.nodes.len() }

    /// Returns the root node of the tree.
    pub fn root_node(&self) -> &Node<'a> { &self.nodes[0] }

    /// Returns the [`TokenStream`] of the root node.
    pub fn root_token_stream(&self) -> &'a TokenStream {
        self.root_node().kind.token_stream()
    }

    /// Returns the location of the given token in the tree.
    ///
    /// # Panics
    ///
    /// If the [Location::token_stream_node_index] is out of bounds.
    ///
    /// # Returns
    ///
    /// The location of the token in the tree. [`None`] if the
    /// [Location::token_index] is out of bounds.
    pub fn get_token(&self, location: &Location) -> Option<&'a TokenKind> {
        let node = self.get_node(location.node_index);
        let token_stream = node.kind.token_stream();

        token_stream.get(location.token_index)
    }
}

#[cfg(test)]
pub(crate) mod test;
