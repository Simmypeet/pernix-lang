//! Contains the [`TokenStream`] struct and its related types.

use std::{collections::HashMap, ops::Index, sync::Arc};

use derive_more::{Deref, From};
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_handler::Handler;
use pernixc_source_file::SourceFile;
use proptest::bits::usize;

use crate::{
    error::{self, InvalidIndentation, UndelimitedDelimiter},
    token::{self, Comment, CommentKind, Punctuation, Kind},
};

pub mod strategy;

/// The represents how the [`TokenStream`] is structured in a fragment.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum FragmentKind {
    /// The [`TokenStream`] is enclosed by a pair of delimiters.
    Delimiter(Delimiter),

    /// The [`TokenStream`] is grouped by having the same indentation level.
    Indentation(Indentation),
}

/// Represents a delimiter pair such as `( ... )`, `{ ... }`, or `[ ... ]`
/// delimiters.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Delimiter {
    /// The opening delimiter.
    pub open: Punctuation,

    /// The closing delimiter.
    pub close: Punctuation,

    /// The type of delimiter.
    pub delimiter: DelimiterKind,
}

/// The token stream is grouped by having the same indentation level. The
/// indentation group is started by a colon followed by a newline character.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Indentation {
    /// The indentation level of the group (in space or tab characters count,
    /// not how many levels deep).
    pub indentation_size: usize,

    /// The colon character signifying the start of the indentation group.
    pub colon: Punctuation,

    /// List of insignificant tokens that appear before the
    /// [`Self::new_line_or_line_comment`].
    ///
    /// For example, `: /*comment*/ \n` would have `:` as the colon,
    /// `/*comment*/` as the `prior_new_line_tokens`, and `\n` as the
    /// `new_line_or_line_comment`.
    pub prior_new_line_tokens: Vec<Kind>,

    /// The new line character or line comment that follows the colon.
    ///
    /// The colon can be followed by a line comment as well since the line
    /// comment is also ended by a new line character.
    pub new_line_or_line_comment: Kind,

    /// The token that marks an end of the indentation group.
    ///
    /// This can either be the last new line/line comment or the closing
    /// delimiter if the indentation group is enclosed by a pair of delimiters.
    /// Alternatively, it can be `None` if the indentation group appears last
    /// in the file (EOF).
    pub ending_token: Option<Kind>,
}

/// Is an enumeration of the different types of delimiters in the [`Delimiter`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum DelimiterKind {
    Parenthesis,
    Brace,
    Bracket,
}

/// Represents a list of tokens enclosed by a pair of delimiters.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Fragment {
    /// The kind of this fragment.
    pub kind: FragmentKind,

    /// The stream of tokens inside the delimiter.
    pub token_stream: TokenStream,
}

/// Is an enumeration of either a [`Token`] or a [`Delimited`].
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From,
)]
#[allow(missing_docs)]
pub enum TokenKind {
    Token(Kind),
    Fragment(Fragment),
}

/// Is a list of well structured tokens in a tree-like structure.
///
/// The [`TokenStream`] consists of a list of [`TokenKind`]s. It's structured
/// similarly to a tree where [`TokenKind::Token`] represents a leaf node and
/// [`TokenKind::Fragment`] represents a branch node.
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum IndentationLevelSearch {
    Found(usize),
    EndOfStream,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct IndentationMark {
    indentation_size: usize,
    colon: Punctuation,
    prior_new_line_tokens: Vec<Kind>,
    new_line_or_line_comment: Kind,
    starting_index: usize,
}

impl TokenStream {
    /// The size of a space character a tab character is equivalent to.
    pub const TAB_INDENT_SIZE: usize = 4;

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
            match Kind::lex(&mut source_file_iterator, handler) {
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
            Self::pop_token(&mut tokens, &source_file, handler)
        {
            token_trees.push(token_tree);
        }

        Self::handle_indentation(
            &mut token_trees,
            &mut vec![],
            &source_file,
            None,
            handler,
        );

        Self { tokens: token_trees, source_file }
    }

    fn get_indentation_level(
        tokens: &[TokenKind],
    ) -> (usize, IndentationLevelSearch) {
        let mut size = 0;
        for (idx, token) in tokens.iter().enumerate() {
            match token {
                TokenKind::Token(token) => match token {
                    // no indentation level here, start a new line
                    Kind::Comment(Comment {
                        kind: CommentKind::Line, ..
                    })
                    | Kind::NewLine(_) => {
                        size = 0;
                    }

                    Kind::Character(_)
                    | Kind::String(_)
                    | Kind::Numeric(_)
                    | Kind::Punctuation(_)
                    | Kind::Identifier(_)
                    | Kind::Keyword(_)
                    | Kind::Comment(Comment {
                        kind: CommentKind::Delimited,
                        ..
                    }) => return (idx, IndentationLevelSearch::Found(size)),

                    // count the space
                    Kind::WhiteSpaces(whitespaces) => {
                        whitespaces.span.str().chars().for_each(|x| {
                            let val = if x == '\t' {
                                Self::TAB_INDENT_SIZE
                            } else {
                                1
                            };

                            size += val;
                        });
                    } // found a significant token
                },
                TokenKind::Fragment(_) => {
                    return (idx, IndentationLevelSearch::Found(size))
                }
            }
        }

        (tokens.len(), IndentationLevelSearch::EndOfStream)
    }

    fn pop_indentation(
        tokens: &mut Vec<TokenKind>,
        indentation_levels: &mut Vec<IndentationMark>,
        source_file: &Arc<SourceFile>,
        pop_count: usize,
        enclosing_delimiter: Option<&Punctuation>,
        mut end_index: usize,
    ) -> usize {
        for _ in 0..pop_count {
            let indentation = indentation_levels.pop().unwrap();

            // +2 is for the new line character and colon
            let eaten_token_by_indentation =
                indentation.prior_new_line_tokens.len() + 2;

            let pop_range = (indentation.starting_index
                + eaten_token_by_indentation)
                ..end_index;

            let ending_token = tokens
                .get(end_index)
                .cloned()
                .map(|x| x.into_token().unwrap())
                .or_else(|| {
                    enclosing_delimiter.map(|x| Kind::Punctuation(x.clone()))
                });
            let indented_tokens = tokens.drain(pop_range).collect::<Vec<_>>();

            // replace the indentation with the indented tokens
            tokens.splice(
                indentation.starting_index
                    ..(indentation.starting_index + eaten_token_by_indentation),
                std::iter::once(TokenKind::Fragment(Fragment {
                    kind: FragmentKind::Indentation(Indentation {
                        indentation_size: indentation.indentation_size,
                        colon: indentation.colon,
                        prior_new_line_tokens: indentation
                            .prior_new_line_tokens
                            .clone(),
                        new_line_or_line_comment: indentation
                            .new_line_or_line_comment,
                        ending_token,
                    }),
                    token_stream: Self {
                        tokens: indented_tokens,
                        source_file: source_file.clone(),
                    },
                })),
            );

            end_index = indentation.starting_index + 1;
        }

        end_index
    }

    fn followed_by_new_line(tokens: &[TokenKind]) -> Option<usize> {
        for (i, token) in tokens.iter().enumerate() {
            match token {
                TokenKind::Token(token) => match token {
                    Kind::WhiteSpaces(_) => {}

                    Kind::Identifier(_)
                    | Kind::Keyword(_)
                    | Kind::Punctuation(_)
                    | Kind::Numeric(_)
                    | Kind::Comment(Comment {
                        kind: CommentKind::Delimited,
                        ..
                    })
                    | Kind::Character(_)
                    | Kind::String(_) => return None,

                    Kind::Comment(Comment {
                        kind: CommentKind::Line, ..
                    })
                    | Kind::NewLine(_) => return Some(i),
                },
                TokenKind::Fragment(_) => return None,
            }
        }

        None
    }

    fn handle_possible_indentation(
        tokens: &mut [TokenKind],
        indentation_levels: &mut Vec<IndentationMark>,
        new_line_offset: usize,
        index: usize,
        handler: &dyn Handler<error::Error>,
    ) -> usize {
        let indentation_start_index = index;
        let new_line_index = new_line_offset + index + 1;
        let prior_new_line_tokens_range = (index + 1)..new_line_index;

        let mut search_index = new_line_index + 1;

        let level = loop {
            if search_index >= tokens.len() {
                return tokens.len();
            }

            let (offset, level) =
                Self::get_indentation_level(&tokens[search_index..]);

            if let IndentationLevelSearch::Found(level) = level {
                search_index += offset;
                break level;
            }

            search_index += offset + 1;
        };

        if indentation_levels.last().is_none_or(|x| x.indentation_size < level)
        {
            indentation_levels.push(IndentationMark {
                indentation_size: level,
                colon: tokens[index]
                    .clone()
                    .as_token()
                    .unwrap()
                    .as_punctuation()
                    .unwrap()
                    .clone(),
                prior_new_line_tokens: tokens[prior_new_line_tokens_range]
                    .iter()
                    .map(|x| x.as_token().unwrap().clone())
                    .collect(),
                new_line_or_line_comment: tokens[new_line_index]
                    .as_token()
                    .unwrap()
                    .clone(),
                starting_index: indentation_start_index,
            });
        } else {
            handler.receive(error::Error::InvalidNewIndentationLevel(
                error::InvalidNewIndentationLevel {
                    span: match &tokens[index] {
                        TokenKind::Token(token) => token.span().clone(),
                        TokenKind::Fragment(fragment) => match &fragment.kind {
                            FragmentKind::Delimiter(delimiter) => {
                                delimiter.open.span.clone()
                            }
                            FragmentKind::Indentation(indentation) => {
                                indentation.colon.span.clone()
                            }
                        },
                    },
                },
            ));
        }

        search_index - 1
    }

    fn handle_new_indentation_line(
        tokens: &mut Vec<TokenKind>,
        indentation_levels: &mut Vec<IndentationMark>,
        new_line_index: usize,
        source_file: &Arc<SourceFile>,
        enclosing_delimiter: Option<&Punctuation>,
        handler: &dyn Handler<error::Error>,
    ) -> usize {
        let search_index = new_line_index + 1;

        // calculate the indentation level
        let (offset, level) =
            Self::get_indentation_level(&tokens[search_index..]);

        let pop_count = match level {
            IndentationLevelSearch::Found(level) => {
                let mut pop_count = None::<usize>;

                for i in 0..indentation_levels.len() {
                    let current =
                        &indentation_levels[indentation_levels.len() - i - 1];

                    if current.indentation_size == level {
                        pop_count = Some(i);
                        break;
                    }
                }

                let less_than_first = indentation_levels
                    .first()
                    .is_none_or(|x| x.indentation_size > level);

                // if the indentation level is not found, then it is
                // an indentation error
                if pop_count.is_none() && !less_than_first {
                    handler.receive(error::Error::InvalidIndentation(
                        InvalidIndentation {
                            span: match &tokens[search_index + offset] {
                                TokenKind::Token(token) => token.span().clone(),
                                TokenKind::Fragment(fragment) => {
                                    match &fragment.kind {
                                        FragmentKind::Delimiter(delimiter) => {
                                            delimiter.open.span.clone()
                                        }
                                        FragmentKind::Indentation(
                                            indentation,
                                        ) => indentation.colon.span.clone(),
                                    }
                                }
                            },
                        },
                    ));
                }

                pop_count.unwrap_or(if less_than_first {
                    indentation_levels.len()
                } else {
                    0
                })
            }

            IndentationLevelSearch::EndOfStream => indentation_levels.len(),
        };

        let diff = search_index - new_line_index;

        Self::pop_indentation(
            tokens,
            indentation_levels,
            source_file,
            pop_count,
            enclosing_delimiter,
            new_line_index,
        ) + diff
    }

    #[allow(clippy::too_many_lines)]
    fn handle_indentation(
        tokens: &mut Vec<TokenKind>,
        indentation_levels: &mut Vec<IndentationMark>,
        source_file: &Arc<SourceFile>,
        enclosing_delimiter: Option<&Punctuation>,
        handler: &dyn Handler<error::Error>,
    ) {
        let mut index = 0;

        while index < tokens.len() {
            // check for the new indentation level
            if let (
                TokenKind::Token(Kind::Punctuation(Punctuation {
                    punctuation: ':',
                    ..
                })),
                Some(offset),
            ) = (
                &tokens[index],
                Self::followed_by_new_line(&tokens[(index + 1)..]),
            ) {
                index = Self::handle_possible_indentation(
                    tokens,
                    indentation_levels,
                    offset,
                    index,
                    handler,
                );
            }
            // recursively handle the indentation in the delimited fragmnet
            else if tokens[index].is_fragment() {
                let fragmnet = tokens[index].as_fragment_mut().unwrap();
                assert!(fragmnet.kind.is_delimiter());

                Self::handle_indentation(
                    &mut fragmnet.token_stream.tokens,
                    &mut vec![],
                    source_file,
                    Some(&fragmnet.kind.as_delimiter().unwrap().close),
                    handler,
                );

                index += 1;
            }
            // handle indentation level
            else if matches!(
                &tokens[index],
                TokenKind::Token(
                    Kind::Comment(Comment { kind: CommentKind::Line, .. })
                        | Kind::NewLine(_),
                )
            ) {
                index = Self::handle_new_indentation_line(
                    tokens,
                    indentation_levels,
                    index,
                    source_file,
                    enclosing_delimiter,
                    handler,
                );
            } else {
                index += 1;
            }
        }

        if !indentation_levels.is_empty() {
            Self::pop_indentation(
                tokens,
                indentation_levels,
                source_file,
                indentation_levels.len(),
                enclosing_delimiter,
                tokens.len(),
            );
        }
    }

    fn pop_token(
        tokens: &mut Vec<Kind>,
        source_file: &Arc<SourceFile>,
        handler: &dyn Handler<error::Error>,
    ) -> Option<TokenKind> {
        tokens.pop().and_then(|x| {
            Self::handle_popped_token(tokens, x, source_file, handler)
        })
    }

    fn handle_popped_token(
        tokens: &mut Vec<Kind>,
        popped_token: Kind,
        source_file: &Arc<SourceFile>,
        handler: &dyn Handler<error::Error>,
    ) -> Option<TokenKind> {
        match popped_token {
            Kind::Punctuation(pun) if pun.punctuation == '{' => {
                Self::handle_delimited(
                    tokens,
                    pun,
                    DelimiterKind::Brace,
                    source_file,
                    handler,
                )
                .map(TokenKind::Fragment)
            }
            Kind::Punctuation(pun) if pun.punctuation == '[' => {
                Self::handle_delimited(
                    tokens,
                    pun,
                    DelimiterKind::Bracket,
                    source_file,
                    handler,
                )
                .map(TokenKind::Fragment)
            }
            Kind::Punctuation(pun) if pun.punctuation == '(' => {
                Self::handle_delimited(
                    tokens,
                    pun,
                    DelimiterKind::Parenthesis,
                    source_file,
                    handler,
                )
                .map(TokenKind::Fragment)
            }

            token => Some(TokenKind::Token(token)),
        }
    }

    fn handle_delimited(
        tokens: &mut Vec<Kind>,
        open: Punctuation,
        delimiter: DelimiterKind,
        source_file: &Arc<SourceFile>,
        handler: &dyn Handler<error::Error>,
    ) -> Option<Fragment> {
        let mut token_trees = Vec::new();

        while let Some(token) = tokens.pop() {
            match (token, delimiter) {
                (Kind::Punctuation(close), DelimiterKind::Brace)
                    if close.punctuation == '}' =>
                {
                    return Some(Fragment {
                        kind: FragmentKind::Delimiter(Delimiter {
                            open,
                            close,
                            delimiter,
                        }),
                        token_stream: Self {
                            tokens: token_trees,
                            source_file: source_file.clone(),
                        },
                    })
                }
                (Kind::Punctuation(close), DelimiterKind::Bracket)
                    if close.punctuation == ']' =>
                {
                    return Some(Fragment {
                        token_stream: Self {
                            tokens: token_trees,
                            source_file: source_file.clone(),
                        },
                        kind: FragmentKind::Delimiter(Delimiter {
                            open,
                            close,
                            delimiter,
                        }),
                    })
                }
                (Kind::Punctuation(close), DelimiterKind::Parenthesis)
                    if close.punctuation == ')' =>
                {
                    return Some(Fragment {
                        token_stream: Self {
                            tokens: token_trees,
                            source_file: source_file.clone(),
                        },
                        kind: FragmentKind::Delimiter(Delimiter {
                            open,
                            close,
                            delimiter,
                        }),
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

/// Represents a kind of node in the tree; either the root or a child fragment
/// node.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
pub enum NodeKind<'a> {
    /// The root of the tree. Always has the index `0`.
    Root(&'a TokenStream),

    /// The child node, which is a fragment node.
    Fragment {
        /// The fragment node
        fragment: &'a Fragment,

        /// The index of the parent node.
        parent: usize,
    },
}

impl<'a> NodeKind<'a> {
    /// Gets the index of the parent node. Returns `None` if the node is the
    /// root node.
    #[must_use]
    pub const fn get_parent(&self) -> Option<usize> {
        match &self {
            NodeKind::Root(_) => None,
            NodeKind::Fragment { parent, .. } => Some(*parent),
        }
    }

    /// Gets the token stream of the node.
    #[must_use]
    pub const fn token_stream(&self) -> &'a TokenStream {
        match &self {
            NodeKind::Root(token_stream) => token_stream,
            NodeKind::Fragment { fragment: delimited, .. } => {
                &delimited.token_stream
            }
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
    #[must_use]
    pub const fn new(node_index: usize, token_index: usize) -> Self {
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
            let TokenKind::Fragment(delimited) = tree else {
                continue;
            };

            let child_index = self.nodes.len();
            self.nodes.push(Node {
                kind: NodeKind::Fragment {
                    fragment: delimited,
                    parent: node_index,
                },
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
    /// # Returns
    ///
    /// The node at the given index. [`None`] if the index is out of bounds.
    #[must_use]
    pub fn get_node(&self, index: usize) -> Option<&Node<'a>> {
        self.nodes.get(index)
    }

    /// Returns the number of nodes in the tree.
    #[must_use]
    pub fn len(&self) -> usize { self.nodes.len() }

    /// Returns `true` if the tree is empty.
    #[must_use]
    pub fn is_empty(&self) -> bool { self.nodes.is_empty() }

    /// Returns the root node of the tree.
    #[must_use]
    pub fn root_node(&self) -> &Node<'a> { &self.nodes[0] }

    /// Returns the [`TokenStream`] of the root node.
    #[must_use]
    pub fn root_token_stream(&self) -> &'a TokenStream {
        self.root_node().kind.token_stream()
    }

    /// Returns the location of the given token in the tree.
    ///
    /// # Returns
    ///
    /// The location of the token in the tree. [`None`] if the
    /// [`Location::token_index`] is out of bounds or if the
    /// [`Location::node_index`] is out of bounds.
    #[must_use]
    pub fn get_token(&self, location: &Location) -> Option<&'a TokenKind> {
        let node = self.get_node(location.node_index)?;
        let token_stream = node.kind.token_stream();

        token_stream.get(location.token_index)
    }
}

#[cfg(test)]
pub(crate) mod test;
