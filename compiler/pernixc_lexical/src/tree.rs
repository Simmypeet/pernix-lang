//! Contains the definition of the [`Tree`] struct

use std::ops::Range;

use derive_more::{Deref, DerefMut, From};
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_handler::Handler;
use pernixc_source_file::Span;

use crate::{
    error::{self, InvalidIndentation, UndelimitedDelimiter},
    token::{Kind, NewLine, Punctuation, Token, Tokenizer, WithInsignificant},
};

pub mod arbitrary;

/// The represents how the [`TokenStream`] is structured in a fragment.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum FragmentKind<S> {
    /// The [`TokenStream`] is enclosed by a pair of delimiters.
    Delimiter(Delimiter<S>),

    /// The [`TokenStream`] is grouped by having the same indentation level.
    Indentation(Indentation<S>),
}

/// Represents a delimiter pair such as `( ... )`, `{ ... }`, or `[ ... ]`
/// delimiters.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Delimiter<S> {
    /// The opening delimiter.
    pub open: WithInsignificant<Punctuation<S>, S>,

    /// The closing delimiter.
    pub close: WithInsignificant<Punctuation<S>, S>,

    /// The type of delimiter.
    pub delimiter: DelimiterKind,
}

/// The token stream is grouped by having the same indentation level. The
/// indentation group is started by a colon followed by a newline character.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Indentation<S> {
    /// The indentation level of the group (in space or tab characters count,
    /// not how many levels deep).
    pub indentation_size: usize,

    /// The colon character signifying the start of the indentation group.
    pub colon: WithInsignificant<Punctuation<S>, S>,

    /// The new line character or line comment that follows the colon.
    ///
    /// The colon can be followed by a line comment as well since the line
    /// comment is also ended by a new line character.
    pub new_line: WithInsignificant<NewLine<S>, S>,

    /// The token that marks an end of the indentation group.
    ///
    /// This can either be the last new line/line comment or the closing
    /// delimiter if the indentation group is enclosed by a pair of delimiters.
    /// Alternatively, it can be `None` if the indentation group appears last
    /// in the file (EOF).
    pub ending_token: Option<Token<S>>,
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
pub struct Fragment<S> {
    /// The kind of this fragment.
    pub kind: FragmentKind<S>,

    /// The stream of tokens inside the delimiter.
    pub token_stream: TokenStream<S>,
}

impl<S> Fragment<S> {
    /// Gets the `prior_insignificant` span of the first token in the fragment.
    #[must_use]
    pub const fn first_fragment_token_insignificant_span(&self) -> Option<&S> {
        match &self.kind {
            FragmentKind::Delimiter(delimiter) => {
                delimiter.open.prior_insignificant.as_ref()
            }
            FragmentKind::Indentation(indentation) => {
                indentation.colon.prior_insignificant.as_ref()
            }
        }
    }
}

/// Is an enumeration of either a [`Token`] or a [`Delimited`].
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From,
)]
#[allow(missing_docs)]
pub enum Node<S> {
    Leaf(Token<S>),
    Fragment(Fragment<S>),
}

/// Is a list of well structured tokens in a tree-like structure.
///
/// The [`TokenStream`] consists of a list of [`Node`]s. It's structured
/// similarly to a tree where [`Node::Leaf`] represents a leaf node and
/// [`Node::Fragment`] represents a branch node.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Deref, DerefMut, Getters,
)]
pub struct TokenStream<S>(pub Vec<Node<S>>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum IndentationLevelSearch {
    Found(usize),
    EndOfStream,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct IndentationMark<S> {
    indentation_size: usize,
    colon: WithInsignificant<Punctuation<S>, S>,
    new_line: WithInsignificant<NewLine<S>, S>,
    starting_index: usize,
}

impl<ID: Clone + PartialEq> TokenStream<Span<ID>> {
    /// The size of a space character a tab character is equivalent to.
    pub const TAB_INDENT_SIZE: usize = 4;

    /// Tokenizes the given source code and returns a [`TokenStream`].
    #[must_use]
    pub fn tokenize(
        source: &str,
        source_id: ID,
        handler: &dyn Handler<error::Error<ID>>,
    ) -> Self {
        // list of tokens to return
        let mut tokens =
            Tokenizer::new(source, source_id, handler).collect::<Vec<_>>();

        // reverse the tokens so that the `pop` method can be used to get the
        // next token
        tokens.reverse();

        // structure the tokens into a token stream
        let mut token_trees = Vec::new();
        while let Some(token_tree) =
            Self::pop_token(&mut tokens, source, handler)
        {
            token_trees.push(token_tree);
        }

        Self::handle_indentation(
            &mut token_trees,
            &mut vec![],
            source,
            None,
            handler,
        );

        Self(token_trees)
    }

    fn calculate_indent_size_from_span_range(
        source: &str,
        range: Option<Range<usize>>,
    ) -> usize {
        range.map_or(0, |x| {
            let source_range = &source[x];
            source_range
                .chars()
                .map(|x| if x == '\t' { Self::TAB_INDENT_SIZE } else { 1 })
                .sum()
        })
    }

    fn get_indentation_level(
        source: &str,
        tokens: &[Node<Span<ID>>],
    ) -> (usize, IndentationLevelSearch) {
        for (idx, token) in tokens.iter().enumerate() {
            match token {
                Node::Leaf(token) => {
                    // ignore new line
                    if token.token.is_new_line() {
                        continue;
                    }

                    return (
                        idx,
                        IndentationLevelSearch::Found(
                            Self::calculate_indent_size_from_span_range(
                                source,
                                token
                                    .prior_insignificant
                                    .as_ref()
                                    .map(Span::range),
                            ),
                        ),
                    );
                }

                Node::Fragment(fragment) => {
                    return (
                        idx,
                        IndentationLevelSearch::Found(
                            Self::calculate_indent_size_from_span_range(
                                source,
                                fragment
                                    .first_fragment_token_insignificant_span()
                                    .map(Span::range),
                            ),
                        ),
                    )
                }
            }
        }

        (tokens.len(), IndentationLevelSearch::EndOfStream)
    }

    #[allow(clippy::type_complexity)]
    fn pop_indentation(
        tokens: &mut Vec<Node<Span<ID>>>,
        indentation_levels: &mut Vec<IndentationMark<Span<ID>>>,
        pop_count: usize,
        enclosing_delimiter: Option<
            &WithInsignificant<Punctuation<Span<ID>>, Span<ID>>,
        >,
        mut end_index: usize,
    ) -> usize {
        for _ in 0..pop_count {
            let indentation = indentation_levels.pop().unwrap();

            // +2 is for the new line character and colon
            let pop_range = (indentation.starting_index + 2)..end_index;

            let ending_token = tokens
                .get(end_index)
                .cloned()
                .map(|x| x.into_leaf().unwrap_or_else(|_| panic!()))
                .or_else(|| {
                    enclosing_delimiter.map(|x| Token {
                        token: Kind::Punctuation(x.token.clone()),
                        prior_insignificant: x.prior_insignificant.clone(),
                    })
                });

            let indented_tokens = tokens.drain(pop_range).collect::<Vec<_>>();

            // replace the indentation with the indented tokens
            tokens.splice(
                indentation.starting_index..(indentation.starting_index + 2),
                std::iter::once(Node::Fragment(Fragment {
                    kind: FragmentKind::Indentation(Indentation {
                        indentation_size: indentation.indentation_size,
                        colon: indentation.colon,
                        ending_token,
                        new_line: indentation.new_line,
                    }),
                    token_stream: Self(indented_tokens),
                })),
            );

            end_index = indentation.starting_index + 1;
        }

        end_index
    }

    fn handle_possible_indentation(
        tokens: &mut [Node<Span<ID>>],
        source: &str,
        indentation_levels: &mut Vec<IndentationMark<Span<ID>>>,
        index: usize,
        handler: &dyn Handler<error::Error<ID>>,
    ) -> usize {
        let indentation_start_index = index;
        let new_line_index = index + 1;

        let mut search_index = new_line_index + 1;

        let level = loop {
            if search_index >= tokens.len() {
                return tokens.len();
            }

            let (offset, level) =
                Self::get_indentation_level(source, &tokens[search_index..]);

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
                colon: WithInsignificant::new(
                    tokens[index]
                        .as_leaf()
                        .unwrap()
                        .token
                        .as_punctuation()
                        .unwrap()
                        .clone(),
                    tokens[index]
                        .as_leaf()
                        .unwrap()
                        .prior_insignificant
                        .clone(),
                ),
                new_line: WithInsignificant::new(
                    tokens[new_line_index]
                        .as_leaf()
                        .unwrap()
                        .token
                        .as_new_line()
                        .unwrap()
                        .clone(),
                    tokens[new_line_index]
                        .as_leaf()
                        .unwrap()
                        .prior_insignificant
                        .clone(),
                ),

                starting_index: indentation_start_index,
            });
        } else {
            handler.receive(error::Error::InvalidNewIndentationLevel(
                error::InvalidNewIndentationLevel {
                    span: (match &tokens[index] {
                        Node::Leaf(token) => token.token.span().clone(),
                        Node::Fragment(fragment) => match &fragment.kind {
                            FragmentKind::Delimiter(delimiter) => {
                                delimiter.open.token.span.clone()
                            }
                            FragmentKind::Indentation(indentation) => {
                                indentation.colon.token.span.clone()
                            }
                        },
                    })
                    .join(&match &tokens[search_index] {
                        Node::Leaf(token) => token.token.span().clone(),
                        Node::Fragment(fragment) => match &fragment.kind {
                            FragmentKind::Delimiter(delimiter) => {
                                delimiter.open.token.span.clone()
                            }
                            FragmentKind::Indentation(indentation) => {
                                indentation.colon.token.span.clone()
                            }
                        },
                    }),
                    found_indentation: level,
                    previous_indentation_span: indentation_levels
                        .last()
                        .map(|x| x.colon.token.span.clone()),
                    latest_indentation: indentation_levels
                        .last()
                        .unwrap()
                        .indentation_size,
                },
            ));
        }

        search_index - 1
    }

    fn handle_new_indentation_line(
        tokens: &mut Vec<Node<Span<ID>>>,
        indentation_levels: &mut Vec<IndentationMark<Span<ID>>>,
        new_line_index: usize,
        source: &str,
        enclosing_delimiter: Option<
            &WithInsignificant<Punctuation<Span<ID>>, Span<ID>>,
        >,
        handler: &dyn Handler<error::Error<ID>>,
    ) -> usize {
        let search_index = new_line_index + 1;

        // calculate the indentation level
        let (offset, level) =
            Self::get_indentation_level(source, &tokens[search_index..]);

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
                                Node::Leaf(token) => token.token.span().clone(),
                                Node::Fragment(fragment) => {
                                    match &fragment.kind {
                                        FragmentKind::Delimiter(delimiter) => {
                                            delimiter.open.token.span.clone()
                                        }
                                        FragmentKind::Indentation(
                                            indentation,
                                        ) => {
                                            indentation.colon.token.span.clone()
                                        }
                                    }
                                }
                            },
                            expected_indentation: indentation_levels
                                .last()
                                .unwrap()
                                .indentation_size,
                            found_indentation: level,
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
            pop_count,
            enclosing_delimiter,
            new_line_index,
        ) + diff
    }

    #[allow(clippy::too_many_lines)]
    fn handle_indentation(
        tokens: &mut Vec<Node<Span<ID>>>,
        indentation_levels: &mut Vec<IndentationMark<Span<ID>>>,
        source: &str,
        enclosing_delimiter: Option<
            &WithInsignificant<Punctuation<Span<ID>>, Span<ID>>,
        >,
        handler: &dyn Handler<error::Error<ID>>,
    ) {
        let mut index = 0;

        while index < tokens.len() {
            // check for the new indentation level
            if let (
                Node::Leaf(Token {
                    token:
                        Kind::Punctuation(Punctuation { punctuation: ':', .. }),
                    ..
                }),
                true,
            ) = (
                &tokens[index],
                tokens.get(index + 1).is_some_and(|x| {
                    x.as_leaf().is_some_and(|x| x.is_new_line())
                }),
            ) {
                index = Self::handle_possible_indentation(
                    tokens,
                    source,
                    indentation_levels,
                    index,
                    handler,
                );
            }
            // recursively handle the indentation in the delimited fragmnet
            else if tokens[index].is_fragment() {
                let fragmnet = tokens[index].as_fragment_mut().unwrap();
                assert!(fragmnet.kind.is_delimiter());

                Self::handle_indentation(
                    &mut fragmnet.token_stream.0,
                    &mut vec![],
                    source,
                    Some(&fragmnet.kind.as_delimiter().unwrap().close),
                    handler,
                );

                index += 1;
            }
            // handle indentation level
            else if tokens[index].as_leaf().is_some_and(|x| x.is_new_line()) {
                index = Self::handle_new_indentation_line(
                    tokens,
                    indentation_levels,
                    index,
                    source,
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
                indentation_levels.len(),
                enclosing_delimiter,
                tokens.len(),
            );
        }
    }

    fn pop_token(
        tokens: &mut Vec<Token<Span<ID>>>,
        source_file: &str,
        handler: &dyn Handler<error::Error<ID>>,
    ) -> Option<Node<Span<ID>>> {
        tokens.pop().and_then(|x| {
            Self::handle_popped_token(tokens, x, source_file, handler)
        })
    }

    fn handle_popped_token(
        tokens: &mut Vec<Token<Span<ID>>>,
        popped_token: Token<Span<ID>>,
        source_file: &str,
        handler: &dyn Handler<error::Error<ID>>,
    ) -> Option<Node<Span<ID>>> {
        match popped_token {
            Token {
                token:
                    Kind::Punctuation(pun @ Punctuation { punctuation: '{', .. }),
                prior_insignificant,
            } => Self::handle_delimited(
                tokens,
                WithInsignificant::new(pun, prior_insignificant),
                DelimiterKind::Brace,
                source_file,
                handler,
            )
            .map(Node::Fragment),

            Token {
                token:
                    Kind::Punctuation(pun @ Punctuation { punctuation: '[', .. }),
                prior_insignificant,
            } => Self::handle_delimited(
                tokens,
                WithInsignificant::new(pun, prior_insignificant),
                DelimiterKind::Bracket,
                source_file,
                handler,
            )
            .map(Node::Fragment),

            Token {
                token:
                    Kind::Punctuation(pun @ Punctuation { punctuation: '(', .. }),
                prior_insignificant,
            } => Self::handle_delimited(
                tokens,
                WithInsignificant::new(pun, prior_insignificant),
                DelimiterKind::Parenthesis,
                source_file,
                handler,
            )
            .map(Node::Fragment),

            token => Some(Node::Leaf(token)),
        }
    }

    fn handle_delimited(
        tokens: &mut Vec<Token<Span<ID>>>,
        open: WithInsignificant<Punctuation<Span<ID>>, Span<ID>>,
        delimiter: DelimiterKind,
        source_file: &str,
        handler: &dyn Handler<error::Error<ID>>,
    ) -> Option<Fragment<Span<ID>>> {
        let mut token_trees = Vec::new();

        while let Some(token) = tokens.pop() {
            match (token, delimiter) {
                (
                    Token {
                        token:
                            Kind::Punctuation(
                                close @ Punctuation { punctuation: ')', .. },
                            ),
                        prior_insignificant,
                    },
                    DelimiterKind::Parenthesis,
                )
                | (
                    Token {
                        token:
                            Kind::Punctuation(
                                close @ Punctuation { punctuation: '}', .. },
                            ),
                        prior_insignificant,
                    },
                    DelimiterKind::Brace,
                )
                | (
                    Token {
                        token:
                            Kind::Punctuation(
                                close @ Punctuation { punctuation: ']', .. },
                            ),
                        prior_insignificant,
                    },
                    DelimiterKind::Bracket,
                ) => {
                    return Some(Fragment {
                        token_stream: Self(token_trees),
                        kind: FragmentKind::Delimiter(Delimiter {
                            open,
                            close: WithInsignificant::new(
                                close,
                                prior_insignificant,
                            ),
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
            UndelimitedDelimiter { opening_span: open.token.span, delimiter },
        ));

        None
    }
}

#[cfg(test)]
mod test;
