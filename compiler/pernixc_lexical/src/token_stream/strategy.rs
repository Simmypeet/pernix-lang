#![allow(missing_docs)]

use std::fmt::{Display, Formatter};

use enum_as_inner::EnumAsInner;
use pernixc_test_input::Input;
use proptest::{
    bits::usize,
    prelude::{Arbitrary, BoxedStrategy, Just, Strategy, TestCaseError},
    prop_assert, prop_assert_eq, prop_oneof,
};

use super::{
    super::token::strategy::{Comment, Token},
    DelimiterKind,
};
use crate::token::{
    strategy::{Keyword, Punctuation, WhiteSpaces},
    KeywordKind,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Indentation {
    pub size: usize,
    pub block: IndentationBlock,
}

impl Input<&super::Fragment> for &Indentation {
    fn assert(
        self,
        output: &super::Fragment,
    ) -> proptest::test_runner::TestCaseResult {
        self.assert_internal(output, 0)
    }
}

impl Indentation {
    fn assert_internal(
        &self,
        output: &super::Fragment,
        prev_size: usize,
    ) -> proptest::test_runner::TestCaseResult {
        prop_assert!(output.kind.is_indentation());

        let mut i = 0;

        for a in &self.block.lines {
            match a {
                IndentationLine::Normal(token_stream, end_with_indentation) => {
                    if self.size > 0 {
                        let current = &output.token_stream[i];
                        let whitespace = current
                            .as_token()
                            .and_then(|x| x.as_white_spaces())
                            .ok_or(TestCaseError::fail(
                                "expected whitespace for \
                                 `IndentationLine::Normal`",
                            ))?;

                        prop_assert_eq!(
                            whitespace.span.str().len(),
                            self.size + prev_size
                        );

                        i += 1;
                    }

                    for token in &token_stream.tokens {
                        let current = &output.token_stream[i];
                        token.assert(current)?;

                        i += 1;
                    }

                    if let Some(indentation) = end_with_indentation {
                        let current = &output.token_stream[i];
                        let fragment = current.as_fragment().ok_or(
                            TestCaseError::fail(
                                "expected fragment for \
                                 `IndentationLine::Normal`",
                            ),
                        )?;

                        indentation
                            .assert_internal(fragment, prev_size + self.size)?;
                    } else {
                        prop_assert!(&output.token_stream[i]
                            .as_token()
                            .and_then(|x| x.as_new_line())
                            .is_some());
                    }

                    i += 1;
                }

                IndentationLine::WhiteSpaces(whitespace_len) => {
                    let current = &output.token_stream[i];
                    let whitespace = current
                        .as_token()
                        .and_then(|x| x.as_white_spaces())
                        .ok_or(TestCaseError::fail(
                            "expected whitespace for \
                             `IndentationLine::Whiitespaces`",
                        ))?;

                    prop_assert_eq!(
                        whitespace.span.str().len(),
                        *whitespace_len
                    );

                    i += 1;

                    prop_assert!(&output.token_stream[i]
                        .as_token()
                        .and_then(|x| x.as_new_line())
                        .is_some());

                    i += 1;
                }
            }
        }

        prop_assert_eq!(i, output.token_stream.len());

        Ok(())
    }
}

impl Arbitrary for Indentation {
    type Parameters = Option<BoxedStrategy<TokenStream>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let indentation_leaf = Just(Self {
            size: 4,
            block: IndentationBlock {
                lines: vec![IndentationLine::Normal(
                    TokenStream {
                        tokens: vec![TokenKind::Token(Token::Keyword(
                            Keyword { keyword: KeywordKind::Pass },
                        ))],
                    },
                    None,
                )],
            },
        });

        indentation_leaf
            .prop_recursive(6, 96, 16, move |inner| {
                (
                    2usize..=10usize,
                    IndentationBlock::arbitrary_with((
                        args.clone(),
                        Some(inner),
                    )),
                )
                    .prop_map(|(x, y)| Self { size: x, block: y })
            })
            .boxed()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IndentationBlock {
    pub lines: Vec<IndentationLine>,
}

impl Arbitrary for IndentationBlock {
    type Parameters = (
        Option<BoxedStrategy<TokenStream>>,
        Option<BoxedStrategy<Indentation>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(IndentationLine::arbitrary_with(args), 1..20)
            .prop_filter("must at least have a token", |x| {
                x.iter().any(|x| {
                    x.as_normal().is_some_and(|x| {
                        x.0.tokens.iter().any(|x| match x {
                            TokenKind::Token(
                                Token::Character(_)
                                | Token::Identifier(_)
                                | Token::Nueric(_)
                                | Token::Keyword(_)
                                | Token::Punctuation(_)
                                | Token::String(_),
                            )
                            | TokenKind::Fragment(_) => true,

                            TokenKind::Token(
                                Token::Comment(_)
                                | Token::NewLine(_)
                                | Token::WhiteSpaces(_),
                            ) => false,
                        })
                    })
                })
            })
            .prop_filter("last line should not be empty", |x| {
                x.last().is_some_and(|x| {
                    !x.is_white_spaces()
                        && x.as_normal().is_some_and(|x| {
                            !x.0.tokens.is_empty() || x.1.is_some()
                        })
                })
            })
            .prop_map(|x| Self { lines: x })
            .boxed()
    }
}

impl IndentationBlock {
    fn fmt_internal(
        &self,
        f: &mut Formatter,
        indentation_size: usize,
    ) -> std::fmt::Result {
        for line in &self.lines {
            match line {
                IndentationLine::Normal(token_stream, end_with_indentation) => {
                    for _ in 0..indentation_size {
                        write!(f, " ")?;
                    }

                    token_stream.fmt_internal(f, Some(indentation_size))?;

                    if let Some(indentation) = end_with_indentation {
                        indentation.fmt_internal(f, indentation_size)?;
                    } else {
                        writeln!(f)?;
                    }
                }

                IndentationLine::WhiteSpaces(size) => {
                    for _ in 0..*size {
                        write!(f, " ")?;
                    }

                    writeln!(f)?;
                }
            }
        }

        Ok(())
    }
}

impl Display for IndentationBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.fmt_internal(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum IndentationLine {
    Normal(TokenStream, Option<Indentation>),
    WhiteSpaces(usize),
}

impl Arbitrary for IndentationLine {
    type Parameters = (
        Option<BoxedStrategy<TokenStream>>,
        Option<BoxedStrategy<Indentation>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(
        (token_stream, indentation): Self::Parameters,
    ) -> Self::Strategy {
        let indentation = indentation.unwrap_or_else(|| {
            Indentation::arbitrary_with(token_stream.clone())
        });

        let token_stream = token_stream.unwrap_or_else(TokenStream::arbitrary);

        prop_oneof![
            6 => (token_stream
                    .prop_map(|mut x| {
                        x.sanitize(true);
                        x
                    }),
                    proptest::option::weighted(0.5, indentation)
                )

                .prop_map(|(x, y)| Self::Normal(x, y)),
            1 => (1usize..=10usize).prop_map(Self::WhiteSpaces),
        ]
        .boxed()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Delimited {
    pub delimiter: DelimiterKind,
    pub token_stream: TokenStream,
}

impl Arbitrary for Delimited {
    type Parameters = Option<BoxedStrategy<TokenStream>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let delimiter_kind = prop_oneof![
            Just(DelimiterKind::Parenthesis),
            Just(DelimiterKind::Brace),
            Just(DelimiterKind::Bracket),
        ];

        (
            delimiter_kind,
            args.unwrap_or_else(TokenStream::arbitrary).prop_map(|mut x| {
                x.sanitize(false);
                x
            }),
        )
            .prop_map(|(delimiter, token_stream)| Self {
                delimiter,
                token_stream,
            })
            .boxed()
    }
}

impl Input<&super::Fragment> for &Delimited {
    fn assert(
        self,
        output: &super::Fragment,
    ) -> proptest::test_runner::TestCaseResult {
        let delimiter = output.kind.as_delimiter().ok_or(
            TestCaseError::fail("expected `Delimited` for `Fragment`"),
        )?;

        prop_assert_eq!(self.delimiter, delimiter.delimiter);

        self.token_stream.assert(&output.token_stream)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Fragment {
    Delimited(Delimited),
    Indentation(Indentation),
}

impl Input<&super::Fragment> for &Fragment {
    fn assert(
        self,
        output: &super::Fragment,
    ) -> proptest::test_runner::TestCaseResult {
        match self {
            Fragment::Delimited(a) => a.assert(output),
            Fragment::Indentation(a) => a.assert(output),
        }
    }
}

impl Arbitrary for Fragment {
    type Parameters = Option<BoxedStrategy<TokenStream>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Delimited::arbitrary_with(args.clone())
                .prop_map(Fragment::Delimited),
            Indentation::arbitrary_with(args).prop_map(Fragment::Indentation),
        ]
        .boxed()
    }
}

/// Represents an input for the [`super::TokenTree`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum TokenKind {
    Token(Token),
    Fragment(Fragment),
}

impl Arbitrary for TokenKind {
    type Parameters = Option<BoxedStrategy<TokenStream>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            4 => Token::arbitrary().prop_map(TokenKind::Token),
            1 => Fragment::arbitrary_with(args).prop_map(TokenKind::Fragment),
        ]
        .boxed()
    }
}

impl Input<&super::TokenKind> for &TokenKind {
    fn assert(
        self,
        output: &super::TokenKind,
    ) -> proptest::test_runner::TestCaseResult {
        match (self, output) {
            (TokenKind::Token(a), super::TokenKind::Token(b)) => a.assert(b),
            (TokenKind::Fragment(a), super::TokenKind::Fragment(b)) => {
                a.assert(b)
            }
            (a, b) => Err(TestCaseError::fail(format!("{a:?} != {b:?}"))),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TokenStream {
    pub tokens: Vec<TokenKind>,
}

impl Arbitrary for TokenStream {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let leaf = proptest::collection::vec(
            Token::arbitrary().prop_map(TokenKind::Token),
            0..10,
        )
        .prop_map(|x| Self { tokens: x });

        leaf.prop_recursive(6, 100, 10, |inner| {
            proptest::collection::vec(
                TokenKind::arbitrary_with(Some(inner)),
                0..10,
            )
            .prop_map(|x| Self { tokens: x })
        })
        .boxed()
    }
}

impl Display for TokenStream {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.fmt_internal(f, None)
    }
}

impl Input<&super::TokenStream> for &TokenStream {
    fn assert(
        self,
        output: &super::TokenStream,
    ) -> proptest::test_runner::TestCaseResult {
        prop_assert_eq!(
            self.tokens.len(),
            output.tokens.len(),
            "{:#?} != {:#?}",
            self,
            output
        );

        for (a, b) in self.tokens.iter().zip(&output.tokens) {
            match (a, b) {
                (TokenKind::Token(a), super::TokenKind::Token(b)) => {
                    a.assert(b)?;
                }

                (TokenKind::Fragment(a), super::TokenKind::Fragment(b)) => {
                    a.assert(b)?;
                }

                _ => {
                    return Err(TestCaseError::fail(format!("{a:?} != {b:?}")))
                }
            }
        }

        Ok(())
    }
}

impl Delimited {
    fn fmt_internal(&self, f: &mut Formatter) -> std::fmt::Result {
        let (open, close) = match self.delimiter {
            DelimiterKind::Parenthesis => ('(', ')'),
            DelimiterKind::Brace => ('{', '}'),
            DelimiterKind::Bracket => ('[', ']'),
        };

        write!(f, "{open}")?;
        self.token_stream.fmt_internal(f, None)?;
        write!(f, "{close}")?;

        Ok(())
    }
}

impl Indentation {
    fn fmt_internal(
        &self,
        f: &mut Formatter,
        prev_indentation_level: usize,
    ) -> std::fmt::Result {
        writeln!(f, ":")?;
        self.block.fmt_internal(f, prev_indentation_level + self.size)
    }
}

impl Display for Indentation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.fmt_internal(f, 0)
    }
}

impl TokenStream {
    fn fmt_internal(
        &self,
        f: &mut Formatter,
        indentation_level: Option<usize>,
    ) -> std::fmt::Result {
        for token in &self.tokens {
            match token {
                TokenKind::Token(token) => write!(f, "{token}")?,

                TokenKind::Fragment(delimited) => match delimited {
                    Fragment::Delimited(delimited) => {
                        delimited.fmt_internal(f)?;
                    }
                    Fragment::Indentation(indentation) => {
                        indentation
                            .fmt_internal(f, indentation_level.unwrap_or(0))?;
                    }
                },
            }
        }

        Ok(())
    }
}

fn get_indentation_level(tokens: &[TokenKind]) -> (usize, Option<usize>) {
    let mut size = 0;
    for (idx, token) in tokens.iter().enumerate() {
        match token {
            TokenKind::Token(token) => match token {
                // no indentation level here, start a new line
                Token::Comment(Comment::Line(_)) | Token::NewLine(_) => {
                    size = 0;
                }

                Token::Character(_)
                | Token::String(_)
                | Token::Nueric(_)
                | Token::Punctuation(_)
                | Token::Identifier(_)
                | Token::Keyword(_)
                | Token::Comment(Comment::Delimited(_)) => {
                    return (idx, Some(size))
                }

                // count the space
                Token::WhiteSpaces(whitespaces) => {
                    size += match whitespaces {
                        WhiteSpaces::Spaces(size) => *size as usize,
                        WhiteSpaces::Tabs(count) => *count as usize * 4,
                    };
                } // found a significant token
            },
            TokenKind::Fragment(_) => return (idx, Some(size)),
        }
    }

    (tokens.len(), None)
}

impl TokenStream {
    pub fn sanitize(&mut self, is_in_indent: bool) {
        let mut changed = true;
        while changed {
            changed = false;

            changed |= self.remove_delimiter_punctuations();
            changed |= self.remove_possible_indent(is_in_indent);
            if is_in_indent {
                changed |= self.sanitize_in_indent_block();
            }
            changed |= self.remove_consecutive_whitespaces();
            changed |= self.remove_consecutive_identifiers();
        }
    }

    pub fn remove_consecutive_whitespaces(&mut self) -> bool {
        let mut changed = false;
        let mut i = 0;

        while i < self.tokens.len() {
            let current = &self.tokens[i];
            let Some(next) = self.tokens.get(i + 1) else {
                break;
            };

            if current.as_token().is_some_and(Token::is_white_spaces)
                && next.as_token().is_some_and(Token::is_white_spaces)
            {
                changed = true;
                self.tokens.remove(i + 1);
            } else {
                i += 1;
            }
        }

        changed
    }

    pub fn remove_consecutive_identifiers(&mut self) -> bool {
        let mut changed = false;
        let mut i = 0;

        while i < self.tokens.len() {
            let current = &self.tokens[i];
            let Some(next) = self.tokens.get(i + 1) else {
                break;
            };

            if current.as_token().is_some_and(|x| {
                x.is_keyword() || x.is_identifier() || x.is_nueric()
            }) && next.as_token().is_some_and(|x| {
                x.is_keyword() || x.is_identifier() || x.is_nueric()
            }) {
                changed = true;
                self.tokens.remove(i + 1);
            } else {
                i += 1;
            }
        }

        changed
    }

    pub fn remove_delimiter_punctuations(&mut self) -> bool {
        let starting_len = self.tokens.len();
        self.tokens.retain(|x| {
            !x.as_token().is_some_and(|x| {
                x.as_punctuation().is_some_and(|x| {
                    ['{', '}', '[', ']', '(', ')'].contains(&x.punctuation)
                })
            })
        });

        starting_len != self.tokens.len()
    }

    #[allow(clippy::too_many_lines)]
    pub fn remove_possible_indent(&mut self, is_in_indent: bool) -> bool {
        enum NewLineSearch {
            FoundSignificant,
            EndOfTokenStream,
            Found(usize),
        }

        let mut changed = false;

        // remove colon followed by new line
        let mut i = 0;

        while i < self.tokens.len() {
            let current = &self.tokens[i];

            if !matches!(
                current,
                TokenKind::Token(Token::Punctuation(Punctuation {
                    punctuation: ':'
                }))
            ) {
                i += 1;
                continue;
            }

            let found_new_line = 'result: {
                for (j, next_tok) in self.tokens.iter().enumerate().skip(i + 1)
                {
                    match next_tok {
                        TokenKind::Token(token) => match token {
                            Token::WhiteSpaces(_)
                            | Token::Comment(Comment::Delimited(_)) => {}

                            Token::Identifier(_)
                            | Token::Keyword(_)
                            | Token::Nueric(_)
                            | Token::Punctuation(_)
                            | Token::Character(_)
                            | Token::String(_) => {
                                break 'result NewLineSearch::FoundSignificant
                            }

                            Token::Comment(Comment::Line(_))
                            | Token::NewLine(_) => {
                                break 'result NewLineSearch::Found(j)
                            }
                        },
                        TokenKind::Fragment(_) => {
                            break 'result NewLineSearch::FoundSignificant
                        }
                    }
                }

                NewLineSearch::EndOfTokenStream
            };

            match (found_new_line, is_in_indent) {
                (NewLineSearch::EndOfTokenStream, false)
                | (NewLineSearch::FoundSignificant, _) => {
                    i += 1;
                }

                (NewLineSearch::EndOfTokenStream, true) => {
                    self.tokens.splice(i.., std::iter::empty());
                    changed = true;
                    break;
                }
                (NewLineSearch::Found(new_line_index), _) => {
                    self.tokens
                        .splice((i + 1)..=new_line_index, std::iter::empty());
                    changed = true;
                }
            }
        }

        let mut i = 0;
        while i < self.tokens.len() {
            let TokenKind::Fragment(Fragment::Indentation(current)) =
                &self.tokens[i]
            else {
                i += 1;
                continue;
            };

            let (offset, Some(level)) =
                get_indentation_level(&self.tokens[(i + 1)..])
            else {
                i += 1;
                continue;
            };

            if level >= current.size {
                changed = true;
                self.tokens
                    .splice((i + 1)..=(i + 1 + offset), std::iter::empty());
            } else {
                i += 1;
            }
        }

        changed
    }

    fn sanitize_in_indent_block(&mut self) -> bool {
        let mut changed = false;
        // remove new line and comment indentation block
        let starting_len = self.tokens.len();
        self.tokens.retain(|x| {
            !x.as_token().is_some_and(|x| {
                x.is_new_line() || x.as_comment().is_some_and(Comment::is_line)
            }) && !x.as_fragment().is_some_and(Fragment::is_indentation)
        });

        changed |= starting_len != self.tokens.len();

        // remove leading whitespaces
        if self
            .tokens
            .first()
            .is_some_and(|x| x.as_token().is_some_and(Token::is_white_spaces))
        {
            changed = true;
            self.tokens.remove(0);
        }

        changed
    }
}
