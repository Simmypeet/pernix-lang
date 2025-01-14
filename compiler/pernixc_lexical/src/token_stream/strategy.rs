#![allow(missing_docs)]

use std::fmt::{Display, Write};

use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Just, Strategy, TestCaseError},
    prop_assert_eq, prop_oneof,
    test_runner::TestCaseResult,
};

use crate::token;

/// Represents an input for the [`super::Delimiter`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Delimiter {
    delimiter: super::Delimiter,
}

impl Arbitrary for Delimiter {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self { delimiter: super::Delimiter::Parenthesis }),
            Just(Self { delimiter: super::Delimiter::Brace }),
            Just(Self { delimiter: super::Delimiter::Bracket }),
        ]
        .boxed()
    }
}

/// Represents an input for the [`super::Delimited`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Delimited {
    /// The type of the delimiter of the input.
    pub delimiter: Delimiter,

    /// The token stream inside the delimiter.
    pub token_stream: TokenStream,
}

impl Arbitrary for Delimited {
    type Parameters = Option<BoxedStrategy<TokenStream>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(
        token_stream_strategy: Self::Parameters,
    ) -> Self::Strategy {
        (
            Delimiter::arbitrary(),
            token_stream_strategy.unwrap_or_else(TokenStream::arbitrary),
        )
            .prop_map(|(delimiter, token_stream)| Self {
                delimiter,
                token_stream,
            })
            .boxed()
    }
}
impl Display for Delimited {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.delimiter.delimiter {
            super::Delimiter::Parenthesis => f.write_char('(')?,
            super::Delimiter::Brace => f.write_char('{')?,
            super::Delimiter::Bracket => f.write_char('[')?,
        }

        Display::fmt(&self.token_stream, f)?;

        match self.delimiter.delimiter {
            super::Delimiter::Parenthesis => f.write_char(')'),
            super::Delimiter::Brace => f.write_char('}'),
            super::Delimiter::Bracket => f.write_char(']'),
        }
    }
}

impl Input<&super::Delimited> for &Delimited {
    fn assert(self, output: &super::Delimited) -> TestCaseResult {
        prop_assert_eq!(self.delimiter.delimiter, output.delimiter);

        let (open_char, close_char) = match self.delimiter.delimiter {
            super::Delimiter::Parenthesis => ('(', ')'),
            super::Delimiter::Brace => ('{', '}'),
            super::Delimiter::Bracket => ('[', ']'),
        };

        prop_assert_eq!(output.open.punctuation, open_char);
        prop_assert_eq!(output.close.punctuation, close_char);

        self.token_stream.assert(&output.token_stream)?;

        Ok(())
    }
}

/// Represents an input for the [`super::TokenTree`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum TokenTree {
    Token(token::strategy::Token),
    Delimited(Delimited),
}

impl Arbitrary for TokenTree {
    type Parameters = Option<BoxedStrategy<TokenStream>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            token::strategy::Token::arbitrary().prop_map(Self::Token),
            Delimited::arbitrary_with(args).prop_map(Self::Delimited)
        ]
        .boxed()
    }
}

impl Display for TokenTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Token(t) => Display::fmt(t, f),
            Self::Delimited(d) => Display::fmt(d, f),
        }
    }
}

impl Input<&super::TokenKind> for &TokenTree {
    fn assert(self, output: &super::TokenKind) -> TestCaseResult {
        match (self, output) {
            (TokenTree::Token(i), super::TokenKind::Token(o)) => i.assert(o)?,
            (TokenTree::Delimited(i), super::TokenKind::Delimited(o)) => {
                i.assert(o)?;
            }
            _ => {
                return Err(TestCaseError::fail("token tree variant mismatch"))
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
enum InsignificantToken {
    Comment(token::strategy::Comment),
    WhiteSpaces(token::strategy::WhiteSpaces),
}

impl From<InsignificantToken> for token::strategy::Token {
    fn from(val: InsignificantToken) -> Self {
        match val {
            InsignificantToken::Comment(c) => Self::Comment(c),
            InsignificantToken::WhiteSpaces(w) => Self::WhiteSpaces(w),
        }
    }
}

impl Arbitrary for InsignificantToken {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            token::strategy::Comment::arbitrary().prop_map(Self::Comment),
            token::strategy::WhiteSpaces::arbitrary()
                .prop_map(Self::WhiteSpaces),
        ]
        .boxed()
    }
}

impl Display for InsignificantToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Comment(c) => Display::fmt(c, f),
            Self::WhiteSpaces(w) => Display::fmt(w, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum SignificantToken {
    Identifier(token::strategy::Identifier),
    Keyword(token::strategy::Keyword),
    NumericLiteral(token::strategy::Numeric),
    Punctuation(token::strategy::Punctuation),
    Character(token::strategy::Character),
    String(token::strategy::String),
}

impl From<SignificantToken> for token::strategy::Token {
    fn from(val: SignificantToken) -> Self {
        match val {
            SignificantToken::Identifier(i) => Self::Identifier(i),
            SignificantToken::Keyword(k) => Self::Keyword(k),
            SignificantToken::NumericLiteral(n) => Self::NumericLiteral(n),
            SignificantToken::Punctuation(p) => Self::Punctuation(p),
            SignificantToken::Character(c) => Self::Character(c),
            SignificantToken::String(s) => Self::String(s),
        }
    }
}

impl Arbitrary for SignificantToken {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            token::strategy::Identifier::arbitrary().prop_map(Self::Identifier),
            token::strategy::Keyword::arbitrary().prop_map(Self::Keyword),
            token::strategy::Numeric::arbitrary()
                .prop_map(Self::NumericLiteral),
            token::strategy::Punctuation::arbitrary().prop_filter_map(
                "filters out the punctuation that might collide with the \
                 delimiters",
                |p| {
                    if matches!(
                        p.punctuation,
                        '(' | ')' | '{' | '}' | '[' | ']'
                    ) {
                        None
                    } else {
                        Some(Self::Punctuation(p))
                    }
                }
            ),
            token::strategy::Character::arbitrary().prop_map(Self::Character),
            token::strategy::String::arbitrary().prop_map(Self::String),
        ]
        .boxed()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TokenStream {
    pub token_trees: Vec<TokenTree>,
}

#[derive(Debug, Clone)]
enum TokenStreamPart {
    Tokens(SignificantToken, InsignificantToken),
    Delimited(Delimited),
}

impl Arbitrary for TokenStream {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let leaf = Just(Self { token_trees: vec![] });
        leaf.prop_recursive(4, 24, 6, |inner| {
            proptest::collection::vec(
                prop_oneof![
                    (
                        SignificantToken::arbitrary(),
                        InsignificantToken::arbitrary()
                    )
                        .prop_filter_map(
                            "filter out grammar ambiguity",
                            |(s, i)| {
                                match (s, i) {
                                    (
                                        SignificantToken::Punctuation(p),
                                        InsignificantToken::Comment(..),
                                    ) if p.punctuation == '/' => None,
                                    (s, i) => {
                                        Some(TokenStreamPart::Tokens(s, i))
                                    }
                                }
                            }
                        ),
                    Delimited::arbitrary_with(Some(inner))
                        .prop_map(TokenStreamPart::Delimited),
                ],
                0..=6,
            )
            .prop_map(|token_parts| {
                let mut tokens = Vec::new();
                for token_part in token_parts {
                    match token_part {
                        TokenStreamPart::Tokens(sig, insignificant) => {
                            tokens.push(TokenTree::Token(sig.into()));
                            tokens.push(TokenTree::Token(insignificant.into()));
                        }
                        TokenStreamPart::Delimited(delimited) => {
                            tokens.push(TokenTree::Delimited(delimited));
                        }
                    }
                }

                Self { token_trees: tokens }
            })
        })
        .boxed()
    }
}

impl Display for TokenStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for token in &self.token_trees {
            Display::fmt(token, f)?;
        }

        Ok(())
    }
}

impl Input<&super::TokenStream> for &TokenStream {
    fn assert(self, output: &super::TokenStream) -> TestCaseResult {
        self.token_trees.assert(&output.tokens)
    }
}
