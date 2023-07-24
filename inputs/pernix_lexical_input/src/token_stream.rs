//! Contains the definition of various inputs that correspond to the definitions in defined
//! [`pernixc_lexical::token_stream`] module.

use std::fmt::{Debug, Display, Write};

use pernix_input::Input;
use proptest::{
    prelude::Arbitrary,
    prop_assert_eq, prop_oneof,
    strategy::{BoxedStrategy, Just, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

/// Represents an input for the [`pernixc_lexical::token_stream::Delimiter`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Delimiter {
    delimiter: pernixc_lexical::token_stream::Delimiter,
}

impl Arbitrary for Delimiter {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self {
                delimiter: pernixc_lexical::token_stream::Delimiter::Parenthesis
            }),
            Just(Self {
                delimiter: pernixc_lexical::token_stream::Delimiter::Brace
            }),
            Just(Self {
                delimiter: pernixc_lexical::token_stream::Delimiter::Bracket
            }),
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

    fn arbitrary_with(token_stream_strategy: Self::Parameters) -> Self::Strategy {
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
            pernixc_lexical::token_stream::Delimiter::Parenthesis => f.write_char('(')?,
            pernixc_lexical::token_stream::Delimiter::Brace => f.write_char('{')?,
            pernixc_lexical::token_stream::Delimiter::Bracket => f.write_char('[')?,
        }

        Display::fmt(&self.token_stream, f)?;

        match self.delimiter.delimiter {
            pernixc_lexical::token_stream::Delimiter::Parenthesis => f.write_char(')'),
            pernixc_lexical::token_stream::Delimiter::Brace => f.write_char('}'),
            pernixc_lexical::token_stream::Delimiter::Bracket => f.write_char(']'),
        }
    }
}

impl Input for Delimited {
    type Output = pernixc_lexical::token_stream::Delimited;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(self.delimiter.delimiter, output.delimiter);

        let (open_char, close_char) = match self.delimiter.delimiter {
            pernixc_lexical::token_stream::Delimiter::Parenthesis => ('(', ')'),
            pernixc_lexical::token_stream::Delimiter::Brace => ('{', '}'),
            pernixc_lexical::token_stream::Delimiter::Bracket => ('[', ']'),
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
    Token(crate::token::Token),
    Delimited(Delimited),
}

impl Arbitrary for TokenTree {
    type Parameters = Option<BoxedStrategy<TokenStream>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            crate::token::Token::arbitrary().prop_map(Self::Token),
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

impl Input for TokenTree {
    type Output = pernixc_lexical::token_stream::TokenTree;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Token(i), pernixc_lexical::token_stream::TokenTree::Token(o)) => i.assert(o)?,
            (Self::Delimited(i), pernixc_lexical::token_stream::TokenTree::Delimited(o)) => {
                i.assert(o)?
            }
            _ => return Err(TestCaseError::fail("token tree variant mismatch")),
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
enum InsignificantToken {
    Comment(crate::token::Comment),
    WhiteSpaces(crate::token::WhiteSpaces),
}

impl From<InsignificantToken> for crate::token::Token {
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

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            crate::token::Comment::arbitrary().prop_map(Self::Comment),
            crate::token::WhiteSpaces::arbitrary().prop_map(Self::WhiteSpaces),
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
    Identifier(crate::token::Identifier),
    Keyword(crate::token::Keyword),
    NumericLiteral(crate::token::NumericLiteral),
    Punctuation(crate::token::Punctuation),
}

impl From<SignificantToken> for crate::token::Token {
    fn from(val: SignificantToken) -> Self {
        match val {
            SignificantToken::Identifier(i) => Self::Identifier(i),
            SignificantToken::Keyword(k) => Self::Keyword(k),
            SignificantToken::NumericLiteral(n) => Self::NumericLiteral(n),
            SignificantToken::Punctuation(p) => Self::Punctuation(p),
        }
    }
}

impl Arbitrary for SignificantToken {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            crate::token::Identifier::arbitrary().prop_map(Self::Identifier),
            crate::token::Keyword::arbitrary().prop_map(Self::Keyword),
            crate::token::NumericLiteral::arbitrary().prop_map(Self::NumericLiteral),
            crate::token::Punctuation::arbitrary().prop_filter_map(
                "filters out the punctuation that might collide with the delimiters",
                |p| {
                    if matches!(p.punctuation, '(' | ')' | '{' | '}' | '[' | ']') {
                        None
                    } else {
                        Some(Self::Punctuation(p))
                    }
                }
            ),
        ]
        .boxed()
    }
}

/// Represents an input for the [`super::TokenStream`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TokenStream {
    /// List of token trees in the token stream.
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

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        let leaf = Just(Self {
            token_trees: vec![],
        });
        leaf.prop_recursive(8, 64, 8, |inner| {
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
                                    (s, i) => Some(TokenStreamPart::Tokens(s, i)),
                                }
                            }
                        ),
                    Delimited::arbitrary_with(Some(inner)).prop_map(TokenStreamPart::Delimited),
                ],
                0..=8,
            )
            .prop_map(|token_parts| {
                let mut tokens = Vec::new();
                for token_part in token_parts {
                    match token_part {
                        TokenStreamPart::Tokens(sig, insig) => {
                            tokens.push(TokenTree::Token(sig.into()));
                            tokens.push(TokenTree::Token(insig.into()));
                        }
                        TokenStreamPart::Delimited(delimited) => {
                            tokens.push(TokenTree::Delimited(delimited));
                        }
                    }
                }

                Self {
                    token_trees: tokens,
                }
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

impl Input for TokenStream {
    type Output = pernixc_lexical::token_stream::TokenStream;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(self.token_trees.len(), output.len());

        for (lhs, rhs) in self.token_trees.iter().zip(output.iter()) {
            lhs.assert(rhs)?;
        }

        Ok(())
    }
}
