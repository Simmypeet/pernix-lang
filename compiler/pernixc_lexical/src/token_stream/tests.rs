use std::{collections::HashMap, path::PathBuf};

use pernixc_source::SourceFile;
use pernixc_system::diagnostic::Storage;
use proptest::{
    prop_assert_eq, prop_oneof, proptest,
    strategy::{Just, Strategy},
    test_runner::TestCaseError,
};

use super::Delimited;
use crate::{
    error::Error,
    token::Token,
    token_stream::{Delimiter, TokenStream, TokenTree},
};

#[derive(Debug, Clone)]
enum TokenString {
    Identifier(String),
    Keyword(String),
    NumericLiteral(String),
    Punctuation(String),
}

impl TokenString {
    fn into_string(self) -> String {
        match self {
            Self::Identifier(s)
            | Self::Keyword(s)
            | Self::NumericLiteral(s)
            | Self::Punctuation(s) => s,
        }
    }
}

#[derive(Debug, Clone)]
struct DelimitedString {
    token_tree_strings: Vec<TokenTreeString>,
    delimiter: Delimiter,
}

impl DelimitedString {
    pub fn into_string(self) -> String {
        std::iter::once(
            match self.delimiter {
                Delimiter::Parenthesis => "(",
                Delimiter::Brace => "{",
                Delimiter::Bracket => "[",
            }
            .to_string(),
        )
        .chain(
            self.token_tree_strings
                .into_iter()
                .map(TokenTreeString::into_string),
        )
        .chain(std::iter::once(
            match self.delimiter {
                Delimiter::Parenthesis => ")",
                Delimiter::Brace => "}",
                Delimiter::Bracket => "]",
            }
            .to_string(),
        ))
        .collect::<Vec<_>>()
        .join(" ")
    }
}

#[derive(Debug, Clone)]
enum TokenTreeString {
    Token(TokenString),
    Delimited(DelimitedString),
}

impl TokenTreeString {
    fn into_string(self) -> String {
        match self {
            Self::Token(token) => token.into_string(),
            Self::Delimited(delimited) => delimited.into_string(),
        }
    }
}

fn token_tree_string_strategy() -> impl Strategy<Value = TokenTreeString> {
    prop_oneof![token_string_strategy().prop_map(TokenTreeString::Token),].prop_recursive(
        8,
        256,
        10,
        |inner| {
            (proptest::collection::vec(inner, 0..10), prop_oneof![
                Just(Delimiter::Brace),
                Just(Delimiter::Bracket),
                Just(Delimiter::Parenthesis)
            ])
                .prop_map(|(token_tree_strings, delimiter)| {
                    TokenTreeString::Delimited(DelimitedString {
                        token_tree_strings,
                        delimiter,
                    })
                })
        },
    )
}

fn token_tree_string_vec_strategy() -> impl Strategy<Value = Vec<TokenTreeString>> {
    proptest::collection::vec(token_tree_string_strategy(), 0..10)
}

fn token_string_strategy() -> impl Strategy<Value = TokenString> {
    prop_oneof![
        crate::token::tests::identifier_strategy().prop_map(TokenString::Identifier),
        crate::token::tests::keyword_kind_strategy()
            .prop_map(|k| TokenString::Keyword(k.to_string())),
        crate::token::tests::numeric_literal_value_strategy().prop_map(TokenString::NumericLiteral),
        proptest::char::any().prop_filter_map("filter out delimiters", |c| {
            let not_allowed = ['_', '{', '[', '(', '}', ']', ')', '@', '\'', '"'];
            if c.is_ascii_punctuation() && !not_allowed.contains(&c) {
                Some(TokenString::Punctuation(c.to_string()))
            } else {
                None
            }
        })
    ]
}

fn check_delimited_token(
    delimited: &Delimited,
    source: &DelimitedString,
) -> Result<(), TestCaseError> {
    let open_str = delimited.open.span().str();
    let close_str = delimited.close.span().str();

    prop_assert_eq!(open_str, match source.delimiter {
        Delimiter::Brace => "{",
        Delimiter::Bracket => "[",
        Delimiter::Parenthesis => "(",
    });

    prop_assert_eq!(close_str, match source.delimiter {
        Delimiter::Brace => "}",
        Delimiter::Bracket => "]",
        Delimiter::Parenthesis => ")",
    });

    for (token, token_string) in delimited
        .token_stream
        .iter()
        .filter(|x| !matches!(x, TokenTree::Token(Token::WhiteSpace(_))))
        .zip(source.token_tree_strings.iter())
    {
        check_token_tree(token, token_string)?;
    }

    Ok(())
}

fn check_token(token: &Token, source: &TokenString) -> Result<(), TestCaseError> {
    match (token, source) {
        (Token::Identifier(token), TokenString::Identifier(source)) => {
            prop_assert_eq!(token.span.str(), source);
        }
        (Token::NumericLiteral(token), TokenString::NumericLiteral(source)) => {
            prop_assert_eq!(token.span.str(), source);
        }
        (Token::Punctuation(token), TokenString::Punctuation(source)) => {
            prop_assert_eq!(token.span.str(), source);
        }
        (Token::Keyword(token), TokenString::Keyword(source)) => {
            prop_assert_eq!(token.span.str(), source);
        }
        _ => Err(TestCaseError::fail("Token and TokenString do not match"))?,
    }

    Ok(())
}

fn check_token_tree(token: &TokenTree, source: &TokenTreeString) -> Result<(), TestCaseError> {
    match (token, source) {
        (TokenTree::Token(token), TokenTreeString::Token(source)) => check_token(token, source),
        (TokenTree::Delimited(token), TokenTreeString::Delimited(source)) => {
            check_delimited_token(token, source)
        }
        _ => Err(TestCaseError::fail(format!(
            "TokenTree and TokenTreeString do not match: {token:?} {source:?}",
        ))),
    }
}

proptest! {
    #[test]
    fn token_stream_test(token_tree in token_tree_string_vec_strategy()) {
        let source = token_tree
            .clone()
            .into_iter()
            .map(TokenTreeString::into_string)
            .collect::<Vec<_>>().join(" ");

        let source_file = SourceFile::new(
            PathBuf::new(),
            "test".to_string(),
            source, vec!["test".to_string()]
        )?;

        let storage: Storage<Error> = Storage::new();
        let token_stream = TokenStream::tokenize(&source_file, &storage);

        for (token, source) in token_stream
            .dissolve()
            .iter()
            .filter(|x| !matches!(x, TokenTree::Token(Token::WhiteSpace(_))))
            .zip(token_tree.iter()) {
            check_token_tree(token, source)?;
        }
    }
}

#[derive(Debug, Clone)]
enum DelimiterStatus {
    Proper(Delimiter),
    Unmatched(Delimiter),
}

fn delimiter_status_property() -> impl Strategy<Value = DelimiterStatus> {
    prop_oneof![
        Just(Delimiter::Brace),
        Just(Delimiter::Bracket),
        Just(Delimiter::Parenthesis),
    ]
    .prop_flat_map(|x| {
        prop_oneof![
            Just(DelimiterStatus::Proper(x)),
            Just(DelimiterStatus::Unmatched(x)),
        ]
    })
}

proptest! {
    #[test]
    fn delimiter_error_handling_test(
        delimiter_statuses in proptest::collection::vec(delimiter_status_property(), 0..10)
    ) {
        let mut source = String::new();
        let mut byte_indices_by_unmatched_index = HashMap::new();

        for (i, delimiter_status) in delimiter_statuses.iter().enumerate() {
            match delimiter_status {
                DelimiterStatus::Proper(matched) => {
                    source.push_str(match matched {
                        Delimiter::Brace => "{}",
                        Delimiter::Bracket => "[]",
                        Delimiter::Parenthesis => "()",
                    });
                }
                DelimiterStatus::Unmatched(unmatched) => {
                    byte_indices_by_unmatched_index.insert(i, source.len());
                    source.push_str(match unmatched {
                        Delimiter::Brace => "{",
                        Delimiter::Bracket => "[",
                        Delimiter::Parenthesis => "(",
                    });
                }
            }
        }

        let source_file = SourceFile::new(
            PathBuf::new(),
            "test".to_string(),
            source, vec!["test".to_string()]
        )?;

        let storage: Storage<Error> = Storage::new();
        let _: TokenStream = TokenStream::tokenize(&source_file, &storage);

        let unmatched_delimiters = {
            let mut storage = storage.as_vec_mut();
            std::mem::take(&mut *storage)
                .into_iter()
                .map(|x| x.into_undelimited_delimiter().unwrap())
                .collect::<Vec<_>>()
        };

        for (i, delimiter_status) in delimiter_statuses.into_iter().enumerate() {
            let DelimiterStatus::Unmatched(unmatched) = delimiter_status else {
                continue;
            };

            unmatched_delimiters.iter().find(|x| {
                x.delimiter == unmatched &&
                    byte_indices_by_unmatched_index.get(&i) == Some(&x.opening_span.start())
            });
        }
    }
}
