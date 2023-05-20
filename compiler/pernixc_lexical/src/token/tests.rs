use std::{path::PathBuf, str::FromStr};

use pernixc_source::SourceFile;
use pernixc_system::diagnostic::Storage;
use proptest::{prop_assert, prop_assert_eq, prop_oneof, proptest, strategy::Strategy};
use strum::IntoEnumIterator;

use super::KeywordKind;
use crate::{
    error::Error,
    token::{CommentKind, Token, TokenizationError},
};

/// Returns a strategy that generates a valid identifier string
pub fn identifier_strategy() -> impl Strategy<Value = String> {
    proptest::collection::vec(
        proptest::char::any().prop_filter("allows only identifier character", |x| {
            Token::is_identifier_character(*x)
        }),
        0..=10,
    )
    .prop_map(|vec| vec.into_iter().collect::<String>())
    .prop_filter(
        "filter out first strings that have digit as the first character and empty string",
        |x| x.chars().next().map_or(false, |x| !x.is_ascii_digit()),
    )
    .prop_filter(
        "filter out identifiers that can be used as a keyword",
        |x| KeywordKind::from_str(x).is_err(),
    )
}

/// Returns a strategy that generates one of the variants of the [`KeywordKind`]
pub fn keyword_kind_strategy() -> impl Strategy<Value = KeywordKind> {
    proptest::sample::select(KeywordKind::iter().collect::<Vec<_>>())
}

/// Returns a strategy that generates a valid numeric literal value string
pub fn numeric_literal_value_strategy() -> impl Strategy<Value = String> {
    prop_oneof![
        proptest::num::f64::ANY.prop_filter_map("filter out negative numbers and inf", |x| {
            if x.is_finite() && x.is_sign_positive() {
                Some(x.to_string())
            } else {
                None
            }
        }),
        proptest::num::u64::ANY.prop_map(|x| x.to_string())
    ]
}

fn delimited_comment_strategy() -> impl Strategy<Value = String> {
    "[^\r]*".prop_filter("must not contain */", |x| !x.contains("*/"))
}

fn tokenize(source: String) -> Result<Token, proptest::test_runner::TestCaseError> {
    let source_file = SourceFile::new(PathBuf::new(), "test".to_string(), source, vec![
        "test".to_string()
    ])?;

    let mut iterator = source_file.iter();

    let error_storage: Storage<Error> = Storage::new();
    let token = Token::tokenize(&mut iterator, &error_storage)?;

    // no errors
    prop_assert!(error_storage.as_vec().is_empty());

    Ok(token)
}

proptest! {
    #[test]
    fn identifier_test(source in identifier_strategy()) {
        let token = tokenize(source.clone())?.into_identifier().unwrap();

        // str is equal to source
        prop_assert_eq!(token.span.str(), source);
    }

    #[test]
    fn keyword_test(keyword in keyword_kind_strategy()) {
        let token = tokenize(keyword.to_string())?.into_keyword().unwrap();

        // keyword is equal to source
        prop_assert_eq!(token.keyword, keyword);
        prop_assert_eq!(token.span.str(), keyword.as_str());
    }

    #[test]
    fn numeric_literal_test(
        value in numeric_literal_value_strategy(),
        optional_suffix in proptest::option::of(identifier_strategy())
    ) {
        let source = format!("{}{}", value, optional_suffix.clone().unwrap_or_default());
        let token = tokenize(source)?.into_numeric_literal().unwrap();

        // value is equal to source
        prop_assert_eq!(token.value_span.str(), value);

        if let Some(prop_suffix) = optional_suffix {
            let token_suffix = token.suffix_span.unwrap();

            prop_assert_eq!(token_suffix.str(), prop_suffix);
        }
    }

    #[test]
    fn line_comment_test(
        mut comment in "[^\\n\\r]*",
        new_line_terminated in proptest::bool::ANY
    ) {
        if new_line_terminated {
            comment.push(SourceFile::NEW_LINE);
        }
        let source = format!("//{comment}");
        let token = tokenize(source.clone())?.into_comment().unwrap();

        prop_assert_eq!(token.kind, CommentKind::SingleLine);
        prop_assert_eq!(token.span.str(), source);
    }

    #[test]
    fn delimited_comment_test(
        comment in delimited_comment_strategy()
    ) {
        let source = format!("/*{comment}*/");
        let token = tokenize(source.clone())?.into_comment().unwrap();

        prop_assert_eq!(token.kind, CommentKind::Delimited);
        prop_assert_eq!(token.span.str(), source);
    }

    #[test]
    fn undelimiated_comment_test(
        comment in delimited_comment_strategy()
    ) {
        let source = format!("/*{comment}");
        let source_file = SourceFile::new(
            PathBuf::new(),
            "test".to_string(),
            source, vec!["test".to_string()]
        )?;
        let mut iter = source_file.iter();

        let storage: Storage<Error> = Storage::new();
        let result = Token::tokenize(&mut iter, &storage).err();

        prop_assert_eq!(result, Some(TokenizationError::FatalLexicalError));

        let error = {
            let mut vec = storage.as_vec_mut();
            prop_assert_eq!(vec.len(), 1);
            vec.pop().unwrap().into_unterminated_delimited_comment().unwrap()
        };

        prop_assert_eq!(error.span.str(), "/*");
    }
}
