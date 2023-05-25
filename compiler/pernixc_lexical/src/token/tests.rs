use std::path::PathBuf;

use pernixc_source::SourceFile;
use pernixc_system::diagnostic::Storage;
use proptest::{prop_assert, prop_assert_eq, proptest};

use crate::{
    error::Error,
    token::{CommentKind, Token, TokenizationError},
};

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
    fn identifier_test(source in super::strategy::identifier()) {
        let token = tokenize(source.clone())?.into_identifier().unwrap();

        // str is equal to source
        prop_assert_eq!(token.span.str(), source);
    }

    #[test]
    fn keyword_test(keyword in super::strategy::keyword_kind()) {
        let token = tokenize(keyword.to_string())?.into_keyword().unwrap();

        // keyword is equal to source
        prop_assert_eq!(token.keyword, keyword);
        prop_assert_eq!(token.span.str(), keyword.as_str());
    }

    #[test]
    fn numeric_literal_test(
        value in super::strategy::numeric_literal_value(),
        optional_suffix in proptest::option::of(super::strategy::identifier())
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
        comment in super::strategy::delimited_comment_body()
    ) {
        let source = format!("/*{comment}*/");
        let token = tokenize(source.clone())?.into_comment().unwrap();

        prop_assert_eq!(token.kind, CommentKind::Delimited);
        prop_assert_eq!(token.span.str(), source);
    }

    #[test]
    fn undelimiated_comment_test(
        comment in super::strategy::delimited_comment_body()
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
