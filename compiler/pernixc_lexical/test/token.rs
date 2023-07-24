use pernix_input::Input;
use pernixc_source::SourceFile;
use pernixc_system::diagnostic::Storage;
use proptest::{arbitrary::Arbitrary, prop_assert, proptest};

fn tokenize(
    source: &str,
) -> Result<pernixc_lexical::token::Token, proptest::test_runner::TestCaseError> {
    let source_file = SourceFile::temp(source)?;
    let mut iterator = source_file.iter();

    let error_storage: Storage<pernixc_lexical::error::Error> = Storage::new();
    let token = pernixc_lexical::token::Token::lex(&mut iterator, &error_storage)?;

    // no errors
    prop_assert!(error_storage.as_vec().is_empty());

    Ok(token)
}

proptest! {
    #[test]
    fn token_test(
        input in pernix_lexical_input::token::Token::arbitrary()
    ) {
        let source = input.to_string();
        let token = tokenize(&source)?;

        input.assert(&token)?;
    }
}
