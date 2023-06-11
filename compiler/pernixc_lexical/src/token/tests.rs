use pernixc_source::SourceFile;
use pernixc_system::{diagnostic::Storage, input::Input};
use proptest::{arbitrary::Arbitrary, prop_assert, proptest};

use crate::{error::Error, token::Token};

fn tokenize(source: &str) -> Result<Token, proptest::test_runner::TestCaseError> {
    let source_file = SourceFile::temp(source)?;
    let mut iterator = source_file.iter();

    let error_storage: Storage<Error> = Storage::new();
    let token = Token::tokenize(&mut iterator, &error_storage)?;

    // no errors
    prop_assert!(error_storage.as_vec().is_empty());

    Ok(token)
}

proptest! {
    #[test]
    fn token_test(
        input in super::input::Token::arbitrary()
    ) {
        let source = input.to_string();
        let token = tokenize(&source)?;

        input.assert(&token)?;
    }
}
