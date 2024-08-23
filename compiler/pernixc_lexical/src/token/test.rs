use std::sync::Arc;

use pernixc_base::{diagnostic::Storage, source_file::SourceFile};
use pernixc_tests::input::Input;
use proptest::{
    prelude::Arbitrary, prop_assert, proptest, test_runner::TestCaseError,
};

fn tokenize(
    source: std::string::String,
) -> Result<super::Token, proptest::test_runner::TestCaseError> {
    let source_file = Arc::new(SourceFile::temp(source)?);
    let mut iterator = source_file.iter();

    let error_storage: Storage<super::error::Error> = Storage::new();
    let token = match super::Token::lex(&mut iterator, &error_storage) {
        Ok(token) => token,
        Err(error) => {
            return Err(TestCaseError::fail(format!(
                "failed to tokenize the source code: {}, errors: {:?}",
                error,
                error_storage.as_vec()
            )))
        }
    };

    prop_assert!(iterator.next().is_none());

    // no errors
    prop_assert!(
        error_storage.as_vec().is_empty(),
        "{:?}",
        error_storage.as_vec()
    );

    Ok(token)
}

proptest! {
    #[test]
    #[allow(clippy::ignored_unit_patterns)]
    fn token(
        input in super::strategy::Token::arbitrary()
    ) {
        let source = input.to_string();
        let token = tokenize(source)?;

        input.assert(&token)?;
    }
}
