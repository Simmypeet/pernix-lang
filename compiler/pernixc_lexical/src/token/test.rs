use pernixc_arena::ID;
use pernixc_handler::Storage;
use pernixc_source_file::{LocalSourceMap, LocalSpan, SourceFile};
use pernixc_test_input::Input;
use proptest::{
    prelude::Arbitrary, prop_assert, prop_assert_eq, proptest,
    test_runner::TestCaseError,
};

use crate::token::Tokenizer;

fn tokenize(
    source: std::string::String,
) -> Result<
    (super::Token<LocalSpan>, LocalSourceMap),
    proptest::test_runner::TestCaseError,
> {
    let mut local_source_map = LocalSourceMap::new();
    let source_file = SourceFile::new(source, "test".into());

    let id = local_source_map.register(source_file).unwrap();

    let error_storage: Storage<super::error::Error<ID<SourceFile>>> =
        Storage::new();
    let mut tokenizer =
        Tokenizer::new(local_source_map[id].content(), id, &error_storage);

    let token = tokenizer
        .next()
        .ok_or_else(|| TestCaseError::fail("failed to get the first token"))?;

    prop_assert_eq!(tokenizer.next(), None);

    // no errors
    prop_assert!(
        error_storage.as_vec().is_empty(),
        "{:?}",
        error_storage.as_vec()
    );

    Ok((token, local_source_map))
}

proptest! {
    #[test]
    #[allow(clippy::ignored_unit_patterns)]
    fn token(
        input in super::arbitrary::Token::arbitrary()
    ) {
        let source = input.to_string();
        let (token, source_map) = tokenize(source)?;

        input.assert(&token, &source_map)?;
    }
}
