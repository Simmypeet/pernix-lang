use pernixc_handler::Storage;
use pernixc_source_file::{ByteIndex, SourceFile, SourceMap};
use pernixc_target::TargetID;
use pernixc_test_input::Input;
use proptest::{
    prelude::Arbitrary, prop_assert, prop_assert_eq, proptest,
    test_runner::TestCaseError,
};

use super::error::Error;
use crate::{
    kind::{self, Kind},
    token::Tokenizer,
};

fn tokenize(
    source: std::string::String,
) -> Result<
    (super::Token<Kind, ByteIndex>, SourceMap),
    proptest::test_runner::TestCaseError,
> {
    let source_map = SourceMap::new();
    let source_file = SourceFile::new(source, "test".into());

    let id = source_map.register(TargetID::Local, source_file);
    let id = TargetID::Local.make_global(id);

    let error_storage: Storage<Error> = Storage::new();

    let source_file = source_map.get_mut(id).unwrap();
    let mut tokenizer =
        Tokenizer::new(source_file.content(), id, &error_storage);

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

    // skipcq: RS-E1011
    drop(source_file);

    Ok((token, source_map))
}

proptest! {
    #[test]
    #[allow(clippy::ignored_unit_patterns)]
    fn token(
        input in super::arbitrary::Token::<kind::arbitrary::Kind>::arbitrary()
    ) {
        let source = input.to_string();
        let (token, source_map) = tokenize(source)?;

        input.assert(&token, (&source_map, ()))?;
    }
}
