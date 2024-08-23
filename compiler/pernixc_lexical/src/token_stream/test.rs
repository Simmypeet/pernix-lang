use std::sync::Arc;

use pernixc_base::{diagnostic::Storage, source_file::SourceFile};
use pernixc_tests::input::Input;
use proptest::{prelude::Arbitrary, proptest};

use crate::error::Error;

proptest! {
    #[test]
    #[allow(clippy::ignored_unit_patterns)]
    fn token_stream_test(
        input in super::strategy::TokenStream::arbitrary()
    ) {
        let source = input.to_string();
        let source_file = Arc::new(SourceFile::temp(source)?);

        let storage: Storage<Error> = Storage::new();
        let token_stream =
            super::TokenStream::tokenize(&source_file, &storage);

        input.assert(&token_stream)?;
    }
}
