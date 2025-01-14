use std::sync::Arc;

use pernixc_handler::Storage;
use pernixc_source_file::SourceFile;
use pernixc_test_input::Input;
use proptest::{prelude::Arbitrary, proptest};

use crate::error::Error;

proptest! {
    #[test]
    #[allow(clippy::ignored_unit_patterns)]
    fn token_stream_test(
        input in super::strategy::TokenStream::arbitrary()
    ) {
        let source = input.to_string();
        let source_file = Arc::new(SourceFile::new(source, "test".into()));

        let storage: Storage<Error> = Storage::new();
        let token_stream =
            super::TokenStream::tokenize(source_file, &storage);

        input.assert(&token_stream)?;
    }
}
