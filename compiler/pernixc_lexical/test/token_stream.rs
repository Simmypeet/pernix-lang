use pernix_input::Input;
use pernixc_lexical::error::Error;
use pernixc_source::SourceFile;
use pernixc_system::diagnostic::Storage;
use proptest::{prelude::Arbitrary, proptest};

proptest! {
    #[test]
    fn token_stream_test(
        input in pernix_lexical_input::token_stream::TokenStream::arbitrary()
    ) {
        let source = input.to_string();
        let source_file = SourceFile::temp(&source)?;

        let storage: Storage<Error> = Storage::new();
        let token_stream =
            pernixc_lexical::token_stream::TokenStream::tokenize(&source_file, &storage);

        input.assert(&token_stream)?;
    }
}
