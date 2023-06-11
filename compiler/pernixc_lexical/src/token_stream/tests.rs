use pernixc_source::SourceFile;
use pernixc_system::{diagnostic::Storage, input::Input};
use proptest::{proptest, prelude::Arbitrary};

use crate::{error::Error, token_stream::TokenStream};

proptest! {
    #[test]
    fn token_test(
        input in super::input::TokenStream::arbitrary()
    ) {
        let source = input.to_string(); 
        let source_file = SourceFile::temp(&source)?;

        let storage: Storage<Error> = Storage::new();
        let token_stream = TokenStream::tokenize(&source_file, &storage);

        input.assert(&token_stream)?;
    }
}
