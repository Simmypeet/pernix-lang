use std::sync::Arc;

use pernixc_handler::Storage;
use pernixc_source_file::SourceFile;
use pernixc_test_input::Input;
use proptest::{prelude::Arbitrary, prop_assert, proptest};

use crate::error::Error;

proptest! {
   #[test]
    fn token_stream(
        input in super::strategy::Indentation::arbitrary()
    ) {
        let source = input.to_string();
        let source_file = Arc::new(SourceFile::new(source, "test".into()));

        let storage = Storage::<Error>::new();
        let token_stream = super::TokenStream::tokenize(
            source_file,
            &storage
        );

        let storage = storage.as_vec();
        prop_assert!(storage.is_empty(), "{storage:?}");

        input.assert(token_stream.tokens[0].as_fragment().unwrap())?;
    }
}

const SOURCE: &str = r":
  |:
    (:
    pass)
";

#[test]
fn another_test() {
    let source_file =
        Arc::new(SourceFile::new(SOURCE.to_string(), "test".into()));

    let storage = Storage::<Error>::new();
    let token_stream = super::TokenStream::tokenize(source_file, &storage);

    let storage = storage.as_vec();
    assert!(storage.is_empty(), "{storage:?}");

    dbg!(token_stream);
}
