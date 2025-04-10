use pernixc_arena::ID;
use pernixc_handler::Storage;
use pernixc_source_file::{ByteIndex, SourceFile, SourceMap};
use pernixc_target::{Global, TargetID};
use pernixc_test_input::Input;
use proptest::{prop_assert, proptest};

use crate::{
    error::Error,
    token_stream::{self, arbitrary::arbitrary_indentation},
};

proptest! {
   #[test]
    fn indentation(
        input in arbitrary_indentation()
    ) {
        let mut source_map = SourceMap::new();
        let source = input.to_string();

        let source_file = SourceFile::new(source, "test".into());

        let id = source_map.register(TargetID::Local, source_file);
        let id = TargetID::Local.make_global(id);

        let storage = Storage::<Error<ByteIndex, Global<ID<SourceFile>>>>::new();
        let token_stream = token_stream::TokenStream::tokenize(
            source_map[id].content(),
            id,
            &storage
        );

        let storage = storage.as_vec();
        prop_assert!(storage.is_empty(), "{storage:?}");

        input.assert(
            token_stream[0].as_fragment().unwrap(),
            (0, &source_map)
        )?;
    }
}
