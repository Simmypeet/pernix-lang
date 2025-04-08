use pernixc_arena::ID;
use pernixc_handler::Storage;
use pernixc_source_file::{LocalSourceMap, SourceFile};
use pernixc_test_input::Input;
use proptest::{prop_assert, proptest};

use crate::{error::Error, tree::arbitrary::arbitrary_indentation};

proptest! {
   #[test]
    fn indentation(
        input in arbitrary_indentation()
    ) {
        let mut local_source_map = LocalSourceMap::new();
        let source = input.to_string();
        println!("{source}");

        let source_file = SourceFile::new(source, "test".into());

        let id = local_source_map.register(source_file).unwrap();

        let storage = Storage::<Error<ID<SourceFile>>>::new();
        let token_stream = super::TokenStream::tokenize(
            local_source_map[id].content(),
            id,
            &storage
        );

        let storage = storage.as_vec();
        prop_assert!(storage.is_empty(), "{storage:?}");

        input.assert(
            token_stream[0].as_fragment().unwrap(),
            (0, &local_source_map)
        )?;
    }
}
