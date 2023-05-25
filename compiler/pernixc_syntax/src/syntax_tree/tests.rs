use std::path::PathBuf;

use pernixc_lexical::token_stream::TokenStream;
use pernixc_source::SourceFile;
use pernixc_system::diagnostic::Storage;
use proptest::{prop_assert_eq, proptest, test_runner::TestCaseError};

use crate::{error::Error, parser::Parser};

pub fn parse<T>(
    source: String,
    f: impl FnOnce(&mut Parser, &Storage<crate::error::Error>) -> super::ParserResult<T>,
) -> Result<T, TestCaseError> {
    let source_file = SourceFile::new(PathBuf::new(), "test".to_string(), source, vec![
        "test".to_string()
    ])?;

    let storage: Storage<pernixc_lexical::error::Error> = Storage::new();

    let token_stream = TokenStream::tokenize(&source_file, &storage);

    if !storage.as_vec().is_empty() {
        return Err(TestCaseError::reject("Tokenization failed"));
    }

    let mut parser = Parser::new(&token_stream);

    let storage: Storage<Error> = Storage::new();
    let output = f(&mut parser, &storage);
    prop_assert_eq!(&*storage.as_vec(), &[]);

    let output = output?;

    Ok(output)
}

proptest! {
    #[test]
    fn qualified_identifier_test(
        qualified_identifier_input in super::strategy::qualified_identifier()
    ) {
        let source = qualified_identifier_input.to_string();
        let qualified_identifier = parse(
            source,
            |parser, handler|
                parser.parse_qualified_identifier(
                    true,
                    handler
                )
        )?;

        qualified_identifier_input.validate(&qualified_identifier)?;
    }

    #[test]
    fn type_specifier_test(
        type_specifier_input in super::strategy::type_specifier()
    ) {
        let source = type_specifier_input.to_string();
        let type_specifier = parse(source, |parser, handler| parser.parse_type_specifier(handler))?;

        type_specifier_input.validate(&type_specifier)?;
    }
}
