use pernixc_lexical::token_stream::TokenStream;
use pernixc_source::SourceFile;
use pernixc_system::diagnostic::Storage;
use proptest::{prop_assert, proptest, test_runner::TestCaseError};

use crate::{error::Error, parser::Parser};

pub fn parse<T, F>(source: &str, f: F) -> Result<T, TestCaseError>
where
    for<'a> F: FnOnce(&mut Parser<'a>, &Storage<crate::error::Error>) -> super::ParserResult<T>,
{
    let source_file = SourceFile::temp(source)?;

    let storage: Storage<pernixc_lexical::error::Error> = Storage::new();

    let token_stream = TokenStream::tokenize(&source_file, &storage);

    if !storage.as_vec().is_empty() {
        return Err(TestCaseError::reject(format!(
            "{:#?} {} Tokenization failed",
            storage.as_vec(),
            source_file.source_code()
        )));
    }

    let mut parser = Parser::new(&token_stream);

    let storage: Storage<Error> = Storage::new();
    let output = f(&mut parser, &storage);
    prop_assert!(storage.as_vec().is_empty());

    let output = output?;

    Ok(output)
}

proptest! {
    #[test]
    fn qualified_identifier_test(
        qualified_identifier_input in super::strategy::qualified_identifier(true)
    ) {
        let source = qualified_identifier_input.to_string();
        let qualified_identifier = parse(
            &source,
            |parser, handler|
                parser.parse_qualified_identifier(
                    true,
                    handler
                )
        )?;

        qualified_identifier_input.validate(&qualified_identifier)?;
    }

    #[test]
    #[allow(clippy::redundant_closure_for_method_calls)]
    fn type_specifier_test(
        type_specifier_input in super::strategy::type_specifier()
    ) {
        let source = type_specifier_input.to_string();
        let type_specifier = parse(&source, |parser, handler| parser.parse_type_specifier(handler))?;

        type_specifier_input.validate(&type_specifier)?;
    }
}
