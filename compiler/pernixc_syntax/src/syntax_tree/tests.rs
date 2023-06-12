use pernixc_lexical::token_stream::TokenStream;
use pernixc_source::SourceFile;
use pernixc_system::{diagnostic::Storage, input::Input};
use proptest::{prelude::Arbitrary, proptest, test_runner::TestCaseError};

use crate::{error::Error, parser::Parser};

/// Parses the given source code and returns the result of the given function.
///
/// # Parameters
/// - `source`: The source code to parse.
/// - `f`: The function to call with the parser and the error handler.
///
/// # Errors
/// - Returns [`TestCaseError::Reject`] if found any lexical errors.
/// - Returns [`TestCaseError::Fail`] if found any syntax errors.
pub fn parse<T, F>(source: &str, f: F) -> Result<T, TestCaseError>
where
    for<'a> F: FnOnce(&mut Parser<'a>, &Storage<crate::error::Error>) -> super::ParserResult<T>,
{
    let source_file = SourceFile::temp(source)?;

    let storage: Storage<pernixc_lexical::error::Error> = Storage::new();

    let token_stream = TokenStream::tokenize(&source_file, &storage);

    if !storage.as_vec().is_empty() {
        return Err(TestCaseError::reject(format!(
            "found lexical error(s): {:#?};\nsource: {source}",
            storage.as_vec(),
        )));
    }

    let mut parser = Parser::new(&token_stream);

    let storage: Storage<Error> = Storage::new();
    let output = f(&mut parser, &storage);

    if !storage.as_vec().is_empty() {
        return Err(TestCaseError::fail(format!(
            "found syntax error(s): {:#?};\nsource: {source}",
            storage.as_vec(),
        )));
    }

    let output = output?;

    Ok(output)
}

proptest! {
    #[test]
    fn qualified_identifier_test(
        qualified_identifier_input in super::input::QualifiedIdentifier::arbitrary(),
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

        qualified_identifier_input.assert(&qualified_identifier)?;
    }

    #[test]
    #[allow(clippy::redundant_closure_for_method_calls)]
    fn type_specifier_test(
        type_specifier_input in super::input::TypeSpecifier::arbitrary(),
    ) {
        let source = type_specifier_input.to_string();
        let type_specifier = parse(&source, |parser, handler| parser.parse_type_specifier(handler))?;

        type_specifier_input.assert(&type_specifier)?;
    }
}
