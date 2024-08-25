use std::sync::Arc;

use pernixc_base::{diagnostic::Storage, source_file::SourceFile};
use pernixc_lexical::token_stream::TokenStream;
use pernixc_tests::input::Input;
use proptest::{
    prelude::{Arbitrary, TestCaseError},
    proptest,
};

use crate::{
    error, parser::Parser, syntax_tree::strategy::QualifiedIdentifier,
};

pub fn parse<T, F>(source: &str, f: F) -> Result<T, TestCaseError>
where
    F: FnOnce(&mut Parser, &Storage<error::Error>) -> Option<T>,
{
    let source_file = Arc::new(SourceFile::temp(source.to_string())?);

    let storage: Storage<pernixc_lexical::error::Error> = Storage::new();

    let token_stream = TokenStream::tokenize(&source_file, &storage);

    if !storage.as_vec().is_empty() {
        return Err(TestCaseError::reject(format!(
            "found lexical error(s): {:#?};\nsource: {source}",
            storage.as_vec(),
        )));
    }

    let mut parser = Parser::new(&token_stream, source_file.clone());

    let storage: Storage<error::Error> = Storage::new();
    let output = f(&mut parser, &storage);

    if !storage.as_vec().is_empty() {
        return Err(TestCaseError::fail(format!(
            "found syntax error(s): {:#?};\nsource: {source}",
            storage.as_vec(),
        )));
    }

    output.map_or_else(
        || {
            Err(TestCaseError::fail(format!(
                "failed to parse the source code: {source}",
            )))
        },
        |output| Ok(output),
    )
}

proptest! {
    #[allow(clippy::ignored_unit_patterns)]
    #[test]
    fn qualified_identifier(
        qualified_identifier_input in QualifiedIdentifier::arbitrary(),
    ) {
        let source = qualified_identifier_input.to_string();
        let qualified_identifier = parse(
            &source,
            |parser, handler| parser.parse_qualified_identifier(handler)
        )?;

        qualified_identifier_input.assert(&qualified_identifier)?;
    }
}
