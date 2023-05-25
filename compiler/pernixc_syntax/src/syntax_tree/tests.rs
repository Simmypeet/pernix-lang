use std::path::PathBuf;

use pernixc_lexical::{token::KeywordKind, token_stream::TokenStream};
use pernixc_source::SourceFile;
use pernixc_system::diagnostic::Storage;
use proptest::{prop_assert_eq, proptest, test_runner::TestCaseError};

use super::{
    strategy::{
        GenericArgumentInput, GenericArgumentsInput, GenericIdentifierInput,
        LifetimeArgumentIdentifierInput, PrimitiveTypeSpecifierInput, QualifiedIdentifierInput,
        TypeSpecifierInput,
    },
    GenericArgument, GenericArguments, GenericIdentifier, LifetimeArgument, PrimitiveTypeSpecifier,
    QualifiedIdentifier, TypeSpecifier,
};
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

macro_rules! validate_primitive_type {
    ($output:ident, $input:ident, $($ty:ident),*) => {
        match ($output, $input) {
            $(
                (PrimitiveTypeSpecifier::$ty(i), PrimitiveTypeSpecifierInput::$ty) => {
                    prop_assert_eq!(i.keyword, KeywordKind::$ty);
                    Ok(())
                }
            )*

            _ => Err(TestCaseError::fail("Primitive type specifier mismatch")),
        }
    };
}

pub fn validate_primitive_type_specifier(
    output: &PrimitiveTypeSpecifier,
    input: PrimitiveTypeSpecifierInput,
) -> Result<(), TestCaseError> {
    validate_primitive_type!(
        output, input, Bool, Void, Int8, Int16, Int32, Int64, Uint8, Uint16, Uint32, Uint64,
        Float32, Float64
    )
}

pub fn validate_lifetime_argument(
    output: &LifetimeArgument,
    input: &LifetimeArgumentIdentifierInput,
) -> Result<(), TestCaseError> {
    match (&output.lifetime_argument_identifier, input) {
        (
            super::LifetimeArgumentIdentifier::Identifier(o),
            LifetimeArgumentIdentifierInput::Identifier(i),
        ) => {
            prop_assert_eq!(o.span.str(), i);
            Ok(())
        }
        (
            super::LifetimeArgumentIdentifier::StaticKeyword(o),
            LifetimeArgumentIdentifierInput::Static,
        ) => {
            prop_assert_eq!(o.keyword, KeywordKind::Static);
            Ok(())
        }

        _ => Err(TestCaseError::fail("Lifetime argument mismatch")),
    }
}

pub fn validate_generic_argument(
    output: &GenericArgument,
    input: &GenericArgumentInput,
) -> Result<(), TestCaseError> {
    match (output, input) {
        (GenericArgument::TypeSpecifier(o), GenericArgumentInput::TypeSpecifierInput(i)) => {
            validate_type_specifier(o, i)
        }
        (
            GenericArgument::LifetimeArgument(o),
            GenericArgumentInput::LifetimeArgumentIdentifierInput(i),
        ) => validate_lifetime_argument(o, i),

        _ => Err(TestCaseError::fail("Generic argument mismatch")),
    }
}

pub fn validate_generic_arguments(
    output: &GenericArguments,
    input: &GenericArgumentsInput,
) -> Result<(), TestCaseError> {
    prop_assert_eq!(output.colon.is_some(), input.use_turbo_fish);
    prop_assert_eq!(output.argument_list.len(), input.arguments.len());

    for (output, input) in output.argument_list.elements().zip(input.arguments.iter()) {
        validate_generic_argument(output, input)?;
    }

    Ok(())
}

pub fn validate_generic_identifier(
    output: &GenericIdentifier,
    input: &GenericIdentifierInput,
) -> Result<(), TestCaseError> {
    prop_assert_eq!(output.identifier.span.str(), &input.identifier);

    match (&output.generic_arguments, &input.generic_arguments_input) {
        (Some(o), Some(i)) => validate_generic_arguments(o, i)?,
        (None, None) => {}
        _ => {
            return Err(TestCaseError::fail(format!(
                "{o:?}, {i:?} Generic arguments mismatch",
                o = &output.generic_arguments,
                i = &input.generic_arguments_input
            )))
        }
    }

    Ok(())
}

pub fn validate_qualified_identifier(
    output: &QualifiedIdentifier,
    input: &QualifiedIdentifierInput,
) -> Result<(), TestCaseError> {
    prop_assert_eq!(
        output.leading_scope_separator.is_some(),
        input.leading_scope_separator
    );

    // must have the same number of identifiers
    prop_assert_eq!(output.identifiers.len(), input.generic_identifiers.len());

    for (output, input) in output
        .identifiers
        .elements()
        .zip(input.generic_identifiers.iter())
    {
        validate_generic_identifier(output, input)?;
    }

    Ok(())
}

pub fn validate_type_specifier(
    output: &TypeSpecifier,
    input: &TypeSpecifierInput,
) -> Result<(), TestCaseError> {
    match (output, input) {
        (
            TypeSpecifier::PrimitiveTypeSpecifier(o),
            TypeSpecifierInput::PrimitiveTypeSpecifierInput(i),
        ) => validate_primitive_type_specifier(o, *i),
        (
            TypeSpecifier::QualifiedIdentifier(o),
            TypeSpecifierInput::QualifiedIdentifierInput(i),
        ) => validate_qualified_identifier(o, i),

        _ => Err(TestCaseError::fail("Type specifier mismatch")),
    }
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

        validate_qualified_identifier(&qualified_identifier, &qualified_identifier_input)?;
    }

    #[test]
    fn type_specifier_test(
        type_specifier_input in super::strategy::type_specifier()
    ) {
        let source = type_specifier_input.to_string();
        let type_specifier = parse(source, |parser, handler| parser.parse_type_specifier(handler))?;

        validate_type_specifier(&type_specifier, &type_specifier_input)?;
    }
}
