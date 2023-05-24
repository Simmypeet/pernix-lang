use std::path::PathBuf;

use pernixc_lexical::{token::KeywordKind, token_stream::TokenStream};
use pernixc_source::SourceFile;
use pernixc_system::diagnostic::Storage;
use proptest::{
    prop_assert_eq, prop_oneof, proptest,
    strategy::{Just, Strategy},
    test_runner::TestCaseError,
};

use super::{
    GenericArgument, GenericArguments, GenericIdentifier, LifetimeArgument, PrimitiveTypeSpecifier,
    QualifiedIdentifier, TypeSpecifier,
};
use crate::{error::Error, parser::Parser};

#[derive(Debug, Clone)]
pub enum LifetimeArgumentIdentifierInput {
    Static,
    Identifier(String),
}

#[derive(Debug, Clone)]
pub enum GenericArgumentInput {
    LifetimeArgumentIdentifierInput(LifetimeArgumentIdentifierInput),
    TypeSpecifierInput(TypeSpecifierInput),
}

#[derive(Debug, Clone)]
pub struct GenericArgumentsInput {
    pub use_turbo_fish: bool,
    pub arguments: Vec<GenericArgumentInput>,
}

#[derive(Debug, Clone)]
pub struct GenericIdentifierInput {
    pub identifier: String,
    pub generic_arguments_input: Option<GenericArgumentsInput>,
}

#[derive(Debug, Clone)]
pub struct QualifiedIdentifierInput {
    pub leading_separator: bool,
    pub identifiers: Vec<GenericIdentifierInput>,
}

#[derive(Debug, Clone, Copy)]
pub enum PrimitiveTypeSpecifierInput {
    Int8,
    Int16,
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Float32,
    Float64,
    Bool,
    Void,
}

#[derive(Debug, Clone)]
pub enum TypeSpecifierInput {
    PrimitiveTypeSpecifierInput(PrimitiveTypeSpecifierInput),
    QualifiedIdentifierInput(QualifiedIdentifierInput),
}

const IDENTIFIER_STR_REGEX: &str = "[a-zA-Z_][a-zA-Z0-9_]*";

pub fn qualified_identifier_strategy() -> impl Strategy<Value = QualifiedIdentifierInput> {
    proptest::bool::ANY.prop_flat_map(|leading_separator| {
        proptest::collection::vec(
            (
                IDENTIFIER_STR_REGEX,
                proptest::option::of(proptest::collection::vec(
                    prop_oneof![
                        lifetime_argument_identifier_strategy()
                            .prop_map(GenericArgumentInput::LifetimeArgumentIdentifierInput),
                        type_specifier_strategy()
                            .prop_map(GenericArgumentInput::TypeSpecifierInput)
                    ],
                    1..=3,
                )),
            )
                .prop_map(move |(identifier, generic_arguments_input)| {
                    GenericIdentifierInput {
                        identifier,
                        generic_arguments_input: generic_arguments_input.map(|arguments| {
                            GenericArgumentsInput {
                                use_turbo_fish: true,
                                arguments,
                            }
                        }),
                    }
                }),
            1..=10,
        )
        .prop_map(move |identifiers| QualifiedIdentifierInput {
            leading_separator,
            identifiers,
        })
    })
}

pub fn lifetime_argument_identifier_strategy(
) -> impl Strategy<Value = LifetimeArgumentIdentifierInput> {
    prop_oneof![
        IDENTIFIER_STR_REGEX.prop_map(LifetimeArgumentIdentifierInput::Identifier),
        Just(LifetimeArgumentIdentifierInput::Static)
    ]
}

pub fn type_specifier_strategy() -> impl Strategy<Value = TypeSpecifierInput> {
    prop_oneof![primitive_type_specifier_strategy()
        .prop_map(|x| { TypeSpecifierInput::PrimitiveTypeSpecifierInput(x) }),]
    .prop_recursive(8, 24, 10, |inner| {
        (
            proptest::bool::ANY,
            proptest::collection::vec(
                (
                    IDENTIFIER_STR_REGEX,
                    proptest::option::of(proptest::collection::vec(
                        prop_oneof![
                            inner.prop_map(GenericArgumentInput::TypeSpecifierInput),
                            lifetime_argument_identifier_strategy()
                                .prop_map(GenericArgumentInput::LifetimeArgumentIdentifierInput)
                        ],
                        1..=10,
                    )),
                )
                    .prop_map(|(name, generic_arguments)| GenericIdentifierInput {
                        identifier: name,
                        generic_arguments_input: generic_arguments.map(|x| GenericArgumentsInput {
                            use_turbo_fish: false,
                            arguments: x,
                        }),
                    }),
                1..=10,
            ),
        )
            .prop_map(|(leading_separator, identifiers)| {
                TypeSpecifierInput::QualifiedIdentifierInput(QualifiedIdentifierInput {
                    leading_separator,
                    identifiers,
                })
            })
    })
}

pub fn primitive_type_specifier_strategy() -> impl Strategy<Value = PrimitiveTypeSpecifierInput> {
    prop_oneof![
        Just(PrimitiveTypeSpecifierInput::Int8),
        Just(PrimitiveTypeSpecifierInput::Int16),
        Just(PrimitiveTypeSpecifierInput::Int32),
        Just(PrimitiveTypeSpecifierInput::Int64),
        Just(PrimitiveTypeSpecifierInput::Uint8),
        Just(PrimitiveTypeSpecifierInput::Uint16),
        Just(PrimitiveTypeSpecifierInput::Uint32),
        Just(PrimitiveTypeSpecifierInput::Uint64),
        Just(PrimitiveTypeSpecifierInput::Float32),
        Just(PrimitiveTypeSpecifierInput::Float64),
        Just(PrimitiveTypeSpecifierInput::Bool),
        Just(PrimitiveTypeSpecifierInput::Void),
    ]
}

impl ToString for LifetimeArgumentIdentifierInput {
    fn to_string(&self) -> String {
        match &self {
            Self::Static => "'static".to_string(),
            Self::Identifier(identifier) => {
                let mut identifier = identifier.clone();
                identifier.insert(0, '\'');
                identifier
            }
        }
    }
}

impl ToString for GenericArgumentInput {
    fn to_string(&self) -> String {
        match self {
            Self::LifetimeArgumentIdentifierInput(i) => i.to_string(),
            Self::TypeSpecifierInput(i) => i.to_string(),
        }
    }
}

impl ToString for PrimitiveTypeSpecifierInput {
    fn to_string(&self) -> String {
        match self {
            Self::Int8 => "int8",
            Self::Int16 => "int16",
            Self::Int32 => "int32",
            Self::Int64 => "int64",
            Self::Uint8 => "uint8",
            Self::Uint16 => "uint16",
            Self::Uint32 => "uint32",
            Self::Uint64 => "uint64",
            Self::Float32 => "float32",
            Self::Float64 => "float64",
            Self::Bool => "bool",
            Self::Void => "void",
        }
        .to_string()
    }
}

impl ToString for TypeSpecifierInput {
    fn to_string(&self) -> String {
        match self {
            Self::PrimitiveTypeSpecifierInput(i) => i.to_string(),
            Self::QualifiedIdentifierInput(i) => i.to_string(),
        }
    }
}

impl ToString for GenericArgumentsInput {
    fn to_string(&self) -> String {
        let mut string = if self.use_turbo_fish { ":<" } else { "<" }.to_string();

        let arguments_string = self
            .arguments
            .iter()
            .map(GenericArgumentInput::to_string)
            .collect::<Vec<_>>()
            .join(", ");

        string.push_str(&arguments_string);
        string.push('>');

        string
    }
}
impl ToString for GenericIdentifierInput {
    fn to_string(&self) -> String {
        let mut string = self.identifier.clone();

        if let Some(generic_arguments_input) = &self.generic_arguments_input {
            string.push_str(&generic_arguments_input.to_string());
        }

        string
    }
}

impl ToString for QualifiedIdentifierInput {
    fn to_string(&self) -> String {
        let mut string = self
            .identifiers
            .iter()
            .map(GenericIdentifierInput::to_string)
            .collect::<Vec<_>>()
            .join("::");

        if self.leading_separator {
            string.insert_str(0, "::");
        }

        string
    }
}

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
        input.leading_separator
    );

    // must have the same number of identifiers
    prop_assert_eq!(output.identifiers.len(), input.identifiers.len());

    for (output, input) in output.identifiers.elements().zip(input.identifiers.iter()) {
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
        qualified_identifier_input in qualified_identifier_strategy()
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

        validate_qualified_identifier(&qualified_identifier, &qualified_identifier_input).unwrap();
    }

    #[test]
    fn type_specifier_test(
        type_specifier_input in type_specifier_strategy()
    ) {
        let source = type_specifier_input.to_string();
        let type_specifier = parse(source, |parser, handler| parser.parse_type_specifier(handler))?;

        validate_type_specifier(&type_specifier, &type_specifier_input)?;
    }
}
