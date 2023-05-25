//! Contains the definition of various `proptest` strategies that produces syntax tree input for
//! testing purposes.

use proptest::{
    prop_oneof,
    strategy::{Just, Strategy},
};

/// Represents an input for [`super::LifetimeArgumentIdentifier`]
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum LifetimeArgumentIdentifierInput {
    Static,
    Identifier(String),
}

/// Represents an input for [`super::GenericArgument`]
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum GenericArgumentInput {
    LifetimeArgumentIdentifierInput(LifetimeArgumentIdentifierInput),
    TypeSpecifierInput(TypeSpecifierInput),
}

/// Represents an input for [`super::GenericArguments`]
#[derive(Debug, Clone)]
pub struct GenericArgumentsInput {
    pub use_turbo_fish: bool,
    pub arguments: Vec<GenericArgumentInput>,
}

/// Represents an input for [`super::GenericIdentifier`]
#[derive(Debug, Clone)]
pub struct GenericIdentifierInput {
    /// The identifier of the generic identifier
    pub identifier: String,

    /// The generic arguments of the generic identifier
    pub generic_arguments_input: Option<GenericArgumentsInput>,
}

/// Represents an input for [`super::QualifiedIdentifier`]
#[derive(Debug, Clone)]
pub struct QualifiedIdentifierInput {
    /// Whether the qualified identifier starts with a scope separator
    pub leading_scope_separator: bool,

    /// The generic identifiers of the qualified identifier
    pub generic_identifiers: Vec<GenericIdentifierInput>,
}

/// Represents an input for [`super::PrimitiveTypeSpecifier`]
#[derive(Debug, Clone, Copy)]
#[allow(missing_docs)]
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

/// Returns an input for [`super::PrimitiveTypeSpecifier`]
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum TypeSpecifierInput {
    PrimitiveTypeSpecifierInput(PrimitiveTypeSpecifierInput),
    QualifiedIdentifierInput(QualifiedIdentifierInput),
}

fn remove_turbo_fish(qualified_identifier: &mut QualifiedIdentifierInput) {
    for generic_identifier in &mut qualified_identifier.generic_identifiers {
        if let Some(generic_arguments) = &mut generic_identifier.generic_arguments_input {
            generic_arguments.use_turbo_fish = false;

            for generic_argument in &mut generic_arguments.arguments {
                if let GenericArgumentInput::TypeSpecifierInput(
                    TypeSpecifierInput::QualifiedIdentifierInput(q),
                ) = generic_argument
                {
                    remove_turbo_fish(q);
                }
            }
        }
    }
}

/// Returs a strategy that produces [`QualifiedIdentifierInput`].
///
/// The produced input will not contain any turbo fish.
pub fn qualified_identifier() -> impl Strategy<Value = QualifiedIdentifierInput> {
    (
        proptest::bool::ANY,
        pernixc_lexical::token::strategy::identifier(),
    )
        .prop_map(|(leading_separator, identifier)| QualifiedIdentifierInput {
            leading_scope_separator: leading_separator,
            generic_identifiers: vec![GenericIdentifierInput {
                identifier,
                generic_arguments_input: None,
            }],
        })
        .prop_recursive(8, 24, 10, |inner| {
            (
                proptest::bool::ANY,
                proptest::collection::vec(
                    (
                        pernixc_lexical::token::strategy::identifier(),
                        proptest::option::of(proptest::collection::vec(
                            prop_oneof![
                                primitive_type_specifier().prop_map(|x| {
                                    GenericArgumentInput::TypeSpecifierInput(
                                        TypeSpecifierInput::PrimitiveTypeSpecifierInput(x),
                                    )
                                }),
                                inner.prop_map(|mut x| {
                                    remove_turbo_fish(&mut x);
                                    GenericArgumentInput::TypeSpecifierInput(
                                        TypeSpecifierInput::QualifiedIdentifierInput(x),
                                    )
                                }),
                                lifetime_argument_identifier().prop_map(|x| {
                                    GenericArgumentInput::LifetimeArgumentIdentifierInput(x)
                                })
                            ],
                            1..=5,
                        )),
                    )
                        .prop_map(|(identifier, generic_arguments)| {
                            GenericIdentifierInput {
                                identifier,
                                generic_arguments_input: generic_arguments.map(|x| {
                                    GenericArgumentsInput {
                                        use_turbo_fish: true,
                                        arguments: x,
                                    }
                                }),
                            }
                        }),
                    1..=5,
                ),
            )
                .prop_map(|(leading_scope_separator, generic_identifiers)| {
                    QualifiedIdentifierInput {
                        leading_scope_separator,
                        generic_identifiers,
                    }
                })
        })
}

fn lifetime_argument_identifier() -> impl Strategy<Value = LifetimeArgumentIdentifierInput> {
    prop_oneof![
        pernixc_lexical::token::strategy::identifier()
            .prop_map(LifetimeArgumentIdentifierInput::Identifier),
        Just(LifetimeArgumentIdentifierInput::Static)
    ]
}

/// Returns a strategy that produces [`PrimitiveTypeSpecifierInput`]
pub fn type_specifier() -> impl Strategy<Value = TypeSpecifierInput> {
    prop_oneof![primitive_type_specifier()
        .prop_map(|x| { TypeSpecifierInput::PrimitiveTypeSpecifierInput(x) }),]
    .prop_recursive(8, 24, 10, |inner| {
        (
            proptest::bool::ANY,
            proptest::collection::vec(
                (
                    pernixc_lexical::token::strategy::identifier(),
                    proptest::option::of(proptest::collection::vec(
                        prop_oneof![
                            inner.prop_map(GenericArgumentInput::TypeSpecifierInput),
                            lifetime_argument_identifier()
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
                    leading_scope_separator: leading_separator,
                    generic_identifiers: identifiers,
                })
            })
    })
}

fn primitive_type_specifier() -> impl Strategy<Value = PrimitiveTypeSpecifierInput> {
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
            .generic_identifiers
            .iter()
            .map(GenericIdentifierInput::to_string)
            .collect::<Vec<_>>()
            .join("::");

        if self.leading_scope_separator {
            string.insert_str(0, "::");
        }

        string
    }
}
