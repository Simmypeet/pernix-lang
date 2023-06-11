//! Contains the definition of various inputs that correspond to the definitions in defined
//! [`crate::syntax_tree`] module.

use std::fmt::{Display, Write};

use pernixc_lexical::token::{self, input::InsignificantToken};
use pernixc_system::input::Input;
use proptest::{
    prelude::Arbitrary,
    prop_oneof,
    strategy::{BoxedStrategy, Just, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

/// Represents an input for the [`super::LifetimeArgumentIdentifier`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LifetimeArgumentIdentifier {
    Static,
    Identifier(token::input::Identifier),
}

impl Arbitrary for LifetimeArgumentIdentifier {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::Static),
            token::input::Identifier::arbitrary().prop_map(Self::Identifier)
        ]
        .boxed()
    }
}

impl Display for LifetimeArgumentIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Static => f.write_str("static"),
            Self::Identifier(ident) => Display::fmt(ident, f),
        }
    }
}

impl Input for LifetimeArgumentIdentifier {
    type Output = super::LifetimeArgumentIdentifier;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Static, super::LifetimeArgumentIdentifier::Static(..)) => Ok(()),
            (Self::Identifier(i), super::LifetimeArgumentIdentifier::Identifier(o)) => i.assert(o),
            (i, o) => Err(TestCaseError::fail(
                format!("Expected {i:?} but got {o:?}",),
            )),
        }
    }
}

/// Represents an input for the [`super::LifetimeArgument`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LifetimeArgument {
    /// The insignificant token before the identifier.
    pub insignificant: Option<InsignificantToken>,

    /// The identifier of the lifetime argument.
    pub lifetime_argument_identifier: LifetimeArgumentIdentifier,
}

impl Arbitrary for LifetimeArgument {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(InsignificantToken::arbitrary()),
            LifetimeArgumentIdentifier::arbitrary(),
        )
            .prop_map(|(insignificant, identifier)| Self {
                insignificant,
                lifetime_argument_identifier: identifier,
            })
            .boxed()
    }
}

impl Input for LifetimeArgument {
    type Output = super::LifetimeArgument;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.lifetime_argument_identifier
            .assert(&output.lifetime_argument_identifier)?;
        Ok(())
    }
}

/// Represents an input for the [`super::ReferenceQualifier`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum ReferenceQualifier {
    Mutable,
    Restrict,
}

impl Arbitrary for ReferenceQualifier {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        prop_oneof![Just(Self::Mutable), Just(Self::Restrict),].boxed()
    }
}

impl Input for ReferenceQualifier {
    type Output = super::ReferenceQualifier;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Mutable, super::ReferenceQualifier::Mutable(..))
            | (Self::Restrict, super::ReferenceQualifier::Restrict(..)) => Ok(()),

            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?} but got {output:?}",
            ))),
        }
    }
}

/// Represents an input for the [`super::PrimitiveTypeSpecifier`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum PrimitiveTypeSpecifier {
    Void,
    Bool,
    Float32,
    Float64,
    Int8,
    Int16,
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
}

impl Arbitrary for PrimitiveTypeSpecifier {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::Void),
            Just(Self::Bool),
            Just(Self::Float32),
            Just(Self::Float64),
            Just(Self::Int8),
            Just(Self::Int16),
            Just(Self::Int32),
            Just(Self::Int64),
            Just(Self::Uint8),
            Just(Self::Uint16),
            Just(Self::Uint32),
            Just(Self::Uint64),
        ]
        .boxed()
    }
}

impl Input for PrimitiveTypeSpecifier {
    type Output = super::PrimitiveTypeSpecifier;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Void, super::PrimitiveTypeSpecifier::Void(..))
            | (Self::Bool, super::PrimitiveTypeSpecifier::Bool(..))
            | (Self::Float32, super::PrimitiveTypeSpecifier::Float32(..))
            | (Self::Float64, super::PrimitiveTypeSpecifier::Float64(..))
            | (Self::Int8, super::PrimitiveTypeSpecifier::Int8(..))
            | (Self::Int16, super::PrimitiveTypeSpecifier::Int16(..))
            | (Self::Int32, super::PrimitiveTypeSpecifier::Int32(..))
            | (Self::Int64, super::PrimitiveTypeSpecifier::Int64(..))
            | (Self::Uint8, super::PrimitiveTypeSpecifier::Uint8(..))
            | (Self::Uint16, super::PrimitiveTypeSpecifier::Uint16(..))
            | (Self::Uint32, super::PrimitiveTypeSpecifier::Uint32(..))
            | (Self::Uint64, super::PrimitiveTypeSpecifier::Uint64(..)) => Ok(()),

            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?} but got {output:?}",
            ))),
        }
    }
}

/// Represents an input for the [`super::TypeSpecifier`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum TypeSpecifier {
    Primitive(PrimitiveTypeSpecifier),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WithInsignificantPrefix<T> {
    pub insignificant: Option<InsignificantToken>,
    pub value: T,
}

/// Is a struct used in [`ConnectedList`] representing a punctuation separator token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstantPunctuation<const CHAR: char>;

impl<const CHAR: char> Display for ConstantPunctuation<CHAR> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { f.write_char(CHAR) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConnectedList<T, U> {
    pub first: T,
    pub rest: Vec<(WithInsignificantPrefix<U>, WithInsignificantPrefix<T>)>,
    pub trailing_separator: Option<WithInsignificantPrefix<U>>,
}
