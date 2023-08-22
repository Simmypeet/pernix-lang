use std::{
    fmt::{Debug, Display, Write},
    str::FromStr,
};

use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_lexical::{token::KeywordKind, token_stream::TokenStream};
use pernixc_source::SourceFile;
use pernixc_system::diagnostic::Storage;
use pernixc_tests::input::Input;
use proptest::{
    prelude::Arbitrary,
    prop_assert_eq, prop_oneof, proptest,
    strategy::{BoxedStrategy, Just, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use super::expression::tests::Expression;
use crate::{
    error::{self},
    parser::Parser,
};

/// Represents an input for the [`pernixc_lexical::token::Identifier`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier {
    /// The string representation of the identifier.
    pub string: String,
}

impl Arbitrary for Identifier {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        "[a-zA-Z_][a-zA-Z0-9_]*"
            .prop_map(|string| Self { string })
            .prop_filter_map("filter out keywords", |x| {
                if KeywordKind::from_str(&x.string).is_ok() {
                    None
                } else {
                    Some(x)
                }
            })
            .boxed()
    }
}

impl Input for Identifier {
    type Output = pernixc_lexical::token::Identifier;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(&self.string, output.span.str());
        Ok(())
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { f.write_str(&self.string) }
}

/// Represents an input for the [`super::LifetimeArgumentIdentifier`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum LifetimeArgumentIdentifier {
    Static,
    Identifier(Identifier),
}

impl Arbitrary for LifetimeArgumentIdentifier {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::Static),
            Identifier::arbitrary().prop_map(Self::Identifier)
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
    /// The identifier of the lifetime argument.
    pub lifetime_argument_identifier: LifetimeArgumentIdentifier,
}

impl Arbitrary for LifetimeArgument {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        LifetimeArgumentIdentifier::arbitrary()
            .prop_map(|identifier| Self {
                lifetime_argument_identifier: identifier,
            })
            .boxed()
    }
}

impl Input for LifetimeArgument {
    type Output = super::LifetimeArgument;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.lifetime_argument_identifier
            .assert(output.identifier())?;
        Ok(())
    }
}

impl Display for LifetimeArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('\'')?;
        Display::fmt(&self.lifetime_argument_identifier, f)
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

impl Display for ReferenceQualifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Mutable => f.write_str("mutable"),
            Self::Restrict => f.write_str("restrict"),
        }
    }
}

/// Represents an input for the [`super::ReferenceTypeSpecifier`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReferenceTypeSpecifier {
    /// The lifetime argument of the reference type.
    pub lifetime_argument: Option<LifetimeArgument>,

    /// The qualifier of the reference type.
    pub qualifier: Option<ReferenceQualifier>,

    /// The type specifier of the reference type.
    pub operand_type: Box<TypeSpecifier>,
}

impl Arbitrary for ReferenceTypeSpecifier {
    type Parameters = Option<BoxedStrategy<TypeSpecifier>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(LifetimeArgument::arbitrary()),
            proptest::option::of(ReferenceQualifier::arbitrary()),
            args.unwrap_or_else(TypeSpecifier::arbitrary),
        )
            .prop_map(|(lifetime_argument, qualifier, operand_type)| Self {
                lifetime_argument,
                qualifier,
                operand_type: Box::new(operand_type),
            })
            .boxed()
    }
}

impl Input for ReferenceTypeSpecifier {
    type Output = super::ReferenceTypeSpecifier;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.lifetime_argument.assert(output.lifetime_argument())?;
        self.qualifier.assert(output.qualifier())?;
        self.operand_type.assert(output.operand_type())
    }
}

impl Display for ReferenceTypeSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('&')?;

        if let Some(lifetime_argument) = &self.lifetime_argument {
            Display::fmt(lifetime_argument, f)?;
            f.write_char(' ')?;
        }

        if let Some(qualifier) = &self.qualifier {
            Display::fmt(qualifier, f)?;
            f.write_char(' ')?;
        }

        Display::fmt(&self.operand_type, f)
    }
}

/// Represents an input for the [`super::PrimitiveTypeSpecifier`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum PrimitiveTypeSpecifier {
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
            (Self::Bool, super::PrimitiveTypeSpecifier::Bool(..))
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

impl Display for PrimitiveTypeSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool => write!(f, "bool"),
            Self::Float32 => write!(f, "float32"),
            Self::Float64 => write!(f, "float64"),
            Self::Int8 => write!(f, "int8"),
            Self::Int16 => write!(f, "int16"),
            Self::Int32 => write!(f, "int32"),
            Self::Int64 => write!(f, "int64"),
            Self::Uint8 => write!(f, "uint8"),
            Self::Uint16 => write!(f, "uint16"),
            Self::Uint32 => write!(f, "uint32"),
            Self::Uint64 => write!(f, "uint64"),
        }
    }
}

/// Represents an input for the [`super::TypeAnnotation`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeAnnotation {
    /// The type specifier
    pub type_specifier: TypeSpecifier,
}

impl Input for TypeAnnotation {
    type Output = super::TypeAnnotation;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.type_specifier.assert(output.type_specifier())
    }
}

impl Display for TypeAnnotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ": {}", self.type_specifier)
    }
}

impl Arbitrary for TypeAnnotation {
    type Parameters = <TypeSpecifier as Arbitrary>::Parameters;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        TypeSpecifier::arbitrary_with(args)
            .prop_map(|type_specifier| Self { type_specifier })
            .boxed()
    }
}

/// Represents an input for the [`super::ArrayTypeSpecifier`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ArrayTypeSpecifier {
    /// The type specifier of the array.
    pub operand: Box<TypeSpecifier>,

    /// The expression of the array.
    pub expression: Box<Expression>,
}

impl Arbitrary for ArrayTypeSpecifier {
    type Parameters = (
        Option<BoxedStrategy<TypeSpecifier>>,
        Option<BoxedStrategy<Expression>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            args.0
                .clone()
                .unwrap_or_else(|| TypeSpecifier::arbitrary_with((None, args.1.clone()))),
            args.1.unwrap_or_else(|| Expression::arbitrary_with(args.0)),
        )
            .prop_map(|(type_specifier, expression)| Self {
                operand: Box::new(type_specifier),
                expression: Box::new(expression),
            })
            .boxed()
    }
}

impl Input for ArrayTypeSpecifier {
    type Output = super::ArrayTypeSpecifier;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.operand.assert(output.operand())?;
        self.expression.assert(output.expression())
    }
}

impl Display for ArrayTypeSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('[')?;
        Display::fmt(&self.operand, f)?;
        f.write_str(": ")?;
        Display::fmt(&self.expression, f)?;
        f.write_char(']')?;

        Ok(())
    }
}

/// Represents an input for the [`super::PointerTypeSpecifier`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PointerTypeSpecifier {
    /// The type specifier of the pointer.
    pub operand: Box<TypeSpecifier>,

    /// Whether the pointer is mutable or not.
    pub mutable: bool,
}

impl Input for PointerTypeSpecifier {
    type Output = super::PointerTypeSpecifier;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.operand.assert(output.operand())?;
        prop_assert_eq!(self.mutable, output.mutable_keyword().is_some());
        Ok(())
    }
}

impl Arbitrary for PointerTypeSpecifier {
    type Parameters = Option<BoxedStrategy<TypeSpecifier>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            args.unwrap_or_else(TypeSpecifier::arbitrary),
            proptest::bool::ANY,
        )
            .prop_map(|(operand, mutable)| Self {
                operand: Box::new(operand),
                mutable,
            })
            .boxed()
    }
}

impl Display for PointerTypeSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('*')?;

        if self.mutable {
            f.write_str("mutable ")?;
        }

        Display::fmt(&self.operand, f)
    }
}

/// Represents an input for the [`super::VariadicTypeParameter`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariadicTypeParameter {
    /// The identifier of the variadic type.
    pub identifier: Identifier,
}

impl Input for VariadicTypeParameter {
    type Output = super::VariadicTypeParameter;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.identifier.assert(output.identifier())
    }
}

impl Arbitrary for VariadicTypeParameter {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        Identifier::arbitrary()
            .prop_map(|identifier| Self { identifier })
            .boxed()
    }
}

impl Display for VariadicTypeParameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "...{}", self.identifier)
    }
}

/// Represents an input for the [`super::VariadicableTypeSpecifier`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum VariadicableTypeSpecifier {
    TypeSpecifier(TypeSpecifier),
    Variadic(VariadicTypeParameter),
}

impl Input for VariadicableTypeSpecifier {
    type Output = super::VariadicableTypeSpecifier;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::TypeSpecifier(i), super::VariadicableTypeSpecifier::TypeSpecifier(o)) => {
                i.assert(o)
            }
            (Self::Variadic(i), super::VariadicableTypeSpecifier::Variadic(o)) => i.assert(o),
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?} but got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for VariadicableTypeSpecifier {
    type Parameters = Option<BoxedStrategy<TypeSpecifier>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            args.unwrap_or_else(TypeSpecifier::arbitrary)
                .prop_map(Self::TypeSpecifier),
            VariadicTypeParameter::arbitrary().prop_map(Self::Variadic),
        ]
        .boxed()
    }
}

impl Display for VariadicableTypeSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TypeSpecifier(i) => Display::fmt(i, f),
            Self::Variadic(i) => Display::fmt(i, f),
        }
    }
}

/// Represents an input for the [`super::TupleTypeSpecifier`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TupleTypeSpecifier {
    /// The type specifier list of the tuple.
    pub type_specifier_list:
        Option<ConnectedList<Box<VariadicableTypeSpecifier>, ConstantPunctuation<','>>>,
}

impl Input for TupleTypeSpecifier {
    type Output = super::TupleTypeSpecifier;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.type_specifier_list
            .assert(output.type_specifier_list())
    }
}

impl Arbitrary for TupleTypeSpecifier {
    type Parameters = Option<BoxedStrategy<TypeSpecifier>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        proptest::option::of(ConnectedList::arbitrary_with(
            VariadicableTypeSpecifier::arbitrary_with(args).prop_map(Box::new),
            ConstantPunctuation::<','>::arbitrary(),
        ))
        .prop_map(|type_specifier_list| Self {
            type_specifier_list,
        })
        .boxed()
    }
}

impl Display for TupleTypeSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('(')?;
        if let Some(type_specifier_list) = &self.type_specifier_list {
            Display::fmt(type_specifier_list, f)?;
        }
        f.write_char(')')?;

        Ok(())
    }
}

/// Represents an input for the [`super::TypeSpecifier`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum TypeSpecifier {
    Primitive(PrimitiveTypeSpecifier),
    Reference(ReferenceTypeSpecifier),
    QualifiedIdentifier(QualifiedIdentifier),
    Array(ArrayTypeSpecifier),
    Pointer(PointerTypeSpecifier),
    Tuple(TupleTypeSpecifier),
}

impl Input for TypeSpecifier {
    type Output = super::TypeSpecifier;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Primitive(i), super::TypeSpecifier::Primitive(o)) => i.assert(o),
            (Self::Reference(i), super::TypeSpecifier::Reference(o)) => i.assert(o),
            (Self::QualifiedIdentifier(i), super::TypeSpecifier::QualifiedIdentifier(o)) => {
                i.assert(o)
            }
            (Self::Array(i), super::TypeSpecifier::Array(o)) => i.assert(o),
            (Self::Pointer(i), super::TypeSpecifier::Pointer(o)) => i.assert(o),
            (Self::Tuple(i), super::TypeSpecifier::Tuple(o)) => i.assert(o),
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?} but got {output:?}",
            ))),
        }
    }
}

fn remove_turbo_fish(qualified_identifier: &mut QualifiedIdentifier) {
    for generic_identifier in
        std::iter::once(&mut qualified_identifier.first).chain(qualified_identifier.rest.iter_mut())
    {
        if let Some(generic_arguments) = &mut generic_identifier.generic_arguments {
            generic_arguments.turbofish = false;

            for generic_argument in std::iter::once(&mut generic_arguments.argument_list.first)
                .chain(
                    generic_arguments
                        .argument_list
                        .rest
                        .iter_mut()
                        .map(|(_, a)| a),
                )
            {
                let GenericArgument::TypeSpecifier(q) = generic_argument else {
                    continue;
                };

                let TypeSpecifier::QualifiedIdentifier(ref mut q) = q.as_mut() else {
                    continue;
                };

                remove_turbo_fish(q);
            }
        }
    }
}

impl Arbitrary for TypeSpecifier {
    type Parameters = (
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Expression>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let leaf =
            prop_oneof![PrimitiveTypeSpecifier::arbitrary().prop_map(TypeSpecifier::Primitive),];

        leaf.prop_recursive(8, 64, 8, move |inner| {
            prop_oneof![
                ReferenceTypeSpecifier::arbitrary_with(Some(inner.clone()))
                    .prop_map(TypeSpecifier::Reference),
                args.0
                    .clone()
                    .unwrap_or_else(|| QualifiedIdentifier::arbitrary_with((false, args.1.clone())))
                    .prop_map(|mut x| {
                        remove_turbo_fish(&mut x);
                        Self::QualifiedIdentifier(x)
                    }),
                ArrayTypeSpecifier::arbitrary_with((Some(inner.clone()), args.1.clone()))
                    .prop_map(TypeSpecifier::Array),
                PointerTypeSpecifier::arbitrary_with(Some(inner.clone()))
                    .prop_map(TypeSpecifier::Pointer),
                TupleTypeSpecifier::arbitrary_with(Some(inner)).prop_map(TypeSpecifier::Tuple),
            ]
        })
        .boxed()
    }
}

impl Display for TypeSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Primitive(i) => Display::fmt(i, f),
            Self::Reference(i) => Display::fmt(i, f),
            Self::QualifiedIdentifier(i) => Display::fmt(i, f),
            Self::Array(i) => Display::fmt(i, f),
            Self::Pointer(i) => Display::fmt(i, f),
            Self::Tuple(i) => Display::fmt(i, f),
        }
    }
}

/// Is a struct used in [`ConnectedList`] representing a punctuation separator token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct ConstantPunctuation<const CHAR: char>;

impl<const CHAR: char> Arbitrary for ConstantPunctuation<CHAR> {
    type Parameters = ();
    type Strategy = Just<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy { Just(Self) }
}

impl<const CHAR: char> Input for ConstantPunctuation<CHAR> {
    type Output = pernixc_lexical::token::Punctuation;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(CHAR, output.punctuation);
        Ok(())
    }
}

impl<const CHAR: char> Display for ConstantPunctuation<CHAR> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { f.write_char(CHAR) }
}

/// Represents an input for the [`super::QualifiedIdentifier`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConnectedList<T, U> {
    /// The first element in the connected list.
    pub first: T,

    /// The continuation of the connected list (comma, element) pairs.
    pub rest: Vec<(U, T)>,

    /// If the connected list ends with a trailing separator, this is the separator.
    pub trailing_separator: Option<U>,
}

impl<T: Input, U: Input + Debug> Input for ConnectedList<T, U>
where
    U::Output: Debug,
{
    type Output = super::ConnectedList<T::Output, U::Output>;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.first.assert(output.first())?;

        prop_assert_eq!(self.rest.len(), output.rest().len());

        for (input, output) in self.rest.iter().zip(output.rest().iter()) {
            input.0.assert(&output.0)?;
            input.1.assert(&output.1)?;
        }

        self.trailing_separator
            .assert(output.trailing_separator())?;

        Ok(())
    }
}

impl<T: Display, U: Display> Display for ConnectedList<T, U> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.first, f)?;

        for (separator, element) in &self.rest {
            Display::fmt(separator, f)?;
            f.write_char(' ')?;
            Display::fmt(element, f)?;
        }

        if let Some(separator) = self.trailing_separator.as_ref() {
            Display::fmt(separator, f)?;
        }

        Ok(())
    }
}

impl<T: Debug, U: Debug> ConnectedList<T, U> {
    /// Creates a new [`ConnectedList`] strategy with the given element strategy and punctuation
    /// strategy.
    pub fn arbitrary_with(
        element_strategy: impl Strategy<Value = T> + Clone,
        punctuation: impl Strategy<Value = U> + Clone,
    ) -> impl Strategy<Value = Self> {
        (
            element_strategy.clone(),
            proptest::collection::vec((punctuation.clone(), element_strategy), 0..=7),
            proptest::option::of(punctuation),
        )
            .prop_map(|(first, rest, trailing_separator)| Self {
                first,
                rest,
                trailing_separator,
            })
    }
}

/// Represents an input for the [`super::ConstArgument`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstArgument {
    /// The expression of the argument.
    pub expression: Box<Expression>,
}

impl Arbitrary for ConstArgument {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        args.unwrap_or_else(|| Expression::arbitrary().boxed())
            .prop_map(|expression| Self {
                expression: Box::new(expression),
            })
            .boxed()
    }
}

impl Input for ConstArgument {
    type Output = super::ConstArgument;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.expression.assert(output.expression())?;
        Ok(())
    }
}

impl Display for ConstArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{{}}}", self.expression)
    }
}

/// Represents an input for the [`super::QualifiedIdentifier`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum GenericArgument {
    Lifetime(LifetimeArgument),
    TypeSpecifier(Box<TypeSpecifier>),
    Const(ConstArgument),
}

impl Arbitrary for GenericArgument {
    type Parameters = <TypeSpecifier as Arbitrary>::Parameters;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            LifetimeArgument::arbitrary().prop_map(Self::Lifetime),
            TypeSpecifier::arbitrary_with(args.clone())
                .prop_map(|x| Self::TypeSpecifier(Box::new(x))),
            ConstArgument::arbitrary_with(args.1).prop_map(Self::Const),
        ]
        .boxed()
    }
}

impl Input for GenericArgument {
    type Output = super::GenericArgument;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Lifetime(i), super::GenericArgument::Lifetime(o)) => i.assert(o),
            (Self::TypeSpecifier(i), super::GenericArgument::TypeSpecifier(o)) => i.assert(o),
            (Self::Const(i), super::GenericArgument::Const(o)) => i.assert(o),
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?} but got {output:?}",
            ))),
        }
    }
}

impl Display for GenericArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lifetime(i) => Display::fmt(i, f),
            Self::TypeSpecifier(i) => Display::fmt(i, f),
            Self::Const(i) => Display::fmt(i, f),
        }
    }
}

/// Represents an input for the [`super::GenericArguments`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericArguments {
    /// Whether the turbofish syntax is used.
    pub turbofish: bool,

    /// The arguments.
    pub argument_list: ConnectedList<GenericArgument, ConstantPunctuation<','>>,
}

impl Arbitrary for GenericArguments {
    type Parameters = (bool, <GenericArgument as Arbitrary>::Parameters);
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (ConnectedList::arbitrary_with(
            GenericArgument::arbitrary_with(args.1),
            ConstantPunctuation::<','>::arbitrary(),
        ))
        .prop_map(move |argument_list| Self {
            turbofish: args.0,
            argument_list,
        })
        .boxed()
    }
}

impl Input for GenericArguments {
    type Output = super::GenericArguments;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(self.turbofish, output.colon().is_some());

        self.argument_list.assert(output.argument_list())?;

        Ok(())
    }
}

impl Display for GenericArguments {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.turbofish {
            f.write_char(':')?;
        }

        f.write_char('<')?;
        Display::fmt(&self.argument_list, f)?;
        f.write_char('>')?;

        Ok(())
    }
}

/// Represents an input for the [`super::GenericIdentifier`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericIdentifier {
    /// The identifier.
    pub identifier: Identifier,

    /// The optional generic arguments suffix.
    pub generic_arguments: Option<GenericArguments>,
}

impl Arbitrary for GenericIdentifier {
    type Parameters = <GenericArguments as Arbitrary>::Parameters;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            proptest::option::of(GenericArguments::arbitrary_with(args)),
        )
            .prop_map(|(identifier, generic_arguments)| Self {
                identifier,
                generic_arguments,
            })
            .boxed()
    }
}

impl Input for GenericIdentifier {
    type Output = super::GenericIdentifier;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        self.generic_arguments.assert(output.generic_arguments())
    }
}

impl Display for GenericIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.identifier, f)?;

        if let Some(generic_arguments) = self.generic_arguments.as_ref() {
            Display::fmt(generic_arguments, f)?;
        }

        Ok(())
    }
}

/// Represents an input for the [`super::QualifiedIdentifier`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct QualifiedIdentifier {
    /// Whether the qualified identifier starts with a scope separator.
    pub leading_scope_separator: bool,

    /// The first identifier.
    pub first: GenericIdentifier,

    /// The rest of the identifiers.
    pub rest: Vec<GenericIdentifier>,
}

impl Arbitrary for QualifiedIdentifier {
    /// `args` is a tuple of `(use_turbofish, inner_type_specifier_strategy)`.
    ///
    /// - `use_turbofish`: is a boolean that determines whether or not to use turbofish syntax.
    /// - `inner_type_specifier_strategy`: is an optional strategy for generating the inner type
    ///   specifiers.
    type Parameters = (bool, Option<BoxedStrategy<Expression>>);
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let leaf = (proptest::bool::ANY, Identifier::arbitrary()).prop_map(
            |(leading_separator, identifier)| Self {
                leading_scope_separator: leading_separator,
                first: GenericIdentifier {
                    identifier,
                    generic_arguments: None,
                },
                rest: Vec::new(),
            },
        );

        leaf.prop_recursive(8, 64, 8, move |inner| {
            let generic_identifier_strategy =
                GenericIdentifier::arbitrary_with((args.0, (Some(inner), args.1.clone()))).boxed();

            (
                proptest::bool::ANY,
                generic_identifier_strategy.clone(),
                proptest::collection::vec(generic_identifier_strategy, 0..=7),
            )
                .prop_map(|(leading_separator, first, rest)| Self {
                    leading_scope_separator: leading_separator,
                    first,
                    rest,
                })
        })
        .boxed()
    }
}

impl Input for QualifiedIdentifier {
    type Output = super::QualifiedIdentifier;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(
            self.leading_scope_separator,
            output.leading_scope_separator().is_some()
        );

        self.first.assert(output.first())?;

        prop_assert_eq!(self.rest.len(), output.rest().len());

        for (input, (_, output)) in self.rest.iter().zip(output.rest().iter()) {
            input.assert(output)?;
        }

        Ok(())
    }
}

impl Display for QualifiedIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.leading_scope_separator {
            f.write_str("::")?;
        }

        Display::fmt(&self.first, f)?;
        for identifier in &self.rest {
            f.write_str("::")?;
            Display::fmt(identifier, f)?;
        }

        Ok(())
    }
}

/// Represents an input for the [`super::AccessModifier`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum AccessModifier {
    Public,
    Private,
    Internal,
}

impl Input for AccessModifier {
    type Output = super::AccessModifier;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Public, super::AccessModifier::Public(k)) => {
                prop_assert_eq!(k.keyword, pernixc_lexical::token::KeywordKind::Public);
            }
            (Self::Private, super::AccessModifier::Private(k)) => {
                prop_assert_eq!(k.keyword, pernixc_lexical::token::KeywordKind::Private);
            }
            (Self::Internal, super::AccessModifier::Internal(k)) => {
                prop_assert_eq!(k.keyword, pernixc_lexical::token::KeywordKind::Internal);
            }
            _ => {
                return Err(TestCaseError::fail(format!(
                    "Expected {self:?}, got {output:?}"
                )))
            }
        }

        Ok(())
    }
}

impl Arbitrary for AccessModifier {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::Public),
            Just(Self::Private),
            Just(Self::Internal),
        ]
        .boxed()
    }
}

impl Display for AccessModifier {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Public => write!(formatter, "public"),
            Self::Private => write!(formatter, "private"),
            Self::Internal => write!(formatter, "internal"),
        }
    }
}

pub fn parse<T, F>(source: &str, f: F) -> Result<T, TestCaseError>
where
    F: FnOnce(&mut Parser, &Storage<error::Error>) -> Option<T>,
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

/// Represents an input for the [`super::Label`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Label {
    /// The identifier of the label.
    pub identifier: Identifier,
}

impl Input for Label {
    type Output = super::Label;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.identifier.assert(output.identifier())
    }
}

impl Arbitrary for Label {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        Identifier::arbitrary()
            .prop_map(|identifier| Self { identifier })
            .boxed()
    }
}

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}", self.identifier)
    }
}

proptest! {
    #[test]
    fn qualified_identifier_test(
        qualified_identifier_input in QualifiedIdentifier::arbitrary_with((true, None)),
    ) {
        let source = qualified_identifier_input.to_string();
        let qualified_identifier = parse(
            &source,
            |parser, handler| parser.parse_qualified_identifier(true, handler)
        )?;

        qualified_identifier_input.assert(&qualified_identifier)?;
    }

    #[test]
    #[allow(clippy::redundant_closure_for_method_calls)]
    fn type_specifier_test(
        type_specifier_input in TypeSpecifier::arbitrary(),
    ) {
        let source = type_specifier_input.to_string();
        let type_specifier = parse(
            &source,
            |parser, handler| parser.parse_type_specifier(handler)
        )?;

        type_specifier_input.assert(&type_specifier)?;
    }
}
