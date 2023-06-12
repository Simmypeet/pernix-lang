//! Contains the definition of various inputs that correspond to the definitions in defined
//! [`crate::syntax_tree`] module.

use std::fmt::{Debug, Display, Write};

use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_lexical::token::{self, input::Identifier};
use pernixc_system::input::Input;
use proptest::{
    prelude::Arbitrary,
    prop_assert_eq, prop_oneof,
    strategy::{BoxedStrategy, Just, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

/// Represents an input for the [`super::LifetimeArgumentIdentifier`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
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
            .assert(&output.lifetime_argument_identifier)?;
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
        self.lifetime_argument.assert(&output.lifetime_argument)?;
        self.qualifier.assert(&output.qualifier)?;
        self.operand_type.assert(&output.operand_type)
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

impl Display for PrimitiveTypeSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Void => write!(f, "void"),
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
        self.type_specifier.assert(&output.type_specifier)
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

/// Represents an input for the [`super::TypeSpecifier`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum TypeSpecifier {
    Primitive(PrimitiveTypeSpecifier),
    Reference(ReferenceTypeSpecifier),
    QualifiedIdentifier(QualifiedIdentifier),
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
    type Parameters = Option<BoxedStrategy<QualifiedIdentifier>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let leaf = PrimitiveTypeSpecifier::arbitrary().prop_map(TypeSpecifier::Primitive);

        leaf.prop_recursive(8, 64, 8, move |inner| {
            prop_oneof![
                ReferenceTypeSpecifier::arbitrary_with(Some(inner.clone()))
                    .prop_map(TypeSpecifier::Reference),
                args.clone()
                    .unwrap_or_else(move || QualifiedIdentifier::arbitrary_with(
                        QualifiedIdentifierArbitraryParameters {
                            use_turbofish: false,
                            inner_type_specifier_strategy: Some(inner)
                        }
                    ))
                    .prop_map(|mut x| {
                        remove_turbo_fish(&mut x);
                        Self::QualifiedIdentifier(x)
                    })
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
    type Output = token::Punctuation;

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
        self.first.assert(&output.first)?;

        prop_assert_eq!(self.rest.len(), output.rest.len());

        for (input, output) in self.rest.iter().zip(output.rest.iter()) {
            input.0.assert(&output.0)?;
            input.1.assert(&output.1)?;
        }

        self.trailing_separator.assert(&output.trailing_separator)?;

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

/// Represents an input for the [`super::QualifiedIdentifier`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum GenericArgument {
    Lifetime(LifetimeArgument),
    TypeSpecifier(Box<TypeSpecifier>),
}

impl GenericArgument {
    fn arbitrary_with(inner_strategy: InnerStrategy) -> BoxedStrategy<Self> {
        prop_oneof![
            LifetimeArgument::arbitrary().prop_map(GenericArgument::Lifetime),
            match inner_strategy {
                InnerStrategy::TypeSpecifier(s) =>
                    s.prop_map(|x| Self::TypeSpecifier(Box::new(x))).boxed(),
                InnerStrategy::QualifiedIdentifier(s) => TypeSpecifier::arbitrary_with(Some(s))
                    .prop_map(|x| Self::TypeSpecifier(Box::new(x)))
                    .boxed(),
            }
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
        }
    }
}

#[derive(Debug, Clone)]
enum InnerStrategy {
    TypeSpecifier(BoxedStrategy<TypeSpecifier>),
    QualifiedIdentifier(BoxedStrategy<QualifiedIdentifier>),
}

/// Represents an input for the [`super::GenericArguments`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericArguments {
    /// Whether the turbofish syntax is used.
    pub turbofish: bool,

    /// The arguments.
    pub argument_list: ConnectedList<GenericArgument, ConstantPunctuation<','>>,
}

impl Input for GenericArguments {
    type Output = super::GenericArguments;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(self.turbofish, output.colon.is_some());

        self.argument_list.assert(&output.argument_list)?;

        Ok(())
    }
}

impl GenericArguments {
    fn arbitrary_with(turbofish: bool, inner_strategy: InnerStrategy) -> BoxedStrategy<Self> {
        ConnectedList::arbitrary_with(
            GenericArgument::arbitrary_with(inner_strategy),
            ConstantPunctuation::arbitrary(),
        )
        .prop_map(move |argument_list| Self {
            turbofish,
            argument_list,
        })
        .boxed()
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

impl GenericIdentifier {
    fn arbitrary_with(turbofish: bool, inner_strategy: InnerStrategy) -> BoxedStrategy<Self> {
        (
            Identifier::arbitrary(),
            proptest::option::of(GenericArguments::arbitrary_with(turbofish, inner_strategy)),
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
        self.identifier.assert(&output.identifier)?;
        self.generic_arguments.assert(&output.generic_arguments)
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

/// Is used to generate the strategy for [`QualifiedIdentifier::arbitrary_with`].
#[derive(Debug, Clone)]
pub struct QualifiedIdentifierArbitraryParameters {
    /// Whether the qualified identifier uses turbofish syntax.
    pub use_turbofish: bool,

    /// The strategy for generating the inner type specifiers.
    pub inner_type_specifier_strategy: Option<BoxedStrategy<TypeSpecifier>>,
}

impl Default for QualifiedIdentifierArbitraryParameters {
    fn default() -> Self {
        Self {
            use_turbofish: true,
            inner_type_specifier_strategy: Option::default(),
        }
    }
}

impl Arbitrary for QualifiedIdentifier {
    /// `args` is a tuple of `(use_turbofish, inner_type_specifier_strategy)`.
    ///
    /// - `use_turbofish`: is a boolean that determines whether or not to use turbofish syntax.
    /// - `inner_type_specifier_strategy`: is an optional strategy for generating the inner type
    ///   specifiers.
    type Parameters = QualifiedIdentifierArbitraryParameters;
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
            let generic_identifier_strategy = GenericIdentifier::arbitrary_with(
                args.use_turbofish,
                args.inner_type_specifier_strategy.clone().map_or_else(
                    || InnerStrategy::QualifiedIdentifier(inner),
                    InnerStrategy::TypeSpecifier,
                ),
            )
            .boxed();
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
            output.leading_scope_separator.is_some()
        );

        self.first.assert(&output.first)?;

        prop_assert_eq!(self.rest.len(), output.rest.len());

        for (input, (_, output)) in self.rest.iter().zip(output.rest.iter()) {
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
