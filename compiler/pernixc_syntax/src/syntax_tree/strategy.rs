use std::{
    fmt::{Debug, Display, Write},
    str::FromStr,
};

use enum_as_inner::EnumAsInner;
use pernixc_lexical::token::{self, KeywordKind, Punctuation};
use pernixc_test_input::Input;
use proptest::{
    prelude::Arbitrary,
    prop_assert_eq, prop_oneof,
    strategy::{BoxedStrategy, Just, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use super::{expression::strategy::Expression, r#type::strategy::Type};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier {
    pub string: String,
}

impl Arbitrary for Identifier {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
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

impl Input<&pernixc_lexical::token::Identifier> for &Identifier {
    fn assert(
        self,
        output: &pernixc_lexical::token::Identifier,
    ) -> TestCaseResult {
        prop_assert_eq!(&self.string, output.span.str());
        Ok(())
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.string)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum LifetimeIdentifier {
    Static,
    Identifier(Identifier),
    Elided,
}

impl Arbitrary for LifetimeIdentifier {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::Static),
            Identifier::arbitrary().prop_map(Self::Identifier),
            Just(Self::Elided)
        ]
        .boxed()
    }
}
impl Display for LifetimeIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Static => f.write_str("static"),
            Self::Identifier(ident) => Display::fmt(ident, f),
            Self::Elided => f.write_str(".."),
        }
    }
}

impl Input<&super::LifetimeIdentifier> for &LifetimeIdentifier {
    fn assert(self, output: &super::LifetimeIdentifier) -> TestCaseResult {
        match (self, output) {
            (
                LifetimeIdentifier::Static,
                super::LifetimeIdentifier::Static(..),
            )
            | (
                LifetimeIdentifier::Elided,
                super::LifetimeIdentifier::Elided(..),
            ) => Ok(()),

            (
                LifetimeIdentifier::Identifier(i),
                super::LifetimeIdentifier::Identifier(o),
            ) => i.assert(o),

            (i, o) => Err(TestCaseError::fail(format!(
                "Expected {i:?} but got {o:?}",
            ))),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Lifetime {
    pub identifier: LifetimeIdentifier,
}

impl Arbitrary for Lifetime {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        LifetimeIdentifier::arbitrary()
            .prop_map(|identifier| Self { identifier })
            .boxed()
    }
}

impl Input<&super::Lifetime> for &Lifetime {
    fn assert(self, output: &super::Lifetime) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        Ok(())
    }
}

impl Display for Lifetime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('\'')?;
        Display::fmt(&self.identifier, f)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct ConstantPunctuation<const CHAR: char>;

impl<const CHAR: char> Arbitrary for ConstantPunctuation<CHAR> {
    type Parameters = ();
    type Strategy = Just<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy { Just(Self) }
}

impl<const CHAR: char> Input<&Punctuation> for &ConstantPunctuation<CHAR> {
    fn assert(self, output: &Punctuation) -> TestCaseResult {
        prop_assert_eq!(CHAR, output.punctuation);
        Ok(())
    }
}

impl<const CHAR: char> Display for ConstantPunctuation<CHAR> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char(CHAR)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConnectedList<T, U> {
    pub first: T,
    pub rest: Vec<(U, T)>,
    pub trailing_separator: Option<U>,
}

impl<T: Debug, U: Debug, V: Debug, W: Debug> Input<&super::ConnectedList<T, U>>
    for &ConnectedList<V, W>
where
    for<'a, 'b> &'a V: Input<&'b T>,
    for<'a, 'b> &'a W: Input<&'b U>,
{
    fn assert(self, output: &super::ConnectedList<T, U>) -> TestCaseResult {
        self.first.assert(output.first())?;
        self.rest.assert(output.rest())?;
        self.trailing_separator
            .as_ref()
            .assert(output.trailing_separator().as_ref())?;

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
    pub fn arbitrary_with(
        element_strategy: impl Strategy<Value = T> + Clone,
        punctuation: impl Strategy<Value = U> + Clone,
    ) -> impl Strategy<Value = Self> {
        (
            element_strategy.clone(),
            proptest::collection::vec(
                (punctuation.clone(), element_strategy),
                0..=7,
            ),
            proptest::option::of(punctuation),
        )
            .prop_map(|(first, rest, trailing_separator)| Self {
                first,
                rest,
                trailing_separator,
            })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Constant {
    Expression(Box<Expression>),
    Elided,
}

impl Arbitrary for Constant {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(strat: Self::Parameters) -> Self::Strategy {
        let strat = strat.unwrap_or_else(Expression::arbitrary);

        prop_oneof![
            strat.prop_map(|x| Self::Expression(Box::new(x))),
            Just(Self::Elided),
        ]
        .boxed()
    }
}

impl Input<&super::Constant> for &Constant {
    fn assert(self, output: &super::Constant) -> TestCaseResult {
        match (self, output) {
            (Constant::Expression(i), super::Constant::Expression(o)) => {
                i.assert(o)
            }
            (Constant::Elided, super::Constant::Elided(..)) => Ok(()),
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?} but got {output:?}",
            ))),
        }
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expression(i) => Display::fmt(i, f),
            Self::Elided => f.write_str(".."),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstantArgument {
    pub constant: Constant,
}

impl Arbitrary for ConstantArgument {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        Constant::arbitrary_with(args)
            .prop_map(|constant| Self { constant })
            .boxed()
    }
}

impl Input<&super::ConstantArgument> for &ConstantArgument {
    fn assert(self, output: &super::ConstantArgument) -> TestCaseResult {
        self.constant.assert(&output.tree)?;
        Ok(())
    }
}

impl Display for ConstantArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{{}}}", self.constant)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum GenericArgument {
    Lifetime(Lifetime),
    Type(Box<Type>),
    Constant(ConstantArgument),
}

impl Arbitrary for GenericArgument {
    type Parameters =
        (Option<BoxedStrategy<Type>>, Option<BoxedStrategy<Expression>>);
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(
        (type_arg, expr_arg): Self::Parameters,
    ) -> Self::Strategy {
        prop_oneof![
            Lifetime::arbitrary().prop_map(Self::Lifetime),
            type_arg
                .unwrap_or_else(|| {
                    Type::arbitrary_with((expr_arg.clone(), None))
                })
                .prop_map(|x| Self::Type(Box::new(x))),
            ConstantArgument::arbitrary_with(expr_arg).prop_map(Self::Constant)
        ]
        .boxed()
    }
}

impl Input<&super::GenericArgument> for &GenericArgument {
    fn assert(self, output: &super::GenericArgument) -> TestCaseResult {
        match (self, output) {
            (
                GenericArgument::Lifetime(i),
                super::GenericArgument::Lifetime(o),
            ) => i.assert(o),
            (GenericArgument::Type(i), super::GenericArgument::Type(o)) => {
                i.assert(o)
            }
            (
                GenericArgument::Constant(i),
                super::GenericArgument::Constant(o),
            ) => i.assert(o),
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
            Self::Type(i) => Display::fmt(i, f),
            Self::Constant(i) => Display::fmt(i, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericArguments {
    pub argument_list:
        Option<ConnectedList<GenericArgument, ConstantPunctuation<','>>>,
}

impl Arbitrary for GenericArguments {
    type Parameters = <GenericArgument as Arbitrary>::Parameters;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        proptest::option::of(ConnectedList::arbitrary_with(
            GenericArgument::arbitrary_with(args),
            ConstantPunctuation::<','>::arbitrary(),
        ))
        .prop_map(move |argument_list| Self { argument_list })
        .boxed()
    }
}

impl Input<&super::GenericArguments> for &GenericArguments {
    fn assert(self, output: &super::GenericArguments) -> TestCaseResult {
        self.argument_list.as_ref().assert(output.connected_list.as_ref())?;

        Ok(())
    }
}

impl Display for GenericArguments {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('[')?;
        if let Some(argument_list) = self.argument_list.as_ref() {
            Display::fmt(argument_list, f)?;
        }
        f.write_char(']')?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericIdentifier {
    pub identifier: Identifier,
    pub generic_arguments: Option<GenericArguments>,
}

impl Arbitrary for GenericIdentifier {
    type Parameters =
        (Option<BoxedStrategy<Type>>, Option<BoxedStrategy<Expression>>);
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

impl Input<&super::GenericIdentifier> for &GenericIdentifier {
    fn assert(self, output: &super::GenericIdentifier) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        self.generic_arguments
            .as_ref()
            .assert(output.generic_arguments().as_ref())
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum QualifiedIdentifierRoot {
    This,
    Target,
    GenericIdentifier(GenericIdentifier),
}

impl Arbitrary for QualifiedIdentifierRoot {
    type Parameters = Option<BoxedStrategy<GenericIdentifier>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::This),
            Just(Self::Target),
            args.unwrap_or_else(GenericIdentifier::arbitrary)
                .prop_map(Self::GenericIdentifier),
        ]
        .boxed()
    }
}

impl Input<&super::QualifiedIdentifierRoot> for &QualifiedIdentifierRoot {
    fn assert(self, output: &super::QualifiedIdentifierRoot) -> TestCaseResult {
        match (self, output) {
            (
                QualifiedIdentifierRoot::This,
                super::QualifiedIdentifierRoot::This(..),
            )
            | (
                QualifiedIdentifierRoot::Target,
                super::QualifiedIdentifierRoot::Target(..),
            ) => Ok(()),

            (
                QualifiedIdentifierRoot::GenericIdentifier(i),
                super::QualifiedIdentifierRoot::GenericIdentifier(o),
            ) => i.assert(o),
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, got {output:?}"
            ))),
        }
    }
}

impl Display for QualifiedIdentifierRoot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::This => f.write_str("this"),
            Self::Target => f.write_str("target"),
            Self::GenericIdentifier(i) => Display::fmt(i, f),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReferenceOf {
    pub is_mutable: bool,
}

impl Input<&super::ReferenceOf> for &ReferenceOf {
    fn assert(self, output: &super::ReferenceOf) -> TestCaseResult {
        prop_assert_eq!(self.is_mutable, output.mutable_keyword.is_some());
        Ok(())
    }
}

impl Arbitrary for ReferenceOf {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::bool::ANY.prop_map(|is_mutable| Self { is_mutable }).boxed()
    }
}

impl Display for ReferenceOf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "&{}", if self.is_mutable { "mutable " } else { "" })
    }
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct QualifiedIdentifier {
    pub root: QualifiedIdentifierRoot,
    pub rest: Vec<GenericIdentifier>,
}

impl Arbitrary for QualifiedIdentifier {
    type Parameters =
        (Option<BoxedStrategy<Type>>, Option<BoxedStrategy<Expression>>);
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let generic_identifier_strategy =
            GenericIdentifier::arbitrary_with(args);
        (
            QualifiedIdentifierRoot::arbitrary_with(Some(
                generic_identifier_strategy.clone(),
            )),
            proptest::collection::vec(generic_identifier_strategy, 0..=6),
        )
            .prop_map(|(root, rest)| Self { root, rest })
            .boxed()
    }
}

impl Input<&super::QualifiedIdentifier> for &QualifiedIdentifier {
    fn assert(self, output: &super::QualifiedIdentifier) -> TestCaseResult {
        self.root.assert(output.root())?;

        prop_assert_eq!(self.rest.len(), output.rest().len());

        for (input, (_, output)) in self.rest.iter().zip(output.rest().iter()) {
            input.assert(output)?;
        }

        Ok(())
    }
}

impl Display for QualifiedIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.root, f)?;
        for identifier in &self.rest {
            f.write_str("::")?;
            Display::fmt(identifier, f)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SimplePathRoot {
    Target,
    Identifier(Identifier),
}

impl Input<&super::SimplePathRoot> for &SimplePathRoot {
    fn assert(self, output: &super::SimplePathRoot) -> TestCaseResult {
        match (self, output) {
            (SimplePathRoot::Target, super::SimplePathRoot::Target(_)) => {
                Ok(())
            }

            (
                SimplePathRoot::Identifier(i),
                super::SimplePathRoot::Identifier(o),
            ) => i.assert(o),

            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for SimplePathRoot {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::Target),
            Identifier::arbitrary().prop_map(Self::Identifier),
        ]
        .boxed()
    }
}

impl Display for SimplePathRoot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Target => f.write_str("target"),
            Self::Identifier(identifier) => Display::fmt(identifier, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SimplePath {
    pub root: SimplePathRoot,
    pub rest: Vec<Identifier>,
}

impl Input<&super::SimplePath> for &SimplePath {
    fn assert(self, output: &super::SimplePath) -> TestCaseResult {
        self.root.assert(output.root())?;
        prop_assert_eq!(self.rest.len(), output.rest().len());

        for (input, (_, output)) in self.rest.iter().zip(output.rest().iter()) {
            input.assert(output)?;
        }

        Ok(())
    }
}

impl Arbitrary for SimplePath {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            SimplePathRoot::arbitrary(),
            proptest::collection::vec(Identifier::arbitrary(), 0..=6),
        )
            .prop_map(|(root, rest)| Self { root, rest })
            .boxed()
    }
}

impl Display for SimplePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.root, f)?;

        for identifier in &self.rest {
            write!(f, "::{identifier}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum AccessModifier {
    Public,
    Private,
    Internal,
}

impl Input<&super::AccessModifier> for &AccessModifier {
    fn assert(self, output: &super::AccessModifier) -> TestCaseResult {
        match (self, output) {
            (AccessModifier::Public, super::AccessModifier::Public(k)) => {
                prop_assert_eq!(
                    k.kind,
                    pernixc_lexical::token::KeywordKind::Public
                );
            }
            (AccessModifier::Private, super::AccessModifier::Private(k)) => {
                prop_assert_eq!(
                    k.kind,
                    pernixc_lexical::token::KeywordKind::Private
                );
            }
            (AccessModifier::Internal, super::AccessModifier::Internal(k)) => {
                prop_assert_eq!(
                    k.kind,
                    pernixc_lexical::token::KeywordKind::Internal
                );
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Label {
    pub identifier: Identifier,
}

impl Input<&super::Label> for &Label {
    fn assert(self, output: &super::Label) -> TestCaseResult {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Numeric {
    pub integer: u64,
}

impl Input<&token::Numeric> for &Numeric {
    fn assert(self, output: &token::Numeric) -> TestCaseResult {
        output.span.str().parse::<u64>().map_or_else(
            |_| {
                Err(TestCaseError::fail(format!(
                    "expected {self:?}, got {output:?}",
                )))
            },
            |x| {
                prop_assert_eq!(self.integer, x);
                Ok(())
            },
        )
    }
}

impl Arbitrary for Numeric {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        proptest::num::u64::ANY.prop_map(|integer| Self { integer }).boxed()
    }
}

impl Display for Numeric {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.integer, f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LifetimeParameter {
    pub identifier: Identifier,
}

impl Input<&super::LifetimeParameter> for &LifetimeParameter {
    fn assert(self, output: &super::LifetimeParameter) -> TestCaseResult {
        self.identifier.assert(output.identifier())
    }
}

impl Arbitrary for LifetimeParameter {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        Identifier::arbitrary()
            .prop_map(|identifier| Self { identifier })
            .boxed()
    }
}

impl Display for LifetimeParameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}", self.identifier)
    }
}
