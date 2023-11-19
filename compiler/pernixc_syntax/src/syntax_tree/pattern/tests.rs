use std::fmt::{Debug, Display};

use enum_as_inner::EnumAsInner;
use pernixc_tests::input::Input;
use proptest::{
    prelude::Arbitrary,
    prop_assert_eq, prop_oneof, proptest,
    strategy::{BoxedStrategy, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use crate::syntax_tree::{
    self,
    expression::tests::{BooleanLiteral, NumericLiteral},
    tests::{ConnectedList, ConstantPunctuation, Identifier},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Named {
    pub is_mutable: bool,
    pub identifier: Identifier,
}

impl Input for Named {
    type Output = super::Named;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(self.is_mutable, output.mutable_keyword.is_some());
        self.identifier.assert(&output.identifier)
    }
}

impl Arbitrary for Named {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (proptest::bool::ANY, Identifier::arbitrary())
            .prop_map(|(is_mutable, identifier)| Self {
                is_mutable,
                identifier,
            })
            .boxed()
    }
}

impl Display for Named {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_mutable {
            write!(f, "mutable ")?;
        }
        write!(f, "{}", self.identifier)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldAssociation<Pattern> {
    identifier: Identifier,
    pattern: Box<Pattern>,
}

impl<Pattern: Input> Input for FieldAssociation<Pattern> {
    type Output = super::FieldAssociation<Pattern::Output>;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.identifier.assert(&output.identifier)?;
        self.pattern.assert(&output.pattern)
    }
}

impl<Pattern: Arbitrary<Strategy = BoxedStrategy<Pattern>> + 'static> Arbitrary
    for FieldAssociation<Pattern>
{
    type Parameters = Option<BoxedStrategy<Pattern>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let pattern = args.unwrap_or_else(Pattern::arbitrary);

        (Identifier::arbitrary(), pattern)
            .prop_map(|(identifier, pattern)| Self {
                identifier,
                pattern: Box::new(pattern),
            })
            .boxed()
    }
}

impl<Pattern: Display> Display for FieldAssociation<Pattern> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.identifier, self.pattern)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Field<Pattern> {
    Association(FieldAssociation<Pattern>),
    Named(Named),
}

impl<Pattern: Input + Debug> Input for Field<Pattern>
where
    Pattern::Output: Debug,
{
    type Output = super::Field<Pattern::Output>;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Association(input), super::Field::Association(output)) => input.assert(output),
            (Self::Named(input), super::Field::Named(output)) => input.assert(output),
            (input, output) => Err(TestCaseError::fail(format!(
                "Expected {input:?} but got {output:?}",
            ))),
        }
    }
}

impl<Pattern: Arbitrary<Strategy = BoxedStrategy<Pattern>> + 'static> Arbitrary for Field<Pattern> {
    type Parameters = Option<BoxedStrategy<Pattern>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Named::arbitrary().prop_map(Field::Named),
            FieldAssociation::arbitrary_with(args).prop_map(Field::Association)
        ]
        .boxed()
    }
}

impl<Pattern: Display> Display for Field<Pattern> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Association(association) => write!(f, "{association}"),
            Self::Named(named) => write!(f, "{named}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Structural<Pattern> {
    pub fields: Option<ConnectedList<Field<Pattern>, ConstantPunctuation<','>>>,
}

impl<Pattern: Input + Debug> Input for Structural<Pattern>
where
    Pattern::Output: Debug,
{
    type Output = super::Structural<Pattern::Output>;

    fn assert(&self, output: &Self::Output) -> TestCaseResult { self.fields.assert(&output.fields) }
}

impl<Pattern: Arbitrary<Strategy = BoxedStrategy<Pattern>> + 'static> Arbitrary
    for Structural<Pattern>
{
    type Parameters = Option<BoxedStrategy<Pattern>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        proptest::option::of(ConnectedList::arbitrary_with(
            Field::arbitrary_with(args),
            ConstantPunctuation::<','>::arbitrary(),
        ))
        .prop_map(|fields| Self { fields })
        .boxed()
    }
}

impl<Pattern: Display> Display for Structural<Pattern> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ ")?;
        if let Some(fields) = &self.fields {
            write!(f, "{fields}")?;
        }
        write!(f, " }}")?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unpack {
    pub is_mutable: bool,
    pub identifier: Identifier,
}

impl Input for Unpack {
    type Output = super::Unpack;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(self.is_mutable, output.mutable_keyword.is_some());
        self.identifier.assert(&output.identifier)
    }
}

impl Arbitrary for Unpack {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (proptest::bool::ANY, Identifier::arbitrary())
            .prop_map(|(is_mutable, identifier)| Self {
                is_mutable,
                identifier,
            })
            .boxed()
    }
}

impl Display for Unpack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "...")?;
        if self.is_mutable {
            write!(f, "mutable ")?;
        }
        write!(f, "{}", self.identifier)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Unpackable<Pattern> {
    Unpack(Unpack),
    Pattern(Box<Pattern>),
}

impl<Pattern: Input + Debug> Input for Unpackable<Pattern>
where
    Pattern::Output: Debug,
{
    type Output = super::Unpackable<Pattern::Output>;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Unpack(input), super::Unpackable::Unpack(output)) => input.assert(output),
            (Self::Pattern(input), super::Unpackable::Pattern(output)) => input.assert(output),
            (input, output) => Err(TestCaseError::fail(format!(
                "Expected {input:?} but got {output:?}",
            ))),
        }
    }
}

impl<Pattern: Arbitrary<Strategy = BoxedStrategy<Pattern>> + 'static> Arbitrary
    for Unpackable<Pattern>
{
    type Parameters = Option<BoxedStrategy<Pattern>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let pattern = args.unwrap_or_else(Pattern::arbitrary);

        prop_oneof![
            Unpack::arbitrary().prop_map(Self::Unpack),
            pattern.prop_map(|x| Self::Pattern(Box::new(x)))
        ]
        .boxed()
    }
}

impl<Pattern: Display> Display for Unpackable<Pattern> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unpack(unpack) => write!(f, "{unpack}"),
            Self::Pattern(pattern) => write!(f, "{pattern}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple<Pattern> {
    pub patterns: Option<ConnectedList<Unpackable<Pattern>, ConstantPunctuation<','>>>,
}

impl<Pattern: Input + Debug> Input for Tuple<Pattern>
where
    Pattern::Output: Debug,
{
    type Output = super::Tuple<Pattern::Output>;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.patterns.assert(&output.patterns)
    }
}

impl<Pattern: Arbitrary<Strategy = BoxedStrategy<Pattern>> + 'static> Arbitrary for Tuple<Pattern> {
    type Parameters = Option<BoxedStrategy<Pattern>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        proptest::option::of(ConnectedList::arbitrary_with(
            Unpackable::arbitrary_with(args),
            ConstantPunctuation::<','>::arbitrary(),
        ))
        .prop_map(|patterns| Self { patterns })
        .boxed()
    }
}

impl<Pattern: Display> Display for Tuple<Pattern> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        if let Some(patterns) = &self.patterns {
            write!(f, "{patterns}")?;
        }
        write!(f, ")")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Irrefutable {
    Structural(Structural<Self>),
    Tuple(Tuple<Self>),
    Named(Named),
}

impl Input for Irrefutable {
    type Output = super::Irrefutable;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Structural(input), super::Irrefutable::Structural(output)) => {
                input.assert(output)
            }
            (Self::Tuple(input), super::Irrefutable::Tuple(output)) => input.assert(output),
            (Self::Named(input), super::Irrefutable::Named(output)) => input.assert(output),
            (input, output) => Err(TestCaseError::fail(format!(
                "Expected {input:?} but got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for Irrefutable {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        let leaf = Named::arbitrary().prop_map(Self::Named);

        leaf.prop_recursive(4, 24, 6, |inner| {
            prop_oneof![
                Structural::arbitrary_with(Some(inner.clone())).prop_map(Self::Structural),
                Tuple::arbitrary_with(Some(inner)).prop_map(Self::Tuple),
            ]
        })
        .boxed()
    }
}

impl Display for Irrefutable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Structural(structural) => write!(f, "{structural}"),
            Self::Tuple(tuple) => write!(f, "{tuple}"),
            Self::Named(named) => write!(f, "{named}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Enum<Pattern> {
    pub identifier: Identifier,
    pub pattern: Box<Pattern>,
}

impl<Pattern: Input + Debug> Input for Enum<Pattern>
where
    Pattern::Output: Debug,
{
    type Output = super::Enum<Pattern::Output>;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.identifier.assert(&output.identifier)?;
        self.pattern.assert(&output.pattern)
    }
}

impl<Pattern: Arbitrary<Strategy = BoxedStrategy<Pattern>> + 'static> Arbitrary for Enum<Pattern> {
    type Parameters = Option<BoxedStrategy<Pattern>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            args.unwrap_or_else(Pattern::arbitrary),
        )
            .prop_map(|(identifier, pattern)| Self {
                identifier,
                pattern: Box::new(pattern),
            })
            .boxed()
    }
}

impl<Pattern: Display> Display for Enum<Pattern> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({})", self.identifier, self.pattern)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Refutable {
    BooleanLiteral(BooleanLiteral),
    NumericLiteral(NumericLiteral),
    Structural(Structural<Self>),
    Tuple(Tuple<Self>),
    Enum(Enum<Self>),
    Named(Named),
}

impl Input for Refutable {
    type Output = super::Refutable;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::BooleanLiteral(input), super::Refutable::BooleanLiteral(output)) => {
                input.assert(output)
            }
            (Self::NumericLiteral(input), super::Refutable::NumericLiteral(output)) => {
                input.assert(output)
            }
            (Self::Structural(input), super::Refutable::Structural(output)) => input.assert(output),
            (Self::Tuple(input), super::Refutable::Tuple(output)) => input.assert(output),
            (Self::Enum(input), super::Refutable::Enum(output)) => input.assert(output),
            (Self::Named(input), super::Refutable::Named(output)) => input.assert(output),
            (input, output) => Err(TestCaseError::fail(format!(
                "Expected {input:?} but got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for Refutable {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        let leaf = prop_oneof![
            BooleanLiteral::arbitrary().prop_map(Self::BooleanLiteral),
            NumericLiteral::arbitrary().prop_map(Self::NumericLiteral),
            Named::arbitrary().prop_map(Self::Named),
        ];

        leaf.prop_recursive(4, 24, 6, |inner| {
            prop_oneof![
                Structural::arbitrary_with(Some(inner.clone())).prop_map(Self::Structural),
                Tuple::arbitrary_with(Some(inner.clone())).prop_map(Self::Tuple),
                Enum::arbitrary_with(Some(inner)).prop_map(Self::Enum),
            ]
        })
        .boxed()
    }
}

impl Display for Refutable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BooleanLiteral(boolean_literal) => {
                write!(f, "{boolean_literal}")
            }
            Self::NumericLiteral(numeric_literal) => {
                write!(f, "{numeric_literal}")
            }
            Self::Structural(structural) => write!(f, "{structural}"),
            Self::Tuple(tuple) => write!(f, "{tuple}"),
            Self::Enum(enum_) => write!(f, "{enum_}"),
            Self::Named(named) => write!(f, "{named}"),
        }
    }
}

proptest! {
    #[test]
    #[allow(clippy::redundant_closure_for_method_calls, clippy::ignored_unit_patterns)]
    fn irrefutable_test(
        irrefutable_input in Irrefutable::arbitrary()
    ) {
        let source = irrefutable_input.to_string();

        let irrefutable = syntax_tree::tests::parse(
            &source,
            |parser, handler| parser.parse_irrefutable_pattern(handler)
        )?;

        irrefutable_input.assert(&irrefutable)?;
    }


    #[test]
    #[allow(clippy::redundant_closure_for_method_calls, clippy::ignored_unit_patterns)]
    fn refutable_test(
        refutable_input in Refutable::arbitrary()
    ) {
        let source = refutable_input.to_string();

        let irrefutable = syntax_tree::tests::parse(
            &source,
            |parser, handler| parser.parse_refutable_pattern(handler)
        )?;

        refutable_input.assert(&irrefutable)?;
    }
}
