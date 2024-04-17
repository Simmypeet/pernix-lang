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
    expression::tests::{Boolean, Numeric},
    tests::{ConnectedList, ConstantPunctuation, Identifier, Qualifier},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Binding {
    Value(bool),
    Ref(Option<Qualifier>),
}

impl Display for Binding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(mutable) => {
                if *mutable {
                    write!(f, "mutable")?;
                }

                Ok(())
            }
            Self::Ref(r) => {
                write!(f, "ref")?;
                if let Some(r) = r {
                    write!(f, " {r}")?;
                }

                Ok(())
            }
        }
    }
}

impl Arbitrary for Binding {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            proptest::bool::ANY.prop_map(Binding::Value),
            proptest::option::of(Qualifier::arbitrary()).prop_map(Binding::Ref),
        ]
        .boxed()
    }
}

impl Input<&super::Binding> for &Binding {
    fn assert(self, output: &super::Binding) -> TestCaseResult {
        match (self, output) {
            (
                Binding::Value(input),
                super::Binding::Value { mutable_keyword },
            ) => {
                prop_assert_eq!(*input, mutable_keyword.is_some());
            }
            (Binding::Ref(input), super::Binding::Ref(output)) => {
                input.as_ref().assert(output.qualifier.as_ref())?;
            }
            (input, output) => {
                return Err(TestCaseError::fail(format!(
                    "Expected {input:?} but got {output:?}",
                )));
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Named {
    pub identifier: Identifier,
    pub kind: Binding,
}

impl Input<&super::Named> for &Named {
    fn assert(self, output: &super::Named) -> TestCaseResult {
        self.kind.assert(&output.binding)?;
        self.identifier.assert(&output.identifier)
    }
}

impl Arbitrary for Named {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (Binding::arbitrary(), Identifier::arbitrary())
            .prop_map(|(kind, identifier)| Self { identifier, kind })
            .boxed()
    }
}

impl Display for Named {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.kind, self.identifier)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Wildcard;

impl Input<&super::Wildcard> for &Wildcard {
    fn assert(self, _: &super::Wildcard) -> TestCaseResult { Ok(()) }
}

impl Arbitrary for Wildcard {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        proptest::strategy::Just(Self).boxed()
    }
}

impl Display for Wildcard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "?")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldAssociation<Pattern> {
    identifier: Identifier,
    pattern: Box<Pattern>,
}

impl<I: Debug, O: Debug> Input<&super::FieldAssociation<O>>
    for &FieldAssociation<I>
where
    for<'i, 'o> &'i I: Input<&'o O>,
{
    fn assert(self, output: &super::FieldAssociation<O>) -> TestCaseResult {
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

impl<I: Debug, O: Debug> Input<&super::Field<O>> for &Field<I>
where
    for<'i, 'o> &'i I: Input<&'o O>,
{
    fn assert(self, output: &super::Field<O>) -> TestCaseResult {
        match (self, output) {
            (Field::Association(input), super::Field::Association(output)) => {
                input.assert(output)
            }
            (Field::Named(input), super::Field::Named(output)) => {
                input.assert(output)
            }
            (input, output) => Err(TestCaseError::fail(format!(
                "Expected {input:?} but got {output:?}",
            ))),
        }
    }
}

impl<Pattern: Arbitrary<Strategy = BoxedStrategy<Pattern>> + 'static> Arbitrary
    for Field<Pattern>
{
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

impl<I: Debug, O: Debug> Input<&super::Structural<O>> for &Structural<I>
where
    for<'i, 'o> &'i I: Input<&'o O>,
{
    fn assert(self, output: &super::Structural<O>) -> TestCaseResult {
        self.fields.as_ref().assert(output.fields.as_ref())
    }
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
pub struct Unpacked<Pattern> {
    pub pattern: Box<Pattern>,
}

impl<I: Debug, O: Debug> Input<&super::Unpacked<O>> for &Unpacked<I>
where
    for<'i, 'o> &'i I: Input<&'o O>,
{
    fn assert(self, output: &super::Unpacked<O>) -> TestCaseResult {
        self.pattern.assert(&output.pattern)
    }
}

impl<Pattern: Arbitrary<Strategy = BoxedStrategy<Pattern>> + 'static> Arbitrary
    for Unpacked<Pattern>
{
    type Parameters = Option<BoxedStrategy<Pattern>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(arg: Self::Parameters) -> Self::Strategy {
        let pattern = arg.unwrap_or_else(Pattern::arbitrary);

        pattern.prop_map(|pattern| Self { pattern: Box::new(pattern) }).boxed()
    }
}

impl<Pattern: Display> Display for Unpacked<Pattern> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "...{}", self.pattern)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Unpackable<Pattern> {
    Unpacked(Unpacked<Pattern>),
    Regular(Box<Pattern>),
}

impl<I: Debug, O: Debug> Input<&super::TupleElement<O>> for &Unpackable<I>
where
    for<'i, 'o> &'i I: Input<&'o O>,
{
    fn assert(self, output: &super::TupleElement<O>) -> TestCaseResult {
        match (self, output) {
            (
                Unpackable::Unpacked(input),
                super::TupleElement::Unpacked(output),
            ) => input.assert(output),
            (
                Unpackable::Regular(input),
                super::TupleElement::Regular(output),
            ) => input.assert(output),
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
            Unpacked::arbitrary().prop_map(Self::Unpacked),
            pattern.prop_map(|x| Self::Regular(Box::new(x)))
        ]
        .boxed()
    }
}

impl<Pattern: Display> Display for Unpackable<Pattern> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unpacked(unpack) => write!(f, "{unpack}"),
            Self::Regular(pattern) => write!(f, "{pattern}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple<Pattern> {
    pub patterns:
        Option<ConnectedList<Unpackable<Pattern>, ConstantPunctuation<','>>>,
}

impl<I: Debug, O: Debug> Input<&super::Tuple<O>> for &Tuple<I>
where
    for<'i, 'o> &'i I: Input<&'o O>,
{
    fn assert(self, output: &super::Tuple<O>) -> TestCaseResult {
        self.patterns.as_ref().assert(output.patterns.as_ref())
    }
}

impl<Pattern: Arbitrary<Strategy = BoxedStrategy<Pattern>> + 'static> Arbitrary
    for Tuple<Pattern>
{
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
    Wildcard(Wildcard),
}

impl Input<&super::Irrefutable> for &Irrefutable {
    fn assert(self, output: &super::Irrefutable) -> TestCaseResult {
        match (self, output) {
            (
                Irrefutable::Structural(input),
                super::Irrefutable::Structural(output),
            ) => input.assert(output),
            (Irrefutable::Tuple(input), super::Irrefutable::Tuple(output)) => {
                input.assert(output)
            }
            (Irrefutable::Named(input), super::Irrefutable::Named(output)) => {
                input.assert(output)
            }
            (
                Irrefutable::Wildcard(input),
                super::Irrefutable::Wildcard(output),
            ) => input.assert(output),
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
        let leaf = prop_oneof![
            Named::arbitrary().prop_map(Self::Named),
            Wildcard::arbitrary().prop_map(Self::Wildcard),
        ];

        leaf.prop_recursive(4, 24, 6, |inner| {
            prop_oneof![
                Structural::arbitrary_with(Some(inner.clone()))
                    .prop_map(Self::Structural),
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
            Self::Wildcard(wildcard) => write!(f, "{wildcard}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Enum<Pattern> {
    pub identifier: Identifier,
    pub association: Option<Box<Pattern>>,
}

impl<I: Debug, O: Debug> Input<&super::Enum<O>> for &Enum<I>
where
    for<'i, 'o> &'i I: Input<&'o O>,
{
    fn assert(self, output: &super::Enum<O>) -> TestCaseResult {
        (&self.identifier).assert(&output.identifier)?;
        self.association
            .as_ref()
            .assert(output.association.as_ref().map(|x| &x.pattern))
    }
}

impl<Pattern: Arbitrary<Strategy = BoxedStrategy<Pattern>> + 'static> Arbitrary
    for Enum<Pattern>
{
    type Parameters = Option<BoxedStrategy<Pattern>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            proptest::option::of(args.unwrap_or_else(Pattern::arbitrary)),
        )
            .prop_map(|(identifier, pattern)| Self {
                identifier,
                association: pattern.map(Box::new),
            })
            .boxed()
    }
}

impl<Pattern: Display> Display for Enum<Pattern> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "case {}", self.identifier)?;

        if let Some(pattern) = &self.association {
            write!(f, "({pattern})")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Refutable {
    BooleanLiteral(Boolean),
    NumericLiteral(Numeric),
    Structural(Structural<Self>),
    Tuple(Tuple<Self>),
    Enum(Enum<Self>),
    Named(Named),
    Wildcard(Wildcard),
}

impl Input<&super::Refutable> for &Refutable {
    fn assert(self, output: &super::Refutable) -> TestCaseResult {
        match (self, output) {
            (
                Refutable::BooleanLiteral(input),
                super::Refutable::Boolean(output),
            ) => input.assert(output),
            (
                Refutable::NumericLiteral(input),
                super::Refutable::Numeric(output),
            ) => input.assert(output),
            (
                Refutable::Structural(input),
                super::Refutable::Structural(output),
            ) => input.assert(output),
            (Refutable::Tuple(input), super::Refutable::Tuple(output)) => {
                input.assert(output)
            }
            (Refutable::Enum(input), super::Refutable::Enum(output)) => {
                input.assert(output)
            }
            (Refutable::Named(input), super::Refutable::Named(output)) => {
                input.assert(output)
            }
            (
                Refutable::Wildcard(input),
                super::Refutable::Wildcard(output),
            ) => input.assert(output),
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
            Boolean::arbitrary().prop_map(Self::BooleanLiteral),
            Numeric::arbitrary().prop_map(Self::NumericLiteral),
            Named::arbitrary().prop_map(Self::Named),
            Wildcard::arbitrary().prop_map(Self::Wildcard),
        ];

        leaf.prop_recursive(4, 24, 6, |inner| {
            prop_oneof![
                Structural::arbitrary_with(Some(inner.clone()))
                    .prop_map(Self::Structural),
                Tuple::arbitrary_with(Some(inner.clone()))
                    .prop_map(Self::Tuple),
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
            Self::Wildcard(wildcard) => write!(f, "{wildcard}"),
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
