use std::fmt::{Debug, Display};

use enum_as_inner::EnumAsInner;
use pernixc_lexical::token;
use pernixc_tests::input::Input;
use proptest::{
    prelude::Arbitrary,
    prop_assert_eq, prop_oneof,
    strategy::{BoxedStrategy, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use crate::syntax_tree::{
    expression::strategy::Boolean,
    strategy::{ConnectedList, ConstantPunctuation, Identifier, ReferenceOf},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Named {
    pub is_mutable: bool,
    pub reference_of: Option<ReferenceOf>,
    pub identifier: Identifier,
}

impl Input<&super::Named> for &Named {
    fn assert(self, output: &super::Named) -> TestCaseResult {
        prop_assert_eq!(self.is_mutable, output.mutable_keyword.is_some());
        self.reference_of.as_ref().assert(output.reference_of.as_ref())?;
        self.identifier.assert(&output.identifier)
    }
}

impl Arbitrary for Named {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            proptest::bool::ANY,
            proptest::option::of(ReferenceOf::arbitrary()),
            Identifier::arbitrary(),
        )
            .prop_map(|(is_mutable, reference_of, identifier)| Self {
                is_mutable,
                reference_of,
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

        if let Some(reference_of) = &self.reference_of {
            write!(f, "{reference_of}")?;
        }

        write!(f, "{}", self.identifier)
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
        write!(f, "..")
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
    pub wildcard: bool,
}

impl<I: Debug, O: Debug> Input<&super::Structural<O>> for &Structural<I>
where
    for<'i, 'o> &'i I: Input<&'o O>,
{
    fn assert(self, output: &super::Structural<O>) -> TestCaseResult {
        assert_eq!(self.wildcard, output.wildcard.is_some());
        self.fields.as_ref().assert(output.fields.as_ref())
    }
}

impl<Pattern: Arbitrary<Strategy = BoxedStrategy<Pattern>> + 'static> Arbitrary
    for Structural<Pattern>
{
    type Parameters = Option<BoxedStrategy<Pattern>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(ConnectedList::arbitrary_with(
                Field::arbitrary_with(args),
                ConstantPunctuation::<','>::arbitrary(),
            )),
            proptest::bool::ANY,
        )
            .prop_map(|(fields, wildcard)| Self { fields, wildcard })
            .boxed()
    }
}

impl<Pattern: Display> Display for Structural<Pattern> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ ")?;
        if let Some(fields) = &self.fields {
            write!(f, "{fields}")?;
        }

        if self.wildcard {
            write!(f, "..")?;
        }

        write!(f, " }}")?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TupleElement<Pattern> {
    pub ellipsis: bool,
    pub pattern: Box<Pattern>,
}

impl<I: Debug, O: Debug> Input<&super::TupleElement<O>> for &TupleElement<I>
where
    for<'i, 'o> &'i I: Input<&'o O>,
{
    fn assert(self, output: &super::TupleElement<O>) -> TestCaseResult {
        prop_assert_eq!(self.ellipsis, output.ellipsis.is_some());
        self.pattern.assert(&output.pattern)
    }
}

impl<Pattern: Arbitrary<Strategy = BoxedStrategy<Pattern>> + 'static> Arbitrary
    for TupleElement<Pattern>
{
    type Parameters = Option<BoxedStrategy<Pattern>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (proptest::bool::ANY, args.unwrap_or_else(Pattern::arbitrary))
            .prop_map(|(ellipsis, pattern)| Self {
                ellipsis,
                pattern: Box::new(pattern),
            })
            .boxed()
    }
}

impl<Pattern: Display> Display for TupleElement<Pattern> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.ellipsis {
            write!(f, "...")?;
        }

        write!(f, "{}", self.pattern)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple<Pattern> {
    pub patterns:
        Option<ConnectedList<TupleElement<Pattern>, ConstantPunctuation<','>>>,
}

impl<I: Debug, O: Debug> Input<&super::Tuple<O>> for &Tuple<I>
where
    for<'i, 'o> &'i I: Input<&'o O>,
{
    fn assert(self, output: &super::Tuple<O>) -> TestCaseResult {
        self.patterns.as_ref().assert(output.connected_list.as_ref())
    }
}

impl<Pattern: Arbitrary<Strategy = BoxedStrategy<Pattern>> + 'static> Arbitrary
    for Tuple<Pattern>
{
    type Parameters = Option<BoxedStrategy<Pattern>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let pattern = TupleElement::arbitrary_with(Some(
            args.unwrap_or_else(Pattern::arbitrary),
        ));
        proptest::option::of(ConnectedList::arbitrary_with(
            pattern,
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
pub struct Enum {
    pub identifier: Identifier,
    pub association: Option<Box<Refutable>>,
}

impl Input<&super::Enum> for &Enum {
    fn assert(self, output: &super::Enum) -> TestCaseResult {
        (&self.identifier).assert(&output.identifier)?;
        self.association
            .as_ref()
            .assert(output.association.as_ref().map(|x| &x.tree))
    }
}

impl Arbitrary for Enum {
    type Parameters = Option<BoxedStrategy<Refutable>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            proptest::option::of(args.unwrap_or_else(Refutable::arbitrary)),
        )
            .prop_map(|(identifier, pattern)| Self {
                identifier,
                association: pattern.map(Box::new),
            })
            .boxed()
    }
}

impl Display for Enum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "case {}", self.identifier)?;

        if let Some(pattern) = &self.association {
            write!(f, "({pattern})")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Integer {
    pub minus: bool,
    pub value: token::strategy::Numeric,
}

impl Input<&super::Integer> for &Integer {
    fn assert(self, output: &super::Integer) -> TestCaseResult {
        prop_assert_eq!(self.minus, output.minus.is_some());
        self.value.assert(&output.numeric)
    }
}

impl Arbitrary for Integer {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (proptest::bool::ANY, token::strategy::Numeric::arbitrary())
            .prop_map(|(minus, value)| Self { minus, value })
            .boxed()
    }
}

impl Display for Integer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.minus {
            write!(f, "-")?;
        }

        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Refutable {
    BooleanLiteral(Boolean),
    Integer(Integer),
    Structural(Structural<Self>),
    Tuple(Tuple<Self>),
    Enum(Enum),
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
            (Refutable::Integer(input), super::Refutable::Integer(output)) => {
                input.assert(output)
            }
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
            Integer::arbitrary().prop_map(Self::Integer),
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
            Self::Integer(numeric_literal) => {
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
