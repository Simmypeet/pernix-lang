use std::fmt::{Display, Write};

use enum_as_inner::EnumAsInner;
use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Just, Strategy, TestCaseError},
    prop_oneof,
    test_runner::TestCaseResult,
};

use crate::syntax_tree::{
    expression::{postfix::strategy::Postfixable, strategy::Expression},
    r#type::strategy::Type,
    strategy::{IndentDisplay, QualifiedIdentifier, ReferenceOf},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Prefix {
    pub prefixable: Box<Prefixable>,
    pub operator: PrefixOperator,
}

impl Input<&super::Prefix> for &Prefix {
    fn assert(self, output: &super::Prefix) -> TestCaseResult {
        self.prefixable.assert(&output.prefixable)?;
        self.operator.assert(&output.operator)
    }
}

impl Arbitrary for Prefix {
    type Parameters = (
        Option<BoxedStrategy<Prefixable>>,
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            args.0.clone().unwrap_or_else(|| {
                Prefixable::arbitrary_with((
                    args.1.clone(),
                    args.2.clone(),
                    args.3.clone(),
                ))
            }),
            PrefixOperator::arbitrary(),
        )
            .prop_map(|(prefixable, operator)| Self {
                prefixable: Box::new(prefixable),
                operator,
            })
            .boxed()
    }
}

impl IndentDisplay for Prefix {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        self.operator.fmt(f)?;
        self.prefixable.indent_fmt(f, indent)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Prefixable {
    Postfixable(Postfixable),
    Prefix(Prefix),
}

impl Input<&super::Prefixable> for &Prefixable {
    fn assert(self, output: &super::Prefixable) -> TestCaseResult {
        match (self, output) {
            (
                Prefixable::Postfixable(input),
                super::Prefixable::Postfixable(output),
            ) => input.assert(output),
            (Prefixable::Prefix(input), super::Prefixable::Prefix(output)) => {
                input.assert(output)
            }

            (input, output) => Err(TestCaseError::fail(format!(
                "expected {input:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for Prefixable {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        Postfixable::arbitrary_with(args.clone())
            .prop_map(Prefixable::Postfixable)
            .prop_recursive(3, 3, 3, move |inner| {
                Prefix::arbitrary_with((
                    Some(inner),
                    args.0.clone(),
                    args.1.clone(),
                    args.2.clone(),
                ))
                .prop_map(Prefixable::Prefix)
            })
            .boxed()
    }
}

impl IndentDisplay for Prefixable {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Postfixable(postfixable) => postfixable.indent_fmt(f, indent),
            Self::Prefix(prefix) => prefix.indent_fmt(f, indent),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PrefixOperator {
    LogicalNot,
    Negate,
    BitwiseNot,
    Dereference,
    ReferenceOf(ReferenceOf),
}

impl Input<&super::PrefixOperator> for &PrefixOperator {
    fn assert(self, output: &super::PrefixOperator) -> TestCaseResult {
        match (self, output) {
            (
                PrefixOperator::LogicalNot,
                super::PrefixOperator::LogicalNot(_),
            )
            | (PrefixOperator::Negate, super::PrefixOperator::Negate(_))
            | (
                PrefixOperator::BitwiseNot,
                super::PrefixOperator::BitwiseNot(_),
            )
            | (
                PrefixOperator::Dereference,
                super::PrefixOperator::Dereference(_),
            ) => Ok(()),

            (
                PrefixOperator::ReferenceOf(input),
                super::PrefixOperator::ReferenceOf(output),
            ) => input.assert(output),

            (input, output) => Err(TestCaseError::fail(format!(
                "expected {input:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for PrefixOperator {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::LogicalNot),
            Just(Self::Negate),
            Just(Self::BitwiseNot),
            Just(Self::Dereference),
            ReferenceOf::arbitrary_with(()).prop_map(Self::ReferenceOf),
        ]
        .boxed()
    }
}

impl Display for PrefixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LogicalNot => f.write_char('!'),
            Self::Negate => f.write_char('-'),
            Self::BitwiseNot => f.write_char('~'),
            Self::Dereference => f.write_char('*'),
            Self::ReferenceOf(reference_of) => reference_of.fmt(f),
        }
    }
}
