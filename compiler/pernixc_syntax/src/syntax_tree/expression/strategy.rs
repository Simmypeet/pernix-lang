use enum_as_inner::EnumAsInner;
use pernixc_test_input::Input;
use proptest::{
    arbitrary::Arbitrary,
    prelude::prop::test_runner::TestCaseResult,
    prop_oneof,
    strategy::{BoxedStrategy, Strategy},
    test_runner::TestCaseError,
};

use super::{
    binary::strategy::{Binary, BinaryNode},
    postfix::strategy::Postfixable,
    prefix::strategy::Prefixable,
    terminator::strategy::Terminator,
    unit::strategy::{Boolean, Numeric, Unit},
};
use crate::syntax_tree::{
    r#type::strategy::Type,
    statement::strategy::Statement,
    strategy::{IndentDisplay, QualifiedIdentifier},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Expression {
    Binary(Binary),
    Terminator(Terminator),
}

impl Input<&super::Expression> for &Expression {
    fn assert(self, output: &super::Expression) -> TestCaseResult {
        match (self, output) {
            (Expression::Binary(input), super::Expression::Binary(output)) => {
                input.assert(output)
            }
            (
                Expression::Terminator(input),
                super::Expression::Terminator(output),
            ) => input.assert(output),

            (input, output) => Err(TestCaseError::fail(format!(
                "expected {input:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for Expression {
    type Parameters = (
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let leaf = prop_oneof![
            Boolean::arbitrary().prop_map(|x| {
                Self::Binary(Binary {
                    first: BinaryNode::Prefixable(Prefixable::Postfixable(
                        Postfixable::Unit(Unit::Boolean(x)),
                    )),
                    chain: Vec::new(),
                })
            }),
            Numeric::arbitrary().prop_map(|x| {
                Self::Binary(Binary {
                    first: BinaryNode::Prefixable(Prefixable::Postfixable(
                        Postfixable::Unit(Unit::Numeric(x)),
                    )),
                    chain: Vec::new(),
                })
            }),
        ];

        leaf.prop_recursive(4, 64, 16, move |inner| {
            prop_oneof![
                Binary::arbitrary_with((
                    Some(inner.clone()),
                    args.0.clone(),
                    args.1.clone(),
                    args.2.clone()
                ))
                .prop_map(Expression::Binary),
                Terminator::arbitrary_with((
                    Some(inner),
                    args.0.clone(),
                    args.1.clone(),
                    args.2.clone()
                ))
                .prop_map(Expression::Terminator),
            ]
        })
        .boxed()
    }
}

impl IndentDisplay for Expression {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Binary(binary) => binary.indent_fmt(f, indent),
            Self::Terminator(terminator) => terminator.indent_fmt(f, indent),
        }
    }
}
