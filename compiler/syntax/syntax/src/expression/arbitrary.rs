use std::fmt::Write;

use enum_as_inner::EnumAsInner;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Strategy},
    prop_oneof,
};

use super::{
    binary::arbitrary::{Binary, Node},
    postfix::arbitrary::Postfix,
    prefix::arbitrary::Prefixable,
    terminator::arbitrary::Terminator,
    unit::arbitrary::{Boolean, Numeric, Unit},
};
use crate::{
    arbitrary::{IndentDisplay, IntoSeparated, QualifiedIdentifier},
    r#type::arbitrary::Type,
    reference,
    statement::arbitrary::Statement,
};

reference! {
    #[derive(Debug, Clone, EnumAsInner)]
    pub enum Expression for super::Expression {
        Binary(Binary),
        Terminator(Terminator),
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
            Boolean::arbitrary().prop_map(|boolean| {
                Self::Binary(Binary {
                    first: Node::Prefixable(Prefixable::Postfix(Postfix {
                        unit: Unit::Boolean(boolean),
                        operators: Vec::default(),
                    })),
                    chain: Vec::default(),
                })
            }),
            Numeric::arbitrary().prop_map(|numeric| {
                Self::Binary(Binary {
                    first: Node::Prefixable(Prefixable::Postfix(Postfix {
                        unit: Unit::Numeric(numeric),
                        operators: Vec::default(),
                    })),
                    chain: Vec::default(),
                })
            })
        ];

        leaf.prop_recursive(4, 16, 4, move |expr| {
            prop_oneof![
                2 => Binary::arbitrary_with((
                    Some(expr.clone()),
                    args.0.clone(),
                    args.1.clone(),
                    args.2.clone()
                )).prop_map(Expression::Binary),
                1 => Terminator::arbitrary_with((
                    Some(expr),
                    args.0.clone(),
                    args.1.clone(),
                    args.2.clone()
                )).prop_map(Expression::Terminator),
            ]
        })
        .boxed()
    }
}

impl IndentDisplay for Expression {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Binary(binary) => binary.indent_fmt(f, indent),
            Self::Terminator(terminator) => terminator.indent_fmt(f, indent),
        }
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Call for super::Call {
        pub expressions (Vec<Expression>)
    }
}

impl IndentDisplay for Call {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_char('(')?;
        self.expressions.into_separated(", ").indent_fmt(f, indent)?;
        f.write_char(')')
    }
}

impl Arbitrary for Call {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(
            args.unwrap_or_else(Expression::arbitrary),
            0..10,
        )
        .prop_map(|expressions| Self { expressions })
        .boxed()
    }
}
