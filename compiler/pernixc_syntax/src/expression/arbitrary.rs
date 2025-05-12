use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Strategy},
    prop_oneof,
};

use super::{
    binary::arbitrary::{Binary, Node},
    postfix::arbitrary::Postfix,
    prefix::arbitrary::Prefixable,
    unit::arbitrary::{Boolean, Numeric, Unit},
};
use crate::{
    arbitrary::{IndentDisplay, QualifiedIdentifier},
    r#type::arbitrary::Type,
    reference,
    statement::arbitrary::Statement,
};

reference! {
    #[derive(Debug, Clone)]
    pub enum Expression for super::Expression {
        Binary(Binary),
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

        leaf.prop_recursive(4, 40, 10, move |expr| {
            prop_oneof![
                2 => Binary::arbitrary_with((
                    Some(expr),
                    args.0.clone(),
                    args.1.clone(),
                    args.2.clone()
                )).prop_map(Expression::Binary),
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
        }
    }
}
