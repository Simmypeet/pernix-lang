use pernixc_parser::expect;
use proptest::prelude::{Arbitrary, BoxedStrategy, Just, Strategy as _};

use crate::{
    arbitrary::{IndentDisplay, QualifiedIdentifier, ReferenceOf},
    expression::{arbitrary::Expression, postfix::arbitrary::Postfix},
    r#type::arbitrary::Type,
    reference,
};

reference! {
    #[derive(Debug, Clone, Copy, derive_more::Display)]
    pub enum Operator for super::Operator {
        #[display("not ")]
        #{prop_assert(|x| x.kind == expect::Keyword::Not)}
        LogicalNot,

        #[display("-")]
        #{prop_assert(|x| *x.kind == '-')}
        Negate,

        #[display("~")]
        #{prop_assert(|x| *x.kind == '~')}
        BitwiseNot,

        #[display("*")]
        #{prop_assert(|x| *x.kind == '*')}
        Dereference,

        ReferenceOf(ReferenceOf)
    }
}

impl Arbitrary for Operator {
    type Parameters = ();
    type Strategy = proptest::strategy::BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::prop_oneof![
            Just(Self::LogicalNot),
            Just(Self::Negate),
            Just(Self::BitwiseNot),
            Just(Self::Dereference),
            ReferenceOf::arbitrary().prop_map(Operator::ReferenceOf)
        ]
        .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub enum Prefixable for super::Prefixable {
        Prefix(Prefix),
        Postfix(Postfix),
    }
}

impl IndentDisplay for Prefixable {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Prefix(prefix) => prefix.indent_fmt(f, indent),
            Self::Postfix(postfix) => postfix.indent_fmt(f, indent),
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
        Postfix::arbitrary_with(args.clone())
            .prop_map(Prefixable::Postfix)
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

reference! {
    #[derive(Debug, Clone)]
    pub struct Prefix for super::Prefix {
        pub operator (Operator),
        pub prefixable (Box<Prefixable>),
    }
}

impl IndentDisplay for Prefix {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, "{}", self.operator)?;
        self.prefixable.indent_fmt(f, indent)
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
            Operator::arbitrary(),
        )
            .prop_map(|(prefixable, operator)| Self {
                prefixable: Box::new(prefixable),
                operator,
            })
            .boxed()
    }
}
