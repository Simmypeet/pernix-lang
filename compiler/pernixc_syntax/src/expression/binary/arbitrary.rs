use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Just, Strategy as _},
    prop_oneof,
};

use crate::{
    arbitrary::{IndentDisplay, QualifiedIdentifier},
    expression::{
        arbitrary::Expression, block::arbitrary::Block,
        prefix::arbitrary::Prefixable,
    },
    r#type::arbitrary::Type,
    reference,
    statement::arbitrary::Statement,
};

reference! {
    #[derive(Debug, Clone, Copy, derive_more::Display)]
    pub enum Operator for super::Operator {
        #[display("<<=")]
        #{prop_assert(|x| true)}
        CompoundBitwiseLeftShift,

        #[display(">>=")]
        #{prop_assert(|x| true)}
        CompoundBitwiseRightShift,

        #[display("+=")]
        #{prop_assert(|x| true)}
        CompoundAdd,

        #[display("-=")]
        #{prop_assert(|x| true)}
        CompoundSubtract,

        #[display("*=")]
        #{prop_assert(|x| true)}
        CompoundMultiply,

        #[display("/=")]
        #{prop_assert(|x| true)}
        CompoundDivide,

        #[display("%=")]
        #{prop_assert(|x| true)}
        CompoundModulus,

        #[display("&=")]
        #{prop_assert(|x| true)}
        CompoundBitwiseAnd,

        #[display("|=")]
        #{prop_assert(|x| true)}
        CompoundBitwiseOr,

        #[display("^=")]
        #{prop_assert(|x| true)}
        CompoundBitwiseXor,

        #[display("==")]
        #{prop_assert(|x| true)}
        Equal,

        #[display("!=")]
        #{prop_assert(|x| true)}
        NotEqual,

        #[display("<=")]
        #{prop_assert(|x| true)}
        LessThanOrEqual,

        #[display(">=")]
        #{prop_assert(|x| true)}
        GreaterThanOrEqual,

        #[display("<<")]
        #{prop_assert(|x| true)}
        BitwiseLeftShift,

        #[display(">>")]
        #{prop_assert(|x| true)}
        BitwiseRightShift,

        #[display("+")]
        #{prop_assert(|x| true)}
        Add,

        #[display("-")]
        #{prop_assert(|x| true)}
        Subtract,

        #[display("*")]
        #{prop_assert(|x| true)}
        Multiply,

        #[display("/")]
        #{prop_assert(|x| true)}
        Divide,

        #[display("%")]
        #{prop_assert(|x| true)}
        Modulus,

        #[display("&")]
        #{prop_assert(|x| true)}
        BitwiseAnd,

        #[display("|")]
        #{prop_assert(|x| true)}
        BitwiseOr,

        #[display("^")]
        #{prop_assert(|x| true)}
        BitwiseXor,

        #[display(">")]
        #{prop_assert(|x| true)}
        GreaterThan,

        #[display("<")]
        #{prop_assert(|x| true)}
        LessThan,

        #[display("=")]
        #{prop_assert(|x| true)}
        Assign,
    }
}

impl Arbitrary for Operator {
    type Parameters = ();
    type Strategy = proptest::strategy::BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::prop_oneof![
            Just(Self::CompoundBitwiseLeftShift),
            Just(Self::CompoundBitwiseRightShift),
            Just(Self::CompoundAdd),
            Just(Self::CompoundSubtract),
            Just(Self::CompoundMultiply),
            Just(Self::CompoundDivide),
            Just(Self::CompoundModulus),
            Just(Self::CompoundBitwiseAnd),
            Just(Self::CompoundBitwiseOr),
            Just(Self::CompoundBitwiseXor),
            Just(Self::Equal),
            Just(Self::NotEqual),
            Just(Self::LessThanOrEqual),
            Just(Self::GreaterThanOrEqual),
            Just(Self::BitwiseLeftShift),
            Just(Self::BitwiseRightShift),
            Just(Self::Add),
            Just(Self::Subtract),
            Just(Self::Multiply),
            Just(Self::Divide),
            Just(Self::Modulus),
            Just(Self::BitwiseAnd),
            Just(Self::BitwiseOr),
            Just(Self::BitwiseXor),
            Just(Self::GreaterThan),
            Just(Self::LessThan),
            Just(Self::Assign)
        ]
        .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub enum Node for super::Node {
        Prefixable(Prefixable),
        Block(Block),
    }
}

impl Arbitrary for Node {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Prefixable::arbitrary_with((
                args.0.clone(),
                args.1.clone(),
                args.2.clone(),
            ))
            .prop_map(Node::Prefixable),
            Block::arbitrary_with(args).prop_map(Node::Block),
        ]
        .boxed()
    }
}

impl IndentDisplay for Node {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Prefixable(prefixable) => prefixable.indent_fmt(f, indent),
            Self::Block(block) => block.indent_fmt(f, indent),
        }
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct BinarySubsequent for super::BinarySubsequent {
        pub operator (Operator),
        pub node (Node),
    }
}

impl IndentDisplay for BinarySubsequent {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, " {} ", self.operator)?;
        self.node.indent_fmt(f, indent)
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Binary for super::Binary {
        pub first (Node),
        pub chain (Vec<BinarySubsequent>)
    }
}

impl IndentDisplay for Binary {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        self.first.indent_fmt(f, indent)?;
        for subsequent in &self.chain {
            subsequent.indent_fmt(f, indent)?;
        }
        Ok(())
    }
}

impl Arbitrary for Binary {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let prefixable = Prefixable::arbitrary_with((
            args.0.clone(),
            args.1.clone(),
            args.2.clone(),
        ))
        .prop_map(Node::Prefixable);

        let trailing = Block::arbitrary_with(args).prop_map(Node::Block);

        prop_oneof![
            1 => trailing
                .clone()
                .prop_map(|x| Self { first: x, chain: Vec::default() }),

            5 => (
                prefixable.clone(),
                proptest::collection::vec(
                    (Operator::arbitrary(), prefixable).prop_map(
                        |(operator, node)| BinarySubsequent {
                            operator,
                            node,
                        },
                    ),
                    0..4,
                ),
                proptest::option::of((Operator::arbitrary(), trailing)),
            )
            .prop_map(|(first, chain, trailing)| {
                let mut result = Self { first, chain };

                if let Some((operator, node)) = trailing {
                    result.chain.push(BinarySubsequent { operator, node });
                }

                result
            })
        ]
        .boxed()
    }
}
