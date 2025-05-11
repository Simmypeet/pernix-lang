use proptest::prelude::{Arbitrary, BoxedStrategy, Just, Strategy as _};

use crate::{
    arbitrary::QualifiedIdentifier,
    expression::{arbitrary::Expression, prefix::arbitrary::Prefixable},
    r#type::arbitrary::Type,
    reference,
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
    #[derive(Debug, Clone, derive_more::Display)]
    #[display(" {operator} {prefixable}")]
    pub struct BinarySubsequent for super::BinarySubsequent {
        pub operator (Operator),
        pub prefixable (Prefixable),
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display(
        "{first}{}",
        chain.iter().map(ToString::to_string).collect::<String>()
    )]
    pub struct Binary for super::Binary {
        pub first (Prefixable),
        pub chain (Vec<BinarySubsequent>)
    }
}

impl Arbitrary for Binary {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let prefixable = Prefixable::arbitrary_with((
            args.0.clone(),
            args.1.clone(),
            args.2,
        ));

        (
            prefixable.clone(),
            proptest::collection::vec(
                (Operator::arbitrary(), prefixable).prop_map(
                    |(operator, prefixable)| BinarySubsequent {
                        operator,
                        prefixable,
                    },
                ),
                0..4,
            ),
        )
            .prop_map(|(first, chain)| Self { first, chain })
            .boxed()
    }
}
