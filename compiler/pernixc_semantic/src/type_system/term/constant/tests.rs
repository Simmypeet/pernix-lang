use proptest::{
    arbitrary::Arbitrary,
    prop_oneof,
    strategy::{BoxedStrategy, Strategy},
};

use super::{Array, Constant, Enum, Primitive, Struct};
use crate::{
    arena::ID,
    symbol::ConstantParameterID,
    type_system::{
        model::Default,
        term::{Local, Tuple},
    },
};

impl Arbitrary for Primitive {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            proptest::num::i128::ANY.prop_map(Self::Integer),
            proptest::bool::ANY.prop_map(Self::Bool),
        ]
        .boxed()
    }
}

impl Arbitrary for Struct<Default> {
    type Parameters = Option<BoxedStrategy<Constant<Default>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            ID::arbitrary(),
            proptest::collection::vec(
                args.unwrap_or_else(Constant::arbitrary),
                0..=2,
            ),
        )
            .prop_map(|(id, fields)| Self { id, fields })
            .boxed()
    }
}

impl Arbitrary for Enum<Default> {
    type Parameters = Option<BoxedStrategy<Constant<Default>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            ID::arbitrary(),
            proptest::option::of(args.unwrap_or_else(Constant::arbitrary)),
        )
            .prop_map(|(variant_id, associated_value)| Self {
                variant_id,
                associated_value: associated_value.map(Box::new),
            })
            .boxed()
    }
}

impl Arbitrary for Array<Default> {
    type Parameters = Option<BoxedStrategy<Constant<Default>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(
            args.unwrap_or_else(Constant::arbitrary),
            0..=2,
        )
        .prop_map(|elements| Self { elements })
        .boxed()
    }
}

impl Arbitrary for Constant<Default> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let leaf = prop_oneof![
            Primitive::arbitrary().prop_map(Self::Primitive),
            ConstantParameterID::arbitrary().prop_map(Self::Parameter)
        ];

        leaf.prop_recursive(8, 48, 6, move |inner| {
            prop_oneof![
                2 => Struct::arbitrary_with(Some(inner.clone())).prop_map(Self::Struct),
                1 => Enum::arbitrary_with(Some(inner.clone())).prop_map(Self::Enum),
                1 => inner.clone().prop_map(|x| Self::Local(Local(Box::new(x)))),
                2 => Array::arbitrary_with(Some(inner.clone())).prop_map(Self::Array),
                2 => Tuple::arbitrary_with(Some(inner))
                    .prop_map(Self::Tuple),
            ]
        })
            .boxed()
    }
}
