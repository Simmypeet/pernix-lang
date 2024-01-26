use proptest::{
    arbitrary::Arbitrary,
    prop_oneof,
    strategy::{BoxedStrategy, Strategy},
};

use super::{Array, Constant, Enum, MemberSymbolKindID, Primitive, Struct};
use crate::{
    arena::ID,
    semantic::term::{
        lifetime::Lifetime, r#type::Type, Local, MemberSymbol, Symbol, Tuple,
    },
    symbol::ConstantParameterID,
};

impl Arbitrary for Primitive {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            proptest::num::i8::ANY.prop_map(Self::Int8),
            proptest::num::i16::ANY.prop_map(Self::Int16),
            proptest::num::i32::ANY.prop_map(Self::Int32),
            proptest::num::i64::ANY.prop_map(Self::Int64),
            proptest::num::u8::ANY.prop_map(Self::Uint8),
            proptest::num::u16::ANY.prop_map(Self::Uint16),
            proptest::num::u32::ANY.prop_map(Self::Uint32),
            proptest::num::u64::ANY.prop_map(Self::Uint64),
            proptest::num::usize::ANY.prop_map(Self::Usize),
            proptest::num::isize::ANY.prop_map(Self::Isize),
            proptest::bool::ANY.prop_map(Self::Bool),
        ]
        .boxed()
    }
}

impl Arbitrary for MemberSymbolKindID {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            ID::arbitrary().prop_map(Self::TraitImplementation),
            ID::arbitrary().prop_map(Self::AdtImplementation),
            ID::arbitrary().prop_map(Self::Trait),
        ]
        .boxed()
    }
}

impl Arbitrary for Struct {
    type Parameters = Option<BoxedStrategy<Constant>>;
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

impl Arbitrary for Enum {
    type Parameters = Option<BoxedStrategy<Constant>>;
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

impl Arbitrary for Array {
    type Parameters = Option<BoxedStrategy<Constant>>;
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

impl Arbitrary for Constant {
    type Parameters =
        (Option<BoxedStrategy<Lifetime>>, Option<BoxedStrategy<Type>>);
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(param: Self::Parameters) -> Self::Strategy {
        let leaf = prop_oneof![
            Primitive::arbitrary().prop_map(Self::Primitive),
            ConstantParameterID::arbitrary().prop_map(Self::Parameter)
        ];

        leaf.prop_recursive(8, 48, 6, move |inner| {
            let lt_strat = param.0.clone().unwrap_or_else(|| {
                Lifetime::arbitrary_with((param.1.clone(), Some(inner.clone())))
            });
            let ty_strat = param.1.clone().unwrap_or_else(|| {
                Type::arbitrary_with((Some(lt_strat.clone()), Some(inner.clone())))
            });

            prop_oneof![
                6 => MemberSymbol::arbitrary_with((
                    Some(lt_strat.clone()),
                    Some(ty_strat.clone()),
                    Some(inner.clone())
                ))
                .prop_map(Self::MemberSymbol),
                6 => Symbol::arbitrary_with((Some(lt_strat), Some(ty_strat), Some(inner.clone())))
                    .prop_map(Self::Symbol),
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
