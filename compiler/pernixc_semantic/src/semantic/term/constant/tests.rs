use proptest::{
    arbitrary::Arbitrary,
    prop_oneof,
    strategy::{BoxedStrategy, Strategy},
};

use super::{Constant, MemberSymbolKindID, Primitive};
use crate::{
    arena::ID,
    semantic::term::{lifetime::Lifetime, r#type::Type, MemberSymbol, Symbol},
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

impl Arbitrary for Constant {
    type Parameters = (Option<BoxedStrategy<Lifetime>>, Option<BoxedStrategy<Type>>);
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
                6 => Symbol::arbitrary_with((Some(lt_strat), Some(ty_strat), Some(inner)))
                    .prop_map(Self::Symbol)
            ]
        })
        .boxed()
    }
}
