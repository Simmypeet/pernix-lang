//! Contains code for generating the term for testing purposes.

use proptest::{
    arbitrary::Arbitrary,
    prop_oneof,
    strategy::{BoxedStrategy, Just, Strategy},
};

use super::{MemberSymbolKindID, Pointer, Primitive, Qualifier, Reference, SymbolKindID, Type};
use crate::{
    arena::ID,
    semantic::term::{constant::Constant, lifetime::Lifetime, MemberSymbol, Symbol},
    symbol::TypeParameterID,
};

impl Arbitrary for Primitive {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::Int8),
            Just(Self::Int16),
            Just(Self::Int32),
            Just(Self::Int64),
            Just(Self::Uint8),
            Just(Self::Uint16),
            Just(Self::Uint32),
            Just(Self::Uint64),
            Just(Self::Float32),
            Just(Self::Float64),
            Just(Self::Bool),
            Just(Self::Usize),
            Just(Self::Isize)
        ]
        .boxed()
    }
}

impl Arbitrary for Qualifier {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::Immutable),
            Just(Self::Mutable),
            Just(Self::Restrict),
        ]
        .boxed()
    }
}

impl Arbitrary for Pointer {
    type Parameters = Option<BoxedStrategy<Type>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(type_strategy: Self::Parameters) -> Self::Strategy {
        let type_strategy = type_strategy.unwrap_or_else(Type::arbitrary);
        (type_strategy, Qualifier::arbitrary())
            .prop_map(|(ty, qualifier)| Self {
                pointee: Box::new(ty),
                qualifier,
            })
            .boxed()
    }
}

impl Arbitrary for Reference {
    type Parameters = Option<BoxedStrategy<Type>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(type_strategy: Self::Parameters) -> Self::Strategy {
        let type_strategy = type_strategy.unwrap_or_else(Type::arbitrary);
        (type_strategy, Qualifier::arbitrary(), Lifetime::arbitrary())
            .prop_map(|(ty, qualifier, lifetime)| Self {
                pointee: Box::new(ty),
                lifetime,
                qualifier,
            })
            .boxed()
    }
}

impl Arbitrary for SymbolKindID {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            proptest::num::usize::ANY.prop_map(|x| Self::Struct(ID::new(x))),
            proptest::num::usize::ANY.prop_map(|x| Self::Enum(ID::new(x))),
            proptest::num::usize::ANY.prop_map(|x| Self::Type(ID::new(x))),
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

impl Arbitrary for Type {
    type Parameters = (
        Option<BoxedStrategy<Lifetime>>,
        Option<BoxedStrategy<Constant>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(param: Self::Parameters) -> Self::Strategy {
        let leaf = prop_oneof![
            Primitive::arbitrary().prop_map(Self::Primitive),
            TypeParameterID::arbitrary().prop_map(Self::Parameter),
        ];

        leaf.prop_recursive(8, 48, 6, move |inner| {
            let lt_strat = param.0.clone().unwrap_or_else(|| {
                Lifetime::arbitrary_with((Some(inner.clone()), param.1.clone()))
            });
            let const_strat = param.1.clone().unwrap_or_else(|| {
                Constant::arbitrary_with((param.0.clone(), Some(inner.clone())))
            });

            prop_oneof![
                6 => MemberSymbol::arbitrary_with((
                    Some(lt_strat.clone()),
                    Some(inner.clone()),
                    Some(const_strat.clone())
                ))
                .prop_map(Self::MemberSymbol),
                6 => Symbol::arbitrary_with((Some(lt_strat), Some(inner), Some(const_strat)))
                    .prop_map(Self::Symbol),
            ]
        })
        .boxed()
    }
}
