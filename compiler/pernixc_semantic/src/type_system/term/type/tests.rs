//! Contains code for generating the term for testing purposes.

use proptest::{
    arbitrary::Arbitrary,
    prop_oneof,
    strategy::{BoxedStrategy, Just, Strategy},
};

use super::{Array, Pointer, Primitive, Qualifier, Reference, SymbolID, Type};
use crate::{
    arena::ID,
    symbol::TypeParameterID,
    type_system::{
        model::Default,
        term::{
            constant::Constant, lifetime::Lifetime, Local, MemberSymbol,
            Symbol, Tuple,
        },
    },
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
            Just(Self::Unique),
        ]
        .boxed()
    }
}

impl Arbitrary for Pointer<Default> {
    type Parameters = Option<BoxedStrategy<Type<Default>>>;
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

impl Arbitrary for Reference<Default> {
    type Parameters = (
        Option<BoxedStrategy<Lifetime<Default>>>,
        Option<BoxedStrategy<Type<Default>>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(
        (lt_strategy, type_strategy): Self::Parameters,
    ) -> Self::Strategy {
        let type_strategy = type_strategy.unwrap_or_else(Type::arbitrary);
        let lt_strategy = lt_strategy.unwrap_or_else(Lifetime::arbitrary);

        (type_strategy, Qualifier::arbitrary(), lt_strategy)
            .prop_map(|(ty, qualifier, lifetime)| Self {
                pointee: Box::new(ty),
                lifetime,
                qualifier,
            })
            .boxed()
    }
}

impl Arbitrary for SymbolID {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            proptest::num::usize::ANY.prop_map(|x| Self::Struct(ID::new(x))),
            proptest::num::usize::ANY.prop_map(|x| Self::Enum(ID::new(x))),
        ]
        .boxed()
    }
}

impl Arbitrary for Array<Default> {
    type Parameters = (
        Option<BoxedStrategy<Type<Default>>>,
        Option<BoxedStrategy<Constant<Default>>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(
        (ty_strat, const_strat): Self::Parameters,
    ) -> Self::Strategy {
        let ty_strat = ty_strat.unwrap_or_else(Type::arbitrary);
        let const_strat = const_strat.unwrap_or_else(Constant::arbitrary);

        (ty_strat, const_strat)
            .prop_map(|(ty, length)| Self { r#type: Box::new(ty), length })
            .boxed()
    }
}

impl Arbitrary for Type<Default> {
    type Parameters = (
        Option<BoxedStrategy<Lifetime<Default>>>,
        Option<BoxedStrategy<Constant<Default>>>,
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
            let const_strat = param.1.clone().unwrap_or_else(Constant::arbitrary);

            prop_oneof![
                6 => MemberSymbol::arbitrary_with((
                    Some(lt_strat.clone()),
                    Some(inner.clone()),
                    Some(const_strat.clone())
                ))
                .prop_map(Self::TraitMember),
                6 => Symbol::arbitrary_with((Some(lt_strat.clone()), Some(inner.clone()), Some(const_strat.clone())))
                    .prop_map(Self::Symbol),
                1 => Pointer::arbitrary_with(Some(inner.clone())).prop_map(Self::Pointer),
                2 => Reference::arbitrary_with((Some(lt_strat), Some(inner.clone()))).prop_map(Self::Reference),
                2 => Tuple::arbitrary_with(Some(inner.clone())).prop_map(Self::Tuple),
                1 => inner.clone().prop_map(|x| Self::Local(Local(Box::new(x)))),
                2 => Array::arbitrary_with((Some(inner), Some(const_strat))).prop_map(Self::Array),
            ]
        })
        .boxed()
    }
}
