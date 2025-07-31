#![allow(missing_docs)]

use proptest::{
    arbitrary::Arbitrary,
    prop_oneof,
    strategy::{BoxedStrategy, Just, Strategy},
};

use super::{
    Array, FunctionSignature, Pointer, Primitive, Qualifier, Reference, Type,
};
use crate::{
    constant::Constant,
    error::Error,
    generic_arguments::{MemberSymbol, Symbol, TraitMember},
    generic_parameters::TypeParameterID,
    lifetime::Lifetime,
    tuple::Tuple,
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
        prop_oneof![Just(Self::Immutable), Just(Self::Mutable)].boxed()
    }
}

impl Arbitrary for Pointer {
    type Parameters = Option<BoxedStrategy<Type>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(type_strategy: Self::Parameters) -> Self::Strategy {
        let type_strategy = type_strategy.unwrap_or_else(Type::arbitrary);

        (type_strategy, proptest::bool::ANY)
            .prop_map(|(ty, mutable)| Self { mutable, pointee: Box::new(ty) })
            .boxed()
    }
}

impl Arbitrary for Reference {
    type Parameters =
        (Option<BoxedStrategy<Lifetime>>, Option<BoxedStrategy<Type>>);
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

impl Arbitrary for Array {
    type Parameters =
        (Option<BoxedStrategy<Type>>, Option<BoxedStrategy<Constant>>);
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

impl Arbitrary for Type {
    type Parameters =
        (Option<BoxedStrategy<Lifetime>>, Option<BoxedStrategy<Constant>>);
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(param: Self::Parameters) -> Self::Strategy {
        let leaf = prop_oneof![
            Primitive::arbitrary().prop_map(Self::Primitive),
            TypeParameterID::arbitrary().prop_map(Self::Parameter),
            Just(Self::Error(Error)),
        ];

        leaf.prop_recursive(8, 48, 6, move |inner| {
            let lt_strat =
                param.0.clone().unwrap_or_else(Lifetime::arbitrary);
            let const_strat = param.1.clone().unwrap_or_else(Constant::arbitrary);

            prop_oneof![
                6 => MemberSymbol::arbitrary_with((
                    Some(lt_strat.clone()),
                    Some(inner.clone()),
                    Some(const_strat.clone())
                ))
                .prop_map(|x| Self::TraitMember(TraitMember(x))),
                6 => MemberSymbol::arbitrary_with((
                    Some(lt_strat.clone()),
                    Some(inner.clone()),
                    Some(const_strat.clone())
                ))
                .prop_map(Self::MemberSymbol),
                6 => Symbol::arbitrary_with((Some(lt_strat.clone()), Some(inner.clone()), Some(const_strat.clone())))
                    .prop_map(Self::Symbol),
                1 => Pointer::arbitrary_with(Some(inner.clone())).prop_map(Self::Pointer),
                2 => Reference::arbitrary_with((Some(lt_strat), Some(inner.clone()))).prop_map(Self::Reference),
                2 => Tuple::arbitrary_with(Some(inner.clone())).prop_map(Self::Tuple),
                2 => Array::arbitrary_with((Some(inner.clone()), Some(const_strat))).prop_map(Self::Array),
                2 => FunctionSignature::arbitrary_with((Some(inner),)).prop_map(Self::FunctionSignature),
            ]
        })
            .boxed()
    }
}

impl Arbitrary for FunctionSignature {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = (Option<BoxedStrategy<Type>>,);

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let ty = args.0.unwrap_or_else(Type::arbitrary);

        (
            proptest::collection::vec(ty, 0..=4),
            Type::arbitrary().prop_map(Box::new),
        )
            .prop_map(|(parameters, return_type)| Self {
                parameters,
                return_type,
            })
            .boxed()
    }
}
