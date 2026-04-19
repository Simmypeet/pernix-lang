#![allow(missing_docs)]

use proptest::{
    arbitrary::Arbitrary,
    prop_oneof,
    strategy::{BoxedStrategy, Just, Strategy},
};
use qbice::storage::intern::Interned;

use super::{
    Array, FunctionSignature, Phantom, Pointer, Primitive, Qualifier,
    Reference, Type,
};
use crate::{
    constant::Constant,
    error::Error,
    generic_arguments::{AssociatedSymbol, Symbol},
    generic_parameters::TypeParameterID,
    instance::{Instance, InstanceAssociated},
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
            Just(Self::Isize),
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

        (proptest::bool::ANY, type_strategy)
            .prop_map(|(mutable, pointee)| {
                Self::new(mutable, Interned::new_duplicating(pointee))
            })
            .boxed()
    }
}

impl Arbitrary for Reference {
    type Parameters =
        (Option<BoxedStrategy<Lifetime>>, Option<BoxedStrategy<Type>>);
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(
        (lifetime_strategy, type_strategy): Self::Parameters,
    ) -> Self::Strategy {
        let type_strategy = type_strategy.unwrap_or_else(Type::arbitrary);
        let lifetime_strategy =
            lifetime_strategy.unwrap_or_else(Lifetime::arbitrary);

        (Qualifier::arbitrary(), lifetime_strategy, type_strategy)
            .prop_map(|(qualifier, lifetime, pointee)| {
                Self::new(
                    qualifier,
                    Interned::new_duplicating(lifetime),
                    Interned::new_duplicating(pointee),
                )
            })
            .boxed()
    }
}

impl Arbitrary for Array {
    type Parameters =
        (Option<BoxedStrategy<Type>>, Option<BoxedStrategy<Constant>>);
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(
        (type_strategy, constant_strategy): Self::Parameters,
    ) -> Self::Strategy {
        let type_strategy = type_strategy.unwrap_or_else(Type::arbitrary);
        let constant_strategy =
            constant_strategy.unwrap_or_else(Constant::arbitrary);

        (constant_strategy, type_strategy)
            .prop_map(|(length, r#type)| {
                Self::new(
                    Interned::new_duplicating(length),
                    Interned::new_duplicating(r#type),
                )
            })
            .boxed()
    }
}

impl Arbitrary for Phantom {
    type Parameters = Option<BoxedStrategy<Type>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(type_strategy: Self::Parameters) -> Self::Strategy {
        type_strategy
            .unwrap_or_else(Type::arbitrary)
            .prop_map(|r#type| Self::new(Interned::new_duplicating(r#type)))
            .boxed()
    }
}

impl Arbitrary for FunctionSignature {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = (Option<BoxedStrategy<Type>>,);

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let type_strategy = args.0.unwrap_or_else(Type::arbitrary);

        (proptest::collection::vec(type_strategy.clone(), 0..=4), type_strategy)
            .prop_map(|(parameters, return_type)| {
                Self::new(
                    parameters
                        .into_iter()
                        .map(Interned::new_duplicating)
                        .collect(),
                    Interned::new_duplicating(return_type),
                )
            })
            .boxed()
    }
}

impl Arbitrary for Type {
    type Parameters = (
        Option<BoxedStrategy<Lifetime>>,
        Option<BoxedStrategy<Constant>>,
        Option<BoxedStrategy<Instance>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(param: Self::Parameters) -> Self::Strategy {
        let leaf = prop_oneof![
            Primitive::arbitrary().prop_map(Self::Primitive),
            TypeParameterID::arbitrary().prop_map(Self::Parameter),
            Just(Self::Error(Error)),
        ];

        leaf.prop_recursive(8, 48, 6, move |inner| {
            let lifetime_strategy =
                param.0.clone().unwrap_or_else(Lifetime::arbitrary);
            let constant_strategy =
                param.1.clone().unwrap_or_else(Constant::arbitrary);
            let instance_strategy = param.2.clone().unwrap_or_else(|| {
                Instance::arbitrary_with((
                    param.0.clone(),
                    Some(inner.clone()),
                    param.1.clone(),
                ))
            });

            prop_oneof![
                6 => AssociatedSymbol::arbitrary_with((
                    Some(lifetime_strategy.clone()),
                    Some(inner.clone()),
                    Some(constant_strategy.clone()),
                    Some(instance_strategy.clone()),
                ))
                .prop_map(Self::AssociatedSymbol),
                6 => Symbol::arbitrary_with((
                    Some(lifetime_strategy.clone()),
                    Some(inner.clone()),
                    Some(constant_strategy.clone()),
                    Some(instance_strategy.clone()),
                ))
                .prop_map(Self::Symbol),
                2 => InstanceAssociated::arbitrary_with((
                    Some(lifetime_strategy.clone()),
                    Some(inner.clone()),
                    Some(constant_strategy.clone()),
                    Some(instance_strategy),
                ))
                .prop_map(Self::InstanceAssociated),
                1 => Pointer::arbitrary_with(Some(inner.clone())).prop_map(Self::Pointer),
                2 => Reference::arbitrary_with((Some(lifetime_strategy), Some(inner.clone()))).prop_map(Self::Reference),
                2 => Tuple::arbitrary_with(Some(inner.clone())).prop_map(Self::Tuple),
                2 => Array::arbitrary_with((Some(inner.clone()), Some(constant_strategy))).prop_map(Self::Array),
                1 => Phantom::arbitrary_with(Some(inner.clone())).prop_map(Self::Phantom),
                2 => FunctionSignature::arbitrary_with((Some(inner),)).prop_map(Self::FunctionSignature),
            ]
        })
        .boxed()
    }
}
