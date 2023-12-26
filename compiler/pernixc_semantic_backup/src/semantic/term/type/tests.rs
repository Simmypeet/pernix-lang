//! Contains code for generating the term for testing purposes.

use proptest::{
    arbitrary::Arbitrary,
    prop_oneof,
    strategy::{BoxedStrategy, Just, Strategy},
};

use super::{Local, Pointer, Primitive, Qualifier, Reference, Type};
use crate::{
    arena::ID,
    semantic::{
        model::Model,
        term::{constant::Constant, lifetime::Lifetime, Never},
    },
    symbol::{GenericID, TypeParameterID},
};
impl Arbitrary for TypeParameterID {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (GenericID::arbitrary(), ID::arbitrary())
            .prop_map(|(parent, id)| Self { parent, id })
            .boxed()
    }
}

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

impl<
        S: Model<
            TypeInference = Never,
            ConstantInference = Never,
            LifetimeInference = Never,
            ScopedLifetime = Never,
        >,
    > Arbitrary for Pointer<S>
{
    type Parameters = Option<BoxedStrategy<Type<S>>>;
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

impl<
        S: Model<
            TypeInference = Never,
            ConstantInference = Never,
            LifetimeInference = Never,
            ScopedLifetime = Never,
        >,
    > Arbitrary for Reference<S>
{
    type Parameters = Option<BoxedStrategy<Type<S>>>;
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

impl<
        S: Model<
            TypeInference = Never,
            ConstantInference = Never,
            LifetimeInference = Never,
            ScopedLifetime = Never,
        >,
    > Arbitrary for Local<S>
{
    type Parameters = Option<BoxedStrategy<Type<S>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(type_strategy: Self::Parameters) -> Self::Strategy {
        type_strategy
            .unwrap_or_else(Type::arbitrary)
            .prop_map(|x| Self(Box::new(x)))
            .boxed()
    }
}

impl<
        S: Model<
            TypeInference = Never,
            ConstantInference = Never,
            LifetimeInference = Never,
            ScopedLifetime = Never,
        >,
    > Arbitrary for Type<S>
{
    type Parameters = Option<BoxedStrategy<Constant<S>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        let leaf = prop_oneof![
            Primitive::arbitrary().prop_map(Self::Primitive),
            TypeParameterID::arbitrary().prop_map(Self::Parameter)
        ];

        leaf.prop_recursive(4, 36, 9, |inner| {
            prop_oneof![
                Pointer::arbitrary_with(Some(inner.clone())).prop_map(Self::Pointer),
                Local::arbitrary_with(Some(inner.clone())).prop_map(Self::Local),
                Reference::arbitrary_with(Some(inner)).prop_map(Self::Reference),
            ]
        })
        .boxed()
    }
}
