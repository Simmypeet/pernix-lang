#![allow(missing_docs)]

use pernixc_target::Global;
use proptest::{
    arbitrary::Arbitrary,
    prop_oneof,
    strategy::{BoxedStrategy, Strategy},
};

use super::{Instance, InstanceAssociated, TraitRef};
use crate::{
    constant::Constant,
    generic_arguments::{GenericArguments, Symbol},
    generic_parameters::InstanceParameterID,
    lifetime::Lifetime,
    r#type::Type,
};

impl Arbitrary for TraitRef {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = (
        Option<BoxedStrategy<Lifetime>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<Constant>>,
        Option<BoxedStrategy<Instance>>,
    );

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        Symbol::arbitrary_with((args.0, args.1, args.2, args.3))
            .prop_map(Self)
            .boxed()
    }
}

impl Arbitrary for InstanceAssociated {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = (
        Option<BoxedStrategy<Lifetime>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<Constant>>,
        Option<BoxedStrategy<Instance>>,
    );

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let instance_strat = args.3.clone().unwrap_or_else(Instance::arbitrary);

        (
            instance_strat,
            Global::arbitrary(),
            GenericArguments::arbitrary_with((args.0, args.1, args.2, args.3)),
        )
            .prop_map(
                |(
                    instance,
                    trait_associated_symbol_id,
                    trait_associated_symbol_generic_arguments,
                )| {
                    Self {
                        instance: Box::new(instance),
                        trait_associated_symbol_id,
                        trait_associated_symbol_generic_arguments,
                    }
                },
            )
            .boxed()
    }
}

impl Arbitrary for Instance {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = (
        Option<BoxedStrategy<Lifetime>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<Constant>>,
    );

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let leaf = prop_oneof![
            InstanceParameterID::arbitrary().prop_map(Self::Parameter),
        ];

        leaf.prop_recursive(8, 48, 6, move |inner| {
            let lifetime_strategy =
                args.0.clone().unwrap_or_else(Lifetime::arbitrary);

            let constant_strategy =
                args.2.clone().unwrap_or_else(Constant::arbitrary);

            let type_strategy = args.1.clone().unwrap_or_else(|| {
                Type::arbitrary_with((
                    args.0.clone(),
                    args.2.clone(),
                    Some(inner.clone()),
                ))
            });

            prop_oneof![
                2 => Symbol::arbitrary_with((
                    Some(lifetime_strategy.clone()),
                    Some(type_strategy.clone()),
                    Some(constant_strategy.clone()),
                    Some(inner.clone())
                ))
                    .prop_map(Self::Symbol),
                1 => InstanceAssociated::arbitrary_with((
                    Some(lifetime_strategy),
                    Some(type_strategy),
                    Some(constant_strategy),
                    Some(inner)
                ))
                    .prop_map(Self::InstanceAssociated),
            ]
        })
        .boxed()
    }
}
