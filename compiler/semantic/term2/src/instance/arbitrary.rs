#![allow(missing_docs)]

use pernixc_target::Global;
use proptest::{
    arbitrary::Arbitrary,
    prop_oneof,
    strategy::{BoxedStrategy, Just, Strategy},
};
use qbice::storage::intern::Interned;

use super::{AnoymousTrait, Instance, InstanceAssociated, TraitRef};
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
            .prop_map(Self::from_symbol)
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
        let instance_strategy =
            args.3.clone().unwrap_or_else(Instance::arbitrary);

        (
            instance_strategy,
            Global::arbitrary(),
            GenericArguments::arbitrary_with((args.0, args.1, args.2, args.3)),
        )
            .prop_map(
                |(
                    instance,
                    trait_associated_symbol_id,
                    trait_associated_symbol_generic_arguments,
                )| {
                    Self::new(
                        Interned::new_duplicating(instance),
                        trait_associated_symbol_id,
                        Interned::new_duplicating(
                            trait_associated_symbol_generic_arguments,
                        ),
                    )
                },
            )
            .boxed()
    }
}

impl Arbitrary for AnoymousTrait {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = ();

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Global::arbitrary().prop_map(Self::new).boxed()
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
            2 => InstanceParameterID::arbitrary().prop_map(Self::Parameter),
            1 => AnoymousTrait::arbitrary().prop_map(Self::AnonymousTrait),
            1 => Just(Self::Error(crate::error::Error)),
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
                    Some(inner.clone()),
                ))
                .prop_map(Self::Symbol),
                1 => InstanceAssociated::arbitrary_with((
                    Some(lifetime_strategy),
                    Some(type_strategy),
                    Some(constant_strategy),
                    Some(inner),
                ))
                .prop_map(Self::InstanceAssociated),
            ]
        })
        .boxed()
    }
}
