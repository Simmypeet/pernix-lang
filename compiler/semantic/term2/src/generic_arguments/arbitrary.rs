#![allow(missing_docs)]

use pernixc_target::Global;
use proptest::prelude::{Arbitrary, BoxedStrategy, Strategy};
use qbice::storage::intern::Interned;

use super::{AssociatedSymbol, GenericArguments, Symbol};
use crate::{
    constant::Constant, instance::Instance, lifetime::Lifetime, r#type::Type,
};

impl Arbitrary for GenericArguments {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = (
        Option<BoxedStrategy<Lifetime>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<Constant>>,
        Option<BoxedStrategy<Instance>>,
    );

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let lifetime_strategy = args.0.unwrap_or_else(Lifetime::arbitrary);
        let type_strategy = args.1.unwrap_or_else(Type::arbitrary);
        let constant_strategy = args.2.unwrap_or_else(Constant::arbitrary);
        let instance_strategy = args.3.unwrap_or_else(Instance::arbitrary);

        (
            proptest::collection::vec(lifetime_strategy, 0..=2),
            proptest::collection::vec(type_strategy, 0..=2),
            proptest::collection::vec(constant_strategy, 0..=2),
            proptest::collection::vec(instance_strategy, 0..=2),
        )
            .prop_map(|(lifetimes, types, constants, instances)| {
                Self::new(
                    lifetimes
                        .into_iter()
                        .map(Interned::new_duplicating)
                        .collect(),
                    types.into_iter().map(Interned::new_duplicating).collect(),
                    constants
                        .into_iter()
                        .map(Interned::new_duplicating)
                        .collect(),
                    instances
                        .into_iter()
                        .map(Interned::new_duplicating)
                        .collect(),
                )
            })
            .boxed()
    }
}

impl Arbitrary for AssociatedSymbol {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = (
        Option<BoxedStrategy<Lifetime>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<Constant>>,
        Option<BoxedStrategy<Instance>>,
    );

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            Global::arbitrary(),
            GenericArguments::arbitrary_with(args.clone()),
            GenericArguments::arbitrary_with(args),
        )
            .prop_map(
                |(id, parent_generic_arguments, member_generic_arguments)| {
                    Self::new(
                        id,
                        Interned::new_duplicating(parent_generic_arguments),
                        Interned::new_duplicating(member_generic_arguments),
                    )
                },
            )
            .boxed()
    }
}

impl Arbitrary for Symbol {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = (
        Option<BoxedStrategy<Lifetime>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<Constant>>,
        Option<BoxedStrategy<Instance>>,
    );

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (Global::arbitrary(), GenericArguments::arbitrary_with(args))
            .prop_map(|(id, generic_arguments)| {
                Self::new(id, Interned::new_duplicating(generic_arguments))
            })
            .boxed()
    }
}
