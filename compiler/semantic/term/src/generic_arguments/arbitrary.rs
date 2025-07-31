#![allow(missing_docs)]

use pernixc_target::Global;
use proptest::prelude::{Arbitrary, BoxedStrategy, Strategy};

use super::GenericArguments;
use crate::{
    constant::Constant,
    generic_arguments::{MemberSymbol, Symbol, TraitMember},
    lifetime::Lifetime,
    r#type::Type,
};

impl Arbitrary for GenericArguments {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = (
        Option<BoxedStrategy<Lifetime>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<Constant>>,
    );

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let lt_strat = args.0.unwrap_or_else(Lifetime::arbitrary);
        let ty_start = args.1.unwrap_or_else(Type::arbitrary);
        let const_strat = args.2.unwrap_or_else(Constant::arbitrary);

        (
            proptest::collection::vec(lt_strat, 0..=2),
            proptest::collection::vec(ty_start, 0..=2),
            proptest::collection::vec(const_strat, 0..=2),
        )
            .prop_map(|(lifetimes, types, constants)| Self {
                lifetimes,
                types,
                constants,
            })
            .boxed()
    }
}

impl Arbitrary for MemberSymbol {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = (
        Option<BoxedStrategy<Lifetime>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<Constant>>,
    );

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            Global::arbitrary(),
            GenericArguments::arbitrary_with(args.clone()),
            GenericArguments::arbitrary_with(args),
        )
            .prop_map(|(x, y, z)| Self {
                id: x,
                member_generic_arguments: y,
                parent_generic_arguments: z,
            })
            .boxed()
    }
}

impl Arbitrary for Symbol {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = (
        Option<BoxedStrategy<Lifetime>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<Constant>>,
    );

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (Global::arbitrary(), GenericArguments::arbitrary_with(args))
            .prop_map(|(x, y)| Self { id: x, generic_arguments: y })
            .boxed()
    }
}

impl Arbitrary for TraitMember {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = (
        Option<BoxedStrategy<Lifetime>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<Constant>>,
    );

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        MemberSymbol::arbitrary_with(args).prop_map(Self).boxed()
    }
}
