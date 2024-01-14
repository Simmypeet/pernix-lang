use std::fmt::Debug;

use proptest::{
    arbitrary::Arbitrary,
    prop_oneof,
    strategy::{BoxedStrategy, Strategy},
};

use super::{
    constant::Constant, lifetime::Lifetime, r#type::Type, GenericArguments, Local, MemberSymbol,
    Symbol, Tuple,
};
use crate::{
    arena::ID,
    semantic::term::TupleElement,
    symbol::{GenericID, MemberID},
};

impl<T: 'static> Arbitrary for ID<T> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::num::usize::ANY.prop_map(Self::new).boxed()
    }
}

impl<ChildID: Arbitrary, ParentID: Arbitrary> Arbitrary for MemberID<ChildID, ParentID>
where
    ChildID::Strategy: 'static,
    ParentID::Strategy: 'static,
{
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (ChildID::arbitrary(), ParentID::arbitrary())
            .prop_map(|(id, parent)| Self { parent, id })
            .boxed()
    }
}

impl Arbitrary for GenericID {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            ID::arbitrary().prop_map(Self::Struct),
            ID::arbitrary().prop_map(Self::Trait),
            ID::arbitrary().prop_map(Self::Enum),
            ID::arbitrary().prop_map(Self::Type),
            ID::arbitrary().prop_map(Self::Function),
            ID::arbitrary().prop_map(Self::Constant),
            ID::arbitrary().prop_map(Self::TraitType),
            ID::arbitrary().prop_map(Self::TraitFunction),
            ID::arbitrary().prop_map(Self::TraitConstant),
            ID::arbitrary().prop_map(Self::NegativeTraitImplementation),
            ID::arbitrary().prop_map(Self::TraitImplementation),
            ID::arbitrary().prop_map(Self::TraitImplementationFunction),
            ID::arbitrary().prop_map(Self::TraitImplementationType),
            ID::arbitrary().prop_map(Self::TraitImplementationConstant),
            ID::arbitrary().prop_map(Self::AdtImplementation),
            ID::arbitrary().prop_map(Self::AdtImplementationFunction),
            ID::arbitrary().prop_map(Self::AdtImplementationType),
            ID::arbitrary().prop_map(Self::AdtImplementationConstant),
        ]
        .boxed()
    }
}

impl<ID: Arbitrary> Arbitrary for Symbol<ID>
where
    ID::Strategy: 'static,
{
    type Parameters = (
        Option<BoxedStrategy<Lifetime>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<Constant>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((lt_strat, ty_start, const_strat): Self::Parameters) -> Self::Strategy {
        let lt_strat = lt_strat.unwrap_or_else(Lifetime::arbitrary);
        let ty_start = ty_start.unwrap_or_else(Type::arbitrary);
        let const_strat = const_strat.unwrap_or_else(Constant::arbitrary);

        (
            ID::arbitrary(),
            proptest::collection::vec(lt_strat, 0..=2),
            proptest::collection::vec(ty_start, 0..=2),
            proptest::collection::vec(const_strat, 0..=2),
        )
            .prop_map(|(id, lifetimes, types, constants)| Self {
                id,
                generic_arguments: GenericArguments {
                    lifetimes,
                    types,
                    constants,
                },
            })
            .boxed()
    }
}

impl<T: Arbitrary<Strategy = BoxedStrategy<T>> + 'static> Arbitrary for Local<T>
where
    Self: Into<T>,
{
    type Parameters = Option<BoxedStrategy<T>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(strat: Self::Parameters) -> Self::Strategy {
        let strat = strat.unwrap_or_else(T::arbitrary);

        (strat).prop_map(|x| Self(Box::new(x))).boxed()
    }
}

impl<ID: Debug + Arbitrary> Arbitrary for MemberSymbol<ID>
where
    ID::Strategy: 'static,
{
    type Parameters = (
        Option<BoxedStrategy<Lifetime>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<Constant>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let lt_strat = args.0.unwrap_or_else(Lifetime::arbitrary);
        let ty_start = args.1.unwrap_or_else(Type::arbitrary);
        let const_strat = args.2.unwrap_or_else(Constant::arbitrary);
        (
            ID::arbitrary(),
            proptest::collection::vec(lt_strat.clone(), 0..=1),
            proptest::collection::vec(ty_start.clone(), 0..=1),
            proptest::collection::vec(const_strat.clone(), 0..=1),
            proptest::collection::vec(lt_strat, 0..=1),
            proptest::collection::vec(ty_start, 0..=1),
            proptest::collection::vec(const_strat, 0..=1),
        )
            .prop_map(move |(id, lts, tys, consts, lts1, tys1, consts1)| Self {
                id,
                member_generic_arguments: GenericArguments {
                    lifetimes: lts,
                    types: tys,
                    constants: consts,
                },
                parent_generic_arguments: GenericArguments {
                    lifetimes: lts1,
                    types: tys1,
                    constants: consts1,
                },
            })
            .boxed()
    }
}

impl<T: Arbitrary<Strategy = BoxedStrategy<T>> + Debug + Clone + 'static> Arbitrary for Tuple<T>
where
    Self: TryFrom<T, Error = T> + Into<T>,
{
    type Parameters = Option<BoxedStrategy<T>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let strat = args.unwrap_or_else(T::arbitrary);

        proptest::collection::vec(
            prop_oneof![
                strat.clone().prop_map(|x| TupleElement::Regular(x)),
                strat.prop_map(|x| TupleElement::Unpacked(x))
            ],
            0..=2,
        )
        .prop_map(|elements| Self { elements })
        .boxed()
    }
}
