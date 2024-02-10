use std::{cmp, fmt::Debug};

use proptest::{
    arbitrary::Arbitrary,
    prop_oneof, proptest,
    strategy::{BoxedStrategy, Strategy},
    test_runner::TestCaseError,
};

use super::{Order, OrderUnifyingConfig};
use crate::{
    semantic::{
        self,
        session::{self, Limit, Session},
        term::{
            constant::{self, Constant},
            lifetime::Lifetime,
            r#type::{self, Type},
            GenericArguments, Symbol, Term,
        },
        tests::State,
        unification, Premise, Semantic,
    },
    symbol::{ConstantParameterID, TypeParameterID},
    table::Table,
};

/// Property checks for the order of terms.
trait Property<T>: Debug + 'static {
    /// Generates a pair of terms that will be used to compare the specificity.
    fn generate(&self) -> (T, T);

    /// Returns a positive integer if the term has *one* more specialized term
    /// than the other or returns a negative integer if the term
    /// has *one* less specialized term than the other.
    ///
    /// Otherwise, returns 0 if both lhs and rhs are equally specialized.
    ///
    /// Returns `None` if the two terms are incompatible.
    fn get_specialization_point(&self) -> Option<isize>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Matching<T>(T);

impl<T: Clone + Debug + 'static> Property<T> for Matching<T> {
    fn generate(&self) -> (T, T) { (self.0.clone(), self.0.clone()) }

    fn get_specialization_point(&self) -> Option<isize> { Some(0) }
}

impl<T: Debug + Arbitrary<Strategy = BoxedStrategy<T>> + Term + 'static>
    Arbitrary for Matching<T>
{
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        T::arbitrary()
            .prop_filter_map(
                "filter out generic parameters and trait members",
                |x| {
                    if x.as_trait_member().is_some()
                        || x.as_generic_parameter().is_some()
                    {
                        None
                    } else {
                        Some(Self(x))
                    }
                },
            )
            .boxed()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Incompatible<T> {
    lhs: T,
    rhs: T,
}

impl<T: Clone + Debug + 'static> Property<T> for Incompatible<T> {
    fn generate(&self) -> (T, T) { (self.lhs.clone(), self.rhs.clone()) }

    fn get_specialization_point(&self) -> Option<isize> { None }
}

impl<T: Debug + Arbitrary<Strategy = BoxedStrategy<T>> + Term + 'static>
    Arbitrary for Incompatible<T>
where
    semantic::Default: Semantic<T>,
    session::Default: Session<T>,
{
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (T::arbitrary(), T::arbitrary())
            .prop_filter_map(
                "filter out generic parameters, trait members, and trivially \
                 equals terms",
                |(lhs, rhs)| {
                    if unification::unify(
                        &lhs,
                        &rhs,
                        &Premise::default(),
                        &Table::<State>::default(),
                        &mut OrderUnifyingConfig,
                        &mut semantic::Default,
                        &mut Limit::new(&mut session::Default::default()),
                    )
                    .unwrap()
                    .is_some()
                    {
                        return None;
                    }

                    Some(Self { lhs, rhs })
                },
            )
            .boxed()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Matchable<TParam, TTraitMember> {
    Parameter(TParam),
    TraitMember(TTraitMember),
}

impl<
        TParam: Arbitrary<Strategy = BoxedStrategy<TParam>> + 'static,
        TTraitMember: Debug + Arbitrary<Strategy = BoxedStrategy<TTraitMember>> + 'static,
    > Arbitrary for Matchable<TParam, TTraitMember>
{
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            TParam::arbitrary().prop_map(Matchable::Parameter),
            TTraitMember::arbitrary().prop_map(Matchable::TraitMember),
        ]
        .boxed()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unified<T, TParam, TTraitMember> {
    matchable_at_lhs: bool,
    matchable: Matchable<TParam, TTraitMember>,
    non_matchable: T,
}

impl<
        T: Term + From<TParam> + From<TTraitMember> + 'static,
        TParam: Clone + Debug + 'static,
        TTraitMember: Clone + Debug + 'static,
    > Property<T> for Unified<T, TParam, TTraitMember>
{
    fn generate(&self) -> (T, T) {
        let mut result = (
            match self.matchable.clone() {
                Matchable::Parameter(t) => t.into(),
                Matchable::TraitMember(t) => t.into(),
            },
            self.non_matchable.clone(),
        );

        if !self.matchable_at_lhs {
            std::mem::swap(&mut result.0, &mut result.1);
        }

        result
    }

    fn get_specialization_point(&self) -> Option<isize> {
        if self.matchable_at_lhs {
            Some(-1)
        } else {
            Some(1)
        }
    }
}

impl<
        T: Term + Arbitrary<Strategy = BoxedStrategy<T>> + 'static,
        TParam: Debug + Arbitrary<Strategy = BoxedStrategy<TParam>> + 'static,
        TTraitMember: Debug + Arbitrary<Strategy = BoxedStrategy<TTraitMember>> + 'static,
    > Arbitrary for Unified<T, TParam, TTraitMember>
{
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            proptest::bool::ANY,
            Matchable::<TParam, TTraitMember>::arbitrary(),
            T::arbitrary(),
        )
            .prop_filter_map(
                "filter out trait member and generic parameter on non \
                 matchable",
                |(matchable_at_lhs, matchable, non_matchable)| {
                    if non_matchable.as_trait_member().is_some()
                        || non_matchable.as_generic_parameter().is_some()
                    {
                        return None;
                    }

                    Some(Self { matchable_at_lhs, matchable, non_matchable })
                },
            )
            .boxed()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Ambiguous<TParam, TTraitMember>(Matchable<TParam, TTraitMember>);

impl<
        T: Term + From<TParam> + From<TTraitMember>,
        TParam: Clone + Debug + 'static,
        TTraitMember: Clone + Debug + 'static,
    > Property<T> for Ambiguous<TParam, TTraitMember>
{
    fn generate(&self) -> (T, T) {
        let (lhs, rhs) = match self.0.clone() {
            Matchable::Parameter(t) => (t.clone().into(), t.into()),
            Matchable::TraitMember(t) => (t.clone().into(), t.into()),
        };

        (lhs, rhs)
    }

    fn get_specialization_point(&self) -> Option<isize> { Some(0) }
}

impl<
        TParam: Debug + Arbitrary<Strategy = BoxedStrategy<TParam>> + 'static,
        TTraitMember: Debug + Arbitrary<Strategy = BoxedStrategy<TTraitMember>> + 'static,
    > Arbitrary for Ambiguous<TParam, TTraitMember>
{
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Matchable::<TParam, TTraitMember>::arbitrary()
            .prop_map(Ambiguous)
            .boxed()
    }
}

impl Arbitrary for Box<dyn Property<Type>> {
    type Parameters = Option<BoxedStrategy<Box<dyn Property<Constant>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            4 => Matching::<Type>::arbitrary().prop_map(|x| Box::new(x) as _),
            1 => Incompatible::<Type>::arbitrary().prop_map(|x| Box::new(x) as _),
            4 => Unified::<Type, TypeParameterID, r#type::TraitMember>::arbitrary()
                .prop_map(|x| Box::new(x) as _),
            4 => Ambiguous::<TypeParameterID, r#type::TraitMember>::arbitrary().prop_map(|x| Box::new(x) as _),
        ].prop_recursive(3, 12, 4, move |inner| {
            let constants = args.clone().unwrap_or_else(|| Box::<dyn Property<Constant>>::arbitrary_with(Some(inner.clone())));

            Congruent::arbitrary_with((Some(inner), Some(constants))).prop_map(|x| Box::new(x) as _)
        })
        .boxed()
    }
}

impl Arbitrary for Box<dyn Property<Constant>> {
    type Parameters = Option<BoxedStrategy<Box<dyn Property<Type>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            4 => Matching::<Constant>::arbitrary().prop_map(|x| Box::new(x) as _),
            1 => Incompatible::<Constant>::arbitrary().prop_map(|x| Box::new(x) as _),
            4 => Unified::<Constant, ConstantParameterID, constant::TraitMember>::arbitrary()
                .prop_map(|x| Box::new(x) as _),
            4 => Ambiguous::<ConstantParameterID, constant::TraitMember>::arbitrary()
                .prop_map(|x| Box::new(x) as _),
        ]
        .prop_recursive(3, 12, 4, move |inner| {
            let types = args.clone().unwrap_or_else(|| Box::<dyn Property<Type>>::arbitrary_with(Some(inner.clone())));

            Congruent::arbitrary_with((Some(types), Some(inner))).prop_map(|x| Box::new(x) as _)
        })
        .boxed()
    }
}

#[derive(Debug)]
struct Congruent<ID> {
    id: ID,
    lifetimes: Vec<(Lifetime, Lifetime)>,
    types: Vec<Box<dyn Property<Type>>>,
    constants: Vec<Box<dyn Property<Constant>>>,
}

impl<T: From<Symbol<ID>> + 'static + Debug, ID: Debug + Clone + 'static>
    Property<T> for Congruent<ID>
{
    fn generate(&self) -> (T, T) {
        let mut lifetime_lhs = Vec::new();
        let mut lifetime_rhs = Vec::new();

        for (lhs, rhs) in &self.lifetimes {
            lifetime_lhs.push(*lhs);
            lifetime_rhs.push(*rhs);
        }

        let mut types_lhs = Vec::new();
        let mut types_rhs = Vec::new();

        for prop in &self.types {
            let (lhs, rhs) = prop.generate();
            types_lhs.push(lhs);
            types_rhs.push(rhs);
        }

        let mut constants_lhs = Vec::new();
        let mut constants_rhs = Vec::new();

        for prop in &self.constants {
            let (lhs, rhs) = prop.generate();
            constants_lhs.push(lhs);
            constants_rhs.push(rhs);
        }

        (
            Symbol {
                id: self.id.clone(),
                generic_arguments: GenericArguments {
                    lifetimes: lifetime_lhs,
                    types: types_lhs,
                    constants: constants_lhs,
                },
            }
            .into(),
            Symbol {
                id: self.id.clone(),
                generic_arguments: GenericArguments {
                    lifetimes: lifetime_rhs,
                    types: types_rhs,
                    constants: constants_rhs,
                },
            }
            .into(),
        )
    }

    fn get_specialization_point(&self) -> Option<isize> {
        let mut result = 0;

        for prop in &self.types {
            result += prop.get_specialization_point()?;
        }

        for prop in &self.constants {
            result += prop.get_specialization_point()?;
        }

        Some(result)
    }
}

impl<ID: Debug + Arbitrary<Strategy = BoxedStrategy<ID>> + 'static> Arbitrary
    for Congruent<ID>
{
    type Strategy = BoxedStrategy<Self>;
    type Parameters = (
        Option<BoxedStrategy<Box<dyn Property<Type>>>>,
        Option<BoxedStrategy<Box<dyn Property<Constant>>>>,
    );

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let lifetimes = Lifetime::arbitrary();
        let types = args.0.unwrap_or_else(Box::<dyn Property<Type>>::arbitrary);
        let constants =
            args.1.unwrap_or_else(Box::<dyn Property<Constant>>::arbitrary);

        (
            ID::arbitrary(),
            proptest::collection::vec((lifetimes.clone(), lifetimes), 0..=2),
            proptest::collection::vec(types, 0..=2),
            proptest::collection::vec(constants, 0..=2),
        )
            .prop_map(|(id, lifetimes, types, constants)| Self {
                id,
                lifetimes,
                types,
                constants,
            })
            .boxed()
    }
}

proptest! {
    #[test]
    fn property_based_testing(
        lifetimes in proptest::collection::vec((Lifetime::arbitrary(), Lifetime::arbitrary()), 0..=2),
        types in proptest::collection::vec(Box::<dyn Property<Type>>::arbitrary(), 0..=2),
        constants in proptest::collection::vec(Box::<dyn Property<Constant>>::arbitrary(), 0..=2),
    ) {
        let mut lifetime_lhs = Vec::new();
        let mut lifetime_rhs = Vec::new();

        for (lhs, rhs) in &lifetimes {
            lifetime_lhs.push(*lhs);
            lifetime_rhs.push(*rhs);
        }

        let mut types_lhs = Vec::new();
        let mut types_rhs = Vec::new();

        for prop in &types {
            let (lhs, rhs) = prop.generate();
            types_lhs.push(lhs);
            types_rhs.push(rhs);
        }

        let mut constants_lhs = Vec::new();
        let mut constants_rhs = Vec::new();

        for prop in &constants {
            let (lhs, rhs) = prop.generate();
            constants_lhs.push(lhs);
            constants_rhs.push(rhs);
        }

        let expected = (|| {
            let mut result = 0;

            for prop in types {
                result += prop.get_specialization_point()?;
            }

            for prop in constants {
                result += prop.get_specialization_point()?;
            }

            Some(result)
        })();

        let lhs = GenericArguments {
            lifetimes: lifetime_lhs,
            types: types_lhs,
            constants: constants_lhs,
        };

        let rhs = GenericArguments {
            lifetimes: lifetime_rhs,
            types: types_rhs,
            constants: constants_rhs,
        };

        let result = lhs.order(
            &rhs,
            &Premise::default(),
            &Table::<State>::default(),
            &mut semantic::Default,
            &mut Limit::new(&mut session::Default::default()),
        )?;

        match (result, expected) {
            (Order::Incompatible, None) => {},

            (order, Some(expected)) => match (expected.cmp(&0), order) {
                (cmp::Ordering::Equal, Order::Ambiguous)
                | (cmp::Ordering::Greater, Order::MoreSpecific)
                | (cmp::Ordering::Less, Order::MoreGeneral) => {},

                (_, order) => return Err(TestCaseError::fail(format!(
                    "Got {order:?} but expected {expected:?}",
                ))),
            },

            (result, expected) => return Err(TestCaseError::fail(format!(
                "Got {result:?} but expected {expected:?}",
            ))),
        };
    }
}
