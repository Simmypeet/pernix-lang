use std::{collections::BTreeSet, fmt::Debug, marker::PhantomData};

use proptest::{
    arbitrary::Arbitrary,
    prop_assert, prop_oneof, proptest,
    strategy::{BoxedStrategy, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use super::equals;
use crate::{
    arena::ID,
    semantic::{
        self,
        mapping::{self},
        session::{self, Session},
        term::{
            constant::Constant,
            lifetime::Lifetime,
            r#type::{Primitive, SymbolKindID, Type},
            GenericArguments, Local, Symbol, Term,
        },
        Premise, Semantic,
    },
    table::{Success, Table},
};

#[test]
fn reflexive() {
    let premise = Premise {
        equalities_mapping: mapping::Mapping::default(),
    };
    let table = Table::<Success>::default();

    let term = Type::Primitive(Primitive::Bool);

    assert!(equals(
        &term,
        &term,
        &premise,
        &table,
        &mut semantic::Default,
        &mut session::Default::default(),
    ));
}

#[test]
fn symmetric() {
    let premise = Premise {
        equalities_mapping: mapping::Mapping::from_pairs(
            std::iter::empty(),
            std::iter::once((
                Type::Primitive(Primitive::Bool),
                Type::Primitive(Primitive::Float32),
            )),
            std::iter::empty(),
        ),
    };
    let table = Table::<Success>::default();
    let lhs = Type::Primitive(Primitive::Bool);
    let rhs = Type::Primitive(Primitive::Float32);

    assert!(equals(
        &lhs,
        &rhs,
        &premise,
        &table,
        &mut semantic::Default,
        &mut session::Default::default(),
    ));
    assert!(equals(
        &rhs,
        &lhs,
        &premise,
        &table,
        &mut semantic::Default,
        &mut session::Default::default(),
    ));
}
#[test]
fn transitivity() {
    let premise = Premise {
        equalities_mapping: mapping::Mapping::from_pairs(
            std::iter::empty(),
            [
                (
                    Type::Primitive(Primitive::Bool),
                    Type::Primitive(Primitive::Float32),
                ),
                (
                    Type::Primitive(Primitive::Float32),
                    Type::Primitive(Primitive::Float64),
                ),
            ],
            std::iter::empty(),
        ),
    };
    let table = Table::<Success>::default();
    let lhs = Type::Primitive(Primitive::Bool);
    let rhs = Type::Primitive(Primitive::Float64);

    assert!(equals(
        &lhs,
        &rhs,
        &premise,
        &table,
        &mut semantic::Default,
        &mut session::Default::default(),
    ));
    assert!(equals(
        &rhs,
        &lhs,
        &premise,
        &table,
        &mut semantic::Default,
        &mut session::Default::default(),
    ));
}

#[test]
fn congruence() {
    let premise = Premise {
        equalities_mapping: mapping::Mapping::from_pairs(
            std::iter::empty(),
            [
                (
                    Type::Primitive(Primitive::Int32),
                    Type::Primitive(Primitive::Float32),
                ),
                (
                    Type::Primitive(Primitive::Int64),
                    Type::Primitive(Primitive::Float64),
                ),
            ],
            std::iter::empty(),
        ),
    };
    let table = Table::<Success>::default();
    let lhs = Type::Symbol(Symbol {
        id: SymbolKindID::Struct(ID::new(0)),
        generic_arguments: GenericArguments {
            lifetimes: Vec::new(),
            types: vec![
                Type::Primitive(Primitive::Int32),
                Type::Primitive(Primitive::Int64),
            ],
            constants: Vec::new(),
        },
    });
    let rhs = Type::Symbol(Symbol {
        id: SymbolKindID::Struct(ID::new(0)),
        generic_arguments: GenericArguments {
            lifetimes: Vec::new(),
            types: vec![
                Type::Primitive(Primitive::Float32),
                Type::Primitive(Primitive::Float64),
            ],
            constants: Vec::new(),
        },
    });

    assert!(equals(
        &lhs,
        &rhs,
        &premise,
        &table,
        &mut semantic::Default,
        &mut session::Default::default(),
    ));
    assert!(equals(
        &rhs,
        &lhs,
        &premise,
        &table,
        &mut semantic::Default,
        &mut session::Default::default(),
    ));
}

/// A trait for generating term for checking equality.
pub trait Property<T>: 'static + Debug {
    /// Determines whether this property requires [`Self::apply`] to be called in order to make the
    /// terms equal.
    fn requires_environment(&self) -> bool;

    /// Applies this property to the environment.
    ///
    /// # Returns
    ///
    /// Returns `false` if failed to apply the environment to the
    #[must_use]
    fn apply(&self, table: &mut Table<Success>, premise: &mut Premise) -> bool;

    /// Generates the term for testing.
    fn generate(&self) -> (T, T);
}

impl Arbitrary for Box<dyn Property<Type>> {
    type Parameters = (
        Option<BoxedStrategy<Box<dyn Property<Lifetime>>>>,
        Option<BoxedStrategy<Box<dyn Property<Constant>>>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(strat: Self::Parameters) -> Self::Strategy {
        let leaf = Identity::arbitrary().prop_map(|x| Box::new(x) as _);

        leaf.prop_recursive(32, 192, 6, move |inner| {
            let constant_start = strat.1.clone().unwrap_or_else(|| {
                Box::<dyn Property<Constant>>::arbitrary_with((
                    strat.0.clone(),
                    Some(inner.clone()),
                ))
            });
            let lifetime_strat = strat.0.clone().unwrap_or_else(|| {
                Box::<dyn Property<Lifetime>>::arbitrary_with((
                    Some(inner.clone()),
                    strat.1.clone(),
                ))
            });

            prop_oneof![
                Mapping::arbitrary_with(Some(inner.clone())).prop_map(|x| Box::new(x) as _),
                SymbolCongruence::arbitrary_with((
                    Some(lifetime_strat),
                    Some(inner),
                    Some(constant_start)
                ))
                .prop_map(|x| Box::new(x) as _)
            ]
        })
        .boxed()
    }
}

impl Arbitrary for Box<dyn Property<Lifetime>> {
    type Parameters = (
        Option<BoxedStrategy<Box<dyn Property<Type>>>>,
        Option<BoxedStrategy<Box<dyn Property<Constant>>>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        let leaf = Identity::arbitrary().prop_map(|x| Box::new(x) as _);

        leaf.prop_recursive(32, 192, 6, |inner| {
            prop_oneof![Mapping::arbitrary_with(Some(inner)).prop_map(|x| Box::new(x) as _)]
        })
        .boxed()
    }
}

impl Arbitrary for Box<dyn Property<Constant>> {
    type Parameters = (
        Option<BoxedStrategy<Box<dyn Property<Lifetime>>>>,
        Option<BoxedStrategy<Box<dyn Property<Type>>>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(strat: Self::Parameters) -> Self::Strategy {
        let leaf = Identity::arbitrary().prop_map(|x| Box::new(x) as _);

        leaf.prop_recursive(32, 192, 6, move |inner| {
            let type_strat = strat.1.clone().unwrap_or_else(|| {
                Box::<dyn Property<Type>>::arbitrary_with((strat.0.clone(), Some(inner.clone())))
            });
            let lifetime_strat = strat.0.clone().unwrap_or_else(|| {
                Box::<dyn Property<Lifetime>>::arbitrary_with((
                    strat.1.clone(),
                    Some(inner.clone()),
                ))
            });

            prop_oneof![
                Mapping::arbitrary_with(Some(inner.clone())).prop_map(|x| Box::new(x) as _),
                SymbolCongruence::arbitrary_with((
                    Some(lifetime_strat),
                    Some(type_strat),
                    Some(inner)
                ))
                .prop_map(|x| Box::new(x) as _)
            ]
        })
        .boxed()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identity<T> {
    term: T,
}

impl<T: Arbitrary> Arbitrary for Identity<T>
where
    T::Strategy: 'static,
{
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        T::arbitrary().prop_map(|term| Self { term }).boxed()
    }
}

impl<T: Term + Debug + 'static> Property<T> for Identity<T> {
    fn requires_environment(&self) -> bool { false }

    fn apply(&self, _: &mut Table<Success>, _: &mut Premise) -> bool { true }

    fn generate(&self) -> (T, T) { (self.term.clone(), self.term.clone()) }
}

#[derive(Debug)]
pub struct Mapping<T> {
    lhs: Box<dyn Property<T>>,
    rhs: Box<dyn Property<T>>,
}

impl<T: 'static + Debug + Term> Arbitrary for Mapping<T>
where
    Box<dyn Property<T>>: Arbitrary<Strategy = BoxedStrategy<Box<dyn Property<T>>>>,
    semantic::Default: Semantic<T>,
    session::Default: Session<T>,
{
    type Parameters = Option<BoxedStrategy<Box<dyn Property<T>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let strat = args.unwrap_or_else(Box::<dyn Property<T>>::arbitrary);

        (strat.clone(), strat)
            .prop_map(|(lhs, rhs)| Self { lhs, rhs })
            .prop_filter("filter out trivial equalities (Mapping)", |x| {
                let (lhs, rhs) = x.generate();

                !semantic::Default.trivially_equals(&lhs, &rhs)
            })
            .boxed()
    }
}

impl<T: Term + Debug + 'static> Property<T> for Mapping<T> {
    fn requires_environment(&self) -> bool { true }

    fn apply(&self, table: &mut Table<Success>, premise: &mut Premise) -> bool {
        if !self.lhs.apply(table, premise) || !self.rhs.apply(table, premise) {
            return false;
        };

        let (_, l_rhs) = self.lhs.generate();
        let (r_lhs, _) = self.rhs.generate();

        premise.equalities_mapping.insert(l_rhs, r_lhs);

        true
    }

    fn generate(&self) -> (T, T) {
        let (l_lhs, _) = self.lhs.generate();
        let (_, r_rhs) = self.rhs.generate();

        (l_lhs, r_rhs)
    }
}

/// A property that generates `local` terms which will be used to test the congruence.
#[derive(Debug)]
pub struct LocalCongruence<T> {
    strategy: Box<dyn Property<T>>,
}

impl<T: 'static + Debug + Term> Arbitrary for LocalCongruence<T>
where
    Local<T>: Into<T>,
    Box<dyn Property<T>>: Arbitrary<Strategy = BoxedStrategy<Box<dyn Property<T>>>>,
    semantic::Default: Semantic<T>,
    session::Default: Session<T>,
{
    type Parameters = Option<BoxedStrategy<Box<dyn Property<T>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let strat = args.unwrap_or_else(Box::<dyn Property<T>>::arbitrary);

        strat
            .prop_map(|strategy| Self { strategy })
            .prop_filter("filter out trivial equalities (LocalCongruence)", |x| {
                let (lhs, rhs) = x.generate();

                !semantic::Default.trivially_equals(&lhs, &rhs)
            })
            .boxed()
    }
}

impl<T: Term + Debug + 'static> Property<T> for LocalCongruence<T>
where
    Local<T>: Into<T>,
{
    fn requires_environment(&self) -> bool { self.strategy.requires_environment() }

    fn apply(&self, table: &mut Table<Success>, premise: &mut Premise) -> bool {
        self.strategy.apply(table, premise)
    }

    fn generate(&self) -> (T, T) {
        let (lhs, rhs) = self.strategy.generate();

        (Local(Box::new(lhs)).into(), Local(Box::new(rhs)).into())
    }
}

#[derive(Debug)]
pub struct SymbolCongruence<ID, T> {
    lifetime_strategies: Vec<Box<dyn Property<Lifetime>>>,
    type_strategies: Vec<Box<dyn Property<Type>>>,
    constant_strategies: Vec<Box<dyn Property<Constant>>>,

    id: ID,

    term: PhantomData<T>,
}

impl<
        ID: 'static + Arbitrary<Strategy = BoxedStrategy<ID>> + Debug + Clone,
        T: Term + From<Symbol<ID>> + 'static,
    > Arbitrary for SymbolCongruence<ID, T>
where
    semantic::Default: Semantic<T>,
    session::Default: Session<T>,
{
    type Parameters = (
        Option<BoxedStrategy<Box<dyn Property<Lifetime>>>>,
        Option<BoxedStrategy<Box<dyn Property<Type>>>>,
        Option<BoxedStrategy<Box<dyn Property<Constant>>>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            proptest::collection::vec(
                args.1.unwrap_or_else(Box::<dyn Property<Type>>::arbitrary),
                0..=2,
            ),
            proptest::collection::vec(
                args.0
                    .unwrap_or_else(Box::<dyn Property<Lifetime>>::arbitrary),
                0..=2,
            ),
            proptest::collection::vec(
                args.2
                    .unwrap_or_else(Box::<dyn Property<Constant>>::arbitrary),
                0..=2,
            ),
            ID::arbitrary(),
        )
            .prop_map(|(tys, lts, consts, id)| Self {
                lifetime_strategies: lts,
                type_strategies: tys,
                constant_strategies: consts,
                id,
                term: PhantomData,
            })
            .prop_filter("filter out trivial equalities (SymbolCongruence)", |x| {
                let (lhs, rhs) = x.generate();

                !semantic::Default.trivially_equals(&lhs, &rhs)
            })
            .boxed()
    }
}

impl<ID: Debug + 'static + Clone, T: Term + 'static> Property<T> for SymbolCongruence<ID, T>
where
    Symbol<ID>: Into<T>,
{
    fn requires_environment(&self) -> bool {
        self.lifetime_strategies
            .iter()
            .any(|x| x.requires_environment())
            || self
                .type_strategies
                .iter()
                .any(|x| x.requires_environment())
            || self
                .constant_strategies
                .iter()
                .any(|x| x.requires_environment())
    }

    fn apply(&self, table: &mut Table<Success>, premise: &mut Premise) -> bool {
        for strategy in &self.type_strategies {
            if !strategy.apply(table, premise) {
                return false;
            }
        }

        for strategy in &self.lifetime_strategies {
            if !strategy.apply(table, premise) {
                return false;
            }
        }

        for strategy in &self.constant_strategies {
            if !strategy.apply(table, premise) {
                return false;
            }
        }

        true
    }

    fn generate(&self) -> (T, T) {
        let mut lhs_generic_arguments = GenericArguments {
            lifetimes: Vec::new(),
            types: Vec::new(),
            constants: Vec::new(),
        };
        let mut rhs_generic_arguments = GenericArguments {
            lifetimes: Vec::new(),
            types: Vec::new(),
            constants: Vec::new(),
        };

        for strategy in &self.lifetime_strategies {
            let (lhs, rhs) = strategy.generate();

            lhs_generic_arguments.lifetimes.push(lhs);
            rhs_generic_arguments.lifetimes.push(rhs);
        }

        for strategy in &self.type_strategies {
            let (lhs, rhs) = strategy.generate();

            lhs_generic_arguments.types.push(lhs);
            rhs_generic_arguments.types.push(rhs);
        }

        for strategy in &self.constant_strategies {
            let (lhs, rhs) = strategy.generate();

            lhs_generic_arguments.constants.push(lhs);
            rhs_generic_arguments.constants.push(rhs);
        }

        (
            Symbol {
                id: self.id.clone(),
                generic_arguments: lhs_generic_arguments,
            }
            .into(),
            Symbol {
                id: self.id.clone(),
                generic_arguments: rhs_generic_arguments,
            }
            .into(),
        )
    }
}

#[allow(clippy::too_many_lines)]
fn property_based_checking<T: Term + 'static>(property: &dyn Property<T>) -> TestCaseResult
where
    semantic::Default: Semantic<T>,
    session::Default: Session<T>,
{
    let (term1, term2) = property.generate();
    let mut premise = Premise::default();
    let mut table = Table::<Success>::default();

    if property.requires_environment() {
        if equals(
            &term1,
            &term2,
            &premise,
            &table,
            &mut semantic::Default,
            &mut session::Default::default(),
        ) || equals(
            &term2,
            &term1,
            &premise,
            &table,
            &mut semantic::Default,
            &mut session::Default::default(),
        ) {
            return Err(TestCaseError::reject("trivial equality"));
        }

        if !property.apply(&mut table, &mut premise) {
            // failed to apply the environment
            return Err(TestCaseError::reject("failed to apply the environment"));
        }
    }

    let equality_mapping_count = premise
        .equalities_mapping
        .lifetimes()
        .values()
        .map(BTreeSet::len)
        .chain(
            premise
                .equalities_mapping
                .types()
                .values()
                .map(BTreeSet::len),
        )
        .chain(
            premise
                .equalities_mapping
                .constants()
                .values()
                .map(BTreeSet::len),
        )
        .sum::<usize>();
    println!("equality mapping count: {equality_mapping_count}");

    prop_assert!(equals(
        &term1,
        &term2,
        &premise,
        &table,
        &mut semantic::Default,
        &mut session::Default::default()
    ));
    prop_assert!(equals(
        &term2,
        &term1,
        &premise,
        &table,
        &mut semantic::Default,
        &mut session::Default::default()
    ));

    // TODO: check if the equality does not hold without the proper environment applied.

    Ok(())
}

fn inequality_checking<T: Term>(lhs: &T, rhs: &T) -> TestCaseResult
where
    semantic::Default: Semantic<T>,
    session::Default: Session<T>,
{
    let premise = Premise::default();
    let table = Table::<Success>::default();

    if semantic::Default.trivially_equals(lhs, rhs) {
        return Err(TestCaseError::reject("trivial equality"));
    }

    prop_assert!(!equals(
        lhs,
        rhs,
        &premise,
        &table,
        &mut semantic::Default,
        &mut session::Default::default()
    ));

    Ok(())
}

proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 100_000,
        max_local_rejects: u32::MAX,
        cases: 4096,
        ..Default::default()
    })] #[test]
    fn property_based_testing_constant(
        property in Box::<dyn Property<Constant>>::arbitrary()
    ) { property_based_checking(&*property)?;
    }

    #[test]
    fn property_based_testing_type(
        property in Box::<dyn Property<Type>>::arbitrary()
    ) {
        property_based_checking(&*property)?;
    }

    #[test]
    fn property_based_testing_lifetime(
        property in Box::<dyn Property<Lifetime>>::arbitrary()
    ) {
        property_based_checking(&*property)?;
    }

    #[test]
    fn inequality_testing_constant(
        lhs in Constant::arbitrary(),
        rhs in Constant::arbitrary(),
    ) {
        inequality_checking(&lhs, &rhs)?;
    }

    #[test]
    fn inequality_testing_type(
        lhs in Type::arbitrary(),
        rhs in Type::arbitrary(),
    ) {
        inequality_checking(&lhs, &rhs)?;
    }

    #[test]
    fn inequality_testing_lifetime(
        lhs in Lifetime::arbitrary(),
        rhs in Lifetime::arbitrary(),
    ) {
        inequality_checking(&lhs, &rhs)?;
    }
}
