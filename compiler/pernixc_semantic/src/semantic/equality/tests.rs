use std::fmt::Debug;

use proptest::{
    arbitrary::Arbitrary,
    prop_assert, prop_oneof, proptest,
    strategy::{BoxedStrategy, Strategy},
    test_runner::TestCaseError,
};

use super::equals;
use crate::{
    arena::ID,
    semantic::{
        self,
        mapping::{self, Map},
        session,
        term::{
            constant::Constant,
            lifetime::Lifetime,
            r#type::{Primitive, SymbolKindID, Type},
            GenericArguments, Symbol, Term,
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
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let leaf = Identity::arbitrary().prop_map(|x| Box::new(x) as _);

        leaf.prop_recursive(128, 256, 2, |inner| {
            prop_oneof![Mapping::arbitrary_with(Some(inner)).prop_map(|x| Box::new(x) as _)]
        })
        .boxed()
    }
}

impl Arbitrary for Box<dyn Property<Lifetime>> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let leaf = Identity::arbitrary().prop_map(|x| Box::new(x) as _);

        leaf.prop_recursive(128, 256, 2, |inner| {
            prop_oneof![Mapping::arbitrary_with(Some(inner)).prop_map(|x| Box::new(x) as _)]
        })
        .boxed()
    }
}

impl Arbitrary for Box<dyn Property<Constant>> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let leaf = Identity::arbitrary().prop_map(|x| Box::new(x) as _);

        leaf.prop_recursive(128, 256, 2, |inner| {
            prop_oneof![Mapping::arbitrary_with(Some(inner)).prop_map(|x| Box::new(x) as _)]
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
{
    type Parameters = Option<BoxedStrategy<Box<dyn Property<T>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let strat = args.unwrap_or_else(Box::<dyn Property<T>>::arbitrary);

        (strat.clone(), strat)
            .prop_map(|(lhs, rhs)| Self { lhs, rhs })
            .prop_filter("filter out the trivially equal case", |x| {
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

        <T as Map>::insert(&mut premise.equalities_mapping, l_rhs, r_lhs);

        true
    }

    fn generate(&self) -> (T, T) {
        let (l_lhs, _) = self.lhs.generate();
        let (_, r_rhs) = self.rhs.generate();

        (l_lhs, r_rhs)
    }
}

proptest! {
    #[test]
    fn property_based_testing(
        property in Box::<dyn Property<Type>>::arbitrary()
    ) {
        let (term1, term2) = property.generate();
        let mut premise = Premise::default();
        let mut table = Table::<Success>::default();

        if property.requires_environment() {
            // without premise the equality should not hold
            prop_assert!(
                !equals(
                    &term1,
                    &term2,
                    &premise,
                    &table,
                    &mut semantic::Default,
                    &mut session::Default::default()
                )
            );
            prop_assert!(
                !equals(
                    &term2,
                    &term1,
                    &premise,
                    &table,
                    &mut semantic::Default,
                    &mut session::Default::default()
                )
            );

            if !property.apply(&mut table, &mut premise) {
                // failed to apply the environment
                return Err(TestCaseError::reject("failed to apply the environment"));
            }
        }

        prop_assert!(
            equals(
                &term1,
                &term2,
                &premise,
                &table,
                &mut semantic::Default,
                &mut session::Default::default()
            )
        );
        prop_assert!(
            equals(
                &term2,
                &term1,
                &premise,
                &table,
                &mut semantic::Default,
                &mut session::Default::default()
            )
        );
    }
}
