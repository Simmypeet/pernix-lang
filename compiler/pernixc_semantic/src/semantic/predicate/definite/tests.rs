use std::fmt::Debug;

use proptest::{
    arbitrary::Arbitrary,
    prop_assert, prop_oneof, proptest,
    strategy::{BoxedStrategy, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use crate::{
    arena::ID,
    semantic::{
        self,
        mapping::{self, Map},
        predicate::definite,
        session::{self, Session},
        term::{
            constant::{self, Constant},
            r#type::{self, SymbolKindID, Type},
            GenericArguments, Symbol, Term,
        },
        Premise, Semantic,
    },
    symbol::{self, ConstantParameterID, TypeParameterID},
    table::{Success, Table},
};

/// A trait for generating term for checking definite predicate satisfiability.
pub trait Property<T>: 'static + Debug {
    /// Applies this property to the environment.
    ///
    /// # Returns
    ///
    /// Returns `false` if failed to apply the environment to the
    #[must_use]
    fn apply(&self, table: &mut Table<Success>, premise: &mut Premise) -> bool;

    /// Generate a term for checking
    #[must_use]
    fn generate(&self) -> T;
}

/// The term is trivially satisifiable without the environment.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TriviallySatisfiable<T>(pub T);

impl<T: Arbitrary + 'static> Arbitrary for TriviallySatisfiable<T>
where
    T::Strategy: 'static,
{
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        T::arbitrary().prop_map(TriviallySatisfiable).boxed()
    }
}

impl<T: Into<U> + Clone + Debug + 'static, U> Property<U> for TriviallySatisfiable<T> {
    fn apply(&self, _: &mut Table<Success>, _: &mut Premise) -> bool { true }

    fn generate(&self) -> U { self.0.clone().into() }
}

#[derive(Debug)]
pub struct Mapping<Param, T> {
    generic_parameter: Param,
    property: Box<dyn Property<T>>,
}

impl<Param: Debug + Arbitrary, T: Debug + 'static> Arbitrary for Mapping<Param, T>
where
    Box<dyn Property<T>>: Arbitrary<Strategy = BoxedStrategy<Box<dyn Property<T>>>>,
    Param::Strategy: 'static,
{
    type Parameters = Option<BoxedStrategy<Box<dyn Property<T>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let args = args.unwrap_or_else(Box::arbitrary);

        (Param::arbitrary(), args)
            .prop_map(|(generic_parameter, property)| Self {
                generic_parameter,
                property,
            })
            .boxed()
    }
}

impl<Param: Debug + Into<T> + Clone + 'static, T: Term + Debug + 'static> Property<T>
    for Mapping<Param, T>
where
    semantic::Default: Semantic<T>,
    session::Default: Session<T>,
{
    fn apply(&self, table: &mut Table<Success>, premise: &mut Premise) -> bool {
        if !definite(
            &self.generate(),
            premise,
            table,
            &mut semantic::Default,
            &mut session::Default::default(),
        ) && !self.property.apply(table, premise)
        {
            return false;
        }

        if !definite(
            &self.generate(),
            premise,
            table,
            &mut semantic::Default,
            &mut session::Default::default(),
        ) {
            premise
                .equalities_mapping
                .insert(self.generate(), self.property.generate());
        }

        true
    }

    fn generate(&self) -> T { self.generic_parameter.clone().into() }
}

#[derive(Debug)]
pub struct SymbolCongruence<ID> {
    type_strategies: Vec<Box<dyn Property<Type>>>,
    constant_strategies: Vec<Box<dyn Property<Constant>>>,

    id: ID,
}

impl<ID: 'static + Arbitrary<Strategy = BoxedStrategy<ID>> + Debug + Clone> Arbitrary
    for SymbolCongruence<ID>
{
    type Parameters = (
        Option<BoxedStrategy<Box<dyn Property<Type>>>>,
        Option<BoxedStrategy<Box<dyn Property<Constant>>>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            proptest::collection::vec(
                args.0.unwrap_or_else(Box::<dyn Property<Type>>::arbitrary),
                0..=2,
            ),
            proptest::collection::vec(
                args.1
                    .unwrap_or_else(Box::<dyn Property<Constant>>::arbitrary),
                0..=2,
            ),
            ID::arbitrary(),
        )
            .prop_map(|(type_strategies, constant_strategies, id)| Self {
                type_strategies,
                constant_strategies,
                id,
            })
            .boxed()
    }
}

impl<ID: Debug + 'static + Clone, T: Term + 'static> Property<T> for SymbolCongruence<ID>
where
    Symbol<ID>: Into<T>,
{
    fn apply(&self, table: &mut Table<Success>, premise: &mut Premise) -> bool {
        for type_strategy in &self.type_strategies {
            if !type_strategy.apply(table, premise) {
                return false;
            }
        }

        for constant_strategy in &self.constant_strategies {
            if !constant_strategy.apply(table, premise) {
                return false;
            }
        }

        true
    }

    fn generate(&self) -> T {
        Symbol {
            id: self.id.clone(),
            generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: self.type_strategies.iter().map(|x| x.generate()).collect(),
                constants: self
                    .constant_strategies
                    .iter()
                    .map(|x| x.generate())
                    .collect(),
            },
        }
        .into()
    }
}

impl Arbitrary for Box<dyn Property<Type>> {
    type Parameters = Option<BoxedStrategy<Box<dyn Property<Constant>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let leaf =
            prop_oneof![TriviallySatisfiable::<r#type::Primitive>::arbitrary()
                .prop_map(|x| Box::new(x) as _),];

        leaf.prop_recursive(16, 64, 4, move |inner| {
            let const_strat = args.clone().unwrap_or_else(|| {
                Box::<dyn Property<Constant>>::arbitrary_with(Some(inner.clone()))
            });

            prop_oneof![
                1 => Mapping::<TypeParameterID, Type>::arbitrary_with(Some(inner.clone()))
                    .prop_map(|x| Box::new(x) as _),
                4 => SymbolCongruence::<SymbolKindID>::arbitrary_with((
                    Some(inner.clone()),
                    Some(const_strat.clone())
                )).prop_map(|x| Box::new(x) as _),
            ]
        })
        .boxed()
    }
}

impl Arbitrary for Box<dyn Property<Constant>> {
    type Parameters = Option<BoxedStrategy<Box<dyn Property<Type>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let leaf =
            prop_oneof![TriviallySatisfiable::<constant::Primitive>::arbitrary()
                .prop_map(|x| Box::new(x) as _),];

        leaf.prop_recursive(16, 64, 4, move |inner| {
            let ty_strat = args
                .clone()
                .unwrap_or_else(|| Box::<dyn Property<Type>>::arbitrary_with(Some(inner.clone())));

            prop_oneof![
                1 => Mapping::<ConstantParameterID, Constant>::arbitrary_with(Some(inner.clone()))
                    .prop_map(|x| Box::new(x) as _),
                4 => SymbolCongruence::<ID<symbol::Constant>>::arbitrary_with((
                    Some(ty_strat),
                    Some(inner.clone()),
                )).prop_map(|x| Box::new(x) as _)
            ]
        })
        .boxed()
    }
}

fn remove<T: Term>(equalities: &mut mapping::Mapping, term: &T) {
    let removed_terms = equalities.remove_equality(term);

    for removed_term in removed_terms.into_iter().flatten() {
        remove(equalities, &removed_term);
    }
}

fn remove_sampled<T: Term>(equalities: &mut mapping::Mapping) -> bool {
    #[allow(clippy::option_if_let_else)]
    if let Some(sampled) = <T as Map>::get(equalities).keys().next() {
        remove(equalities, &sampled.clone());
        true
    } else {
        false
    }
}

fn property_based_testing<T: Term + 'static>(property: &dyn Property<T>) -> TestCaseResult
where
    semantic::Default: Semantic<T>,
    session::Default: Session<T>,
{
    let term = property.generate();
    let mut premise = Premise::default();
    let mut table = Table::<Success>::default();

    if !property.apply(&mut table, &mut premise) {
        // failed to apply the environment
        return Err(TestCaseError::reject("failed to apply the environment"));
    }

    prop_assert!(definite(
        &term,
        &premise,
        &table,
        &mut semantic::Default,
        &mut session::Default::default()
    ));

    // remove one of the mappings and check if the term is still definite
    {
        let mut premise = premise.clone();
        if remove_sampled::<Type>(&mut premise.equalities_mapping)
            || remove_sampled::<Constant>(&mut premise.equalities_mapping)
        {
            prop_assert!(!definite(
                &term,
                &premise,
                &table,
                &mut semantic::Default,
                &mut session::Default::default()
            ));
        }
    }

    Ok(())
}

proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 100_000,
        ..Default::default()
    })]

    #[test]
    fn property_based_testing_constant(
       property in Box::<dyn Property<Constant>>::arbitrary(),
    ) {
        property_based_testing::<Constant>(&*property)?;
    }

    #[test]
    fn property_based_testing_type(
        property in Box::<dyn Property<Type>>::arbitrary(),
    ) {
        property_based_testing::<Type>(&*property)?;
    }
}
