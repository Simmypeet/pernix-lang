use std::{fmt::Debug, sync::Arc};

use pernixc_base::handler;
use pernixc_table::{GlobalID, Table};
use pernixc_term::{
    constant, constant::Constant, generic_arguments::GenericArguments, r#type,
    r#type::Type, Default, Symbol,
};
use proptest::{
    arbitrary::Arbitrary,
    prop_assert, prop_oneof, proptest,
    strategy::{BoxedStrategy, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use super::Definite;
use crate::{
    environment::{Environment, Premise},
    normalizer,
    term::Term,
    AbruptError,
};

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
pub enum ApplyPropertyError {
    #[error("{0}")]
    Abrupt(#[from] AbruptError),
}

pub trait Property<T>: 'static + Debug {
    fn generate(
        &self,
        table: &mut Table,
        premise: &mut Premise<Default>,
    ) -> Result<T, ApplyPropertyError>;

    fn node_count(&self) -> usize;
}

/// The term is trivially satisfiable without the environment.
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

impl<T: Into<U> + Clone + Debug + 'static, U> Property<U>
    for TriviallySatisfiable<T>
{
    fn generate(
        &self,
        _: &mut Table,
        _: &mut Premise<Default>,
    ) -> Result<U, ApplyPropertyError> {
        Ok(self.0.clone().into())
    }

    fn node_count(&self) -> usize { 1 }
}

#[derive(Debug)]
pub struct SymbolCongruence {
    type_strategies: Vec<Box<dyn Property<Type<Default>>>>,
    constant_strategies: Vec<Box<dyn Property<Constant<Default>>>>,

    id: GlobalID,
}

impl Arbitrary for SymbolCongruence {
    type Parameters = (
        Option<BoxedStrategy<Box<dyn Property<Type<Default>>>>>,
        Option<BoxedStrategy<Box<dyn Property<Constant<Default>>>>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            proptest::collection::vec(
                args.0.unwrap_or_else(Box::<dyn Property<Type<_>>>::arbitrary),
                0..=2,
            ),
            proptest::collection::vec(
                args.1.unwrap_or_else(
                    Box::<dyn Property<Constant<_>>>::arbitrary,
                ),
                0..=2,
            ),
            GlobalID::arbitrary(),
        )
            .prop_map(|(type_strategies, constant_strategies, id)| Self {
                type_strategies,
                constant_strategies,
                id,
            })
            .boxed()
    }
}

impl<T: Term + 'static> Property<T> for SymbolCongruence
where
    Symbol<Default>: Into<T>,
{
    fn generate(
        &self,
        table: &mut Table,
        premise: &mut Premise<Default>,
    ) -> Result<T, ApplyPropertyError> {
        let mut generic_arguments = GenericArguments::default();

        for type_strategy in &self.type_strategies {
            generic_arguments
                .types
                .push(type_strategy.generate(table, premise)?);
        }

        for constant_strategy in &self.constant_strategies {
            generic_arguments
                .constants
                .push(constant_strategy.generate(table, premise)?);
        }

        Ok(Symbol { id: self.id, generic_arguments }.into())
    }

    fn node_count(&self) -> usize {
        1 + self.type_strategies.iter().map(|x| x.node_count()).sum::<usize>()
            + self
                .constant_strategies
                .iter()
                .map(|x| x.node_count())
                .sum::<usize>()
    }
}

impl Arbitrary for Box<dyn Property<Type<Default>>> {
    type Parameters =
        Option<BoxedStrategy<Box<dyn Property<Constant<Default>>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let leaf =
            prop_oneof![TriviallySatisfiable::<r#type::Primitive>::arbitrary()
                .prop_map(|x| Box::new(x) as _),];

        leaf.prop_recursive(16, 32, 2, move |inner| {
            let const_strat = args
                .clone()
                .unwrap_or_else(Box::<dyn Property<Constant<_>>>::arbitrary);

            prop_oneof![
                4 => SymbolCongruence::arbitrary_with((
                    Some(inner.clone()),
                    Some(const_strat.clone())
                )).prop_map(|x| Box::new(x) as _),
            ]
        })
        .boxed()
    }
}

impl Arbitrary for Box<dyn Property<Constant<Default>>> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![TriviallySatisfiable::<constant::Primitive>::arbitrary()
            .prop_map(|x| Box::new(x) as _),]
        .boxed()
    }
}

fn property_based_testing<T: Term<Model = Default> + 'static>(
    property: &dyn Property<T>,
) -> TestCaseResult {
    let mut premise = Premise::default();
    let mut table = Table::new(Arc::new(handler::Panic));

    let term = property.generate(&mut table, &mut premise)?;

    let environment = Environment::new(&premise, &table, normalizer::NO_OP);

    let result = environment
        .query(&Definite(term))
        .map_err(|x| TestCaseError::reject(format!("{x}")))?;

    prop_assert!(result.is_some());

    Ok(())
}

proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 100_000,
        cases: 8192,
        ..std::default::Default::default()
    })]

    #[test]
    fn property_based_testing_constant(
       property in Box::<dyn Property<Constant<Default>>>::arbitrary(),
    ) {
        property_based_testing::<Constant<_>>(&*property)?;
    }

    #[test]
    fn property_based_testing_type(
        property in Box::<dyn Property<Type<Default>>>::arbitrary(),
    ) {
        property_based_testing::<Type<_>>(&*property)?;
    }
}
